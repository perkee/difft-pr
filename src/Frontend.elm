port module Frontend exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Duration
import Effect.Browser.Navigation as Nav
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Process
import Effect.Subscription as Subscription
import Effect.Task
import Env
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera
import Set
import Types exposing (..)
import Url
import Url.Parser exposing ((</>))
import Url.Parser.Query


port requestDiffs : Encode.Value -> Cmd msg


port diffsComputed : (Decode.Value -> msg) -> Sub msg


{-| `FrontendMsg` is one of Lamdera's "core" types (it gets serialized for
the time-travel debugger), so it can't hold a raw `Json.Decode.Value` —
decoding has to happen here, at the port boundary, rather than inside
`update`.
-}
decodeDiffsComputed : Decode.Value -> FrontendMsg
decodeDiffsComputed value =
    case Decode.decodeValue wasmResponseDecoder value of
        Ok result ->
            GotDiffsFromWasm result

        Err decodeError ->
            GotDiffsFromWasm (Err (Decode.errorToString decodeError))


type alias Model =
    FrontendModel


app =
    Effect.Lamdera.frontend
        Lamdera.sendToBackend
        app_


app_ =
    { init = init
    , onUrlRequest = UrlClicked
    , onUrlChange = UrlChanged
    , update = update
    , updateFromBackend = updateFromBackend
    , subscriptions = \_ -> Subscription.fromJs "diffsComputed" diffsComputed decodeDiffsComputed
    , view = view
    }


emptyModel : Nav.Key -> Model
emptyModel key =
    { key = key
    , page = LoginPage
    , githubUser = Nothing
    , repos = []
    , selectedRepo = Nothing
    , pullRequests = []
    , selectedPR = Nothing
    , diffs = DiffsNotStarted
    , expandedContexts = Set.empty
    , error = Nothing
    }


extractCode : Url.Url -> Maybe String
extractCode url =
    { url | path = "/" }
        |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string "code"))
        |> Maybe.andThen identity


type Route
    = HomeRoute
    | RepoRoute String String
    | PRRoute String String Int
    | NotFoundRoute


routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map HomeRoute Url.Parser.top
        , Url.Parser.map RepoRoute (Url.Parser.string </> Url.Parser.string </> Url.Parser.s "pulls")
        , Url.Parser.map PRRoute (Url.Parser.string </> Url.Parser.string </> Url.Parser.s "pull" </> Url.Parser.int)
        ]


parseRoute : Url.Url -> Route
parseRoute url =
    Url.Parser.parse routeParser url
        |> Maybe.withDefault NotFoundRoute


init : Url.Url -> Nav.Key -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
init url key =
    case extractCode url of
        Just code ->
            ( { key = key
              , page = LoadingPage
              , githubUser = Nothing
              , repos = []
              , selectedRepo = Nothing
              , pullRequests = []
              , selectedPR = Nothing
              , diffs = DiffsNotStarted
              , expandedContexts = Set.empty
              , error = Nothing
              }
            , Command.batch
                [ Nav.replaceUrl key "/"
                , authTimeoutCommand
                , sendAuthenticateWithCodeCommand code
                ]
            )

        Nothing ->
            ( emptyModel key
            , Command.none
            )


{-| The GitHub token exchange is an async round-trip through the backend
(see Backend.exchangeCodeForToken), and there's no guarantee it ever
resolves from the frontend's point of view — most notably, a `lamdera
live` recompile mid-flight orphans the in-flight request's response (it
gets routed to a ClientId that no longer exists once reconnected, and is
silently dropped). Without this, that leaves the user stuck on
LoadingPage forever with no way out but a full server restart.

Guarded on still being on LoadingPage when it fires (see `update`'s
AuthAttemptTimedOut case), so a login that actually completes in time
isn't disrupted by a late timeout.

-}
authTimeoutCommand : Command FrontendOnly ToBackend FrontendMsg
authTimeoutCommand =
    Effect.Task.perform (\() -> AuthAttemptTimedOut) (Effect.Process.sleep (Duration.seconds 20))


{-| `init` runs (and its commands fire) right when the page loads — before
the Lamdera live-connection WebSocket has necessarily finished opening.
Sending `AuthenticateWithCode` synchronously as part of `init` risks the
message going out on a socket that isn't open yet and being silently
lost, which is indistinguishable from the backend just never responding
(it's the same failure this code exists to route around: see
`authTimeoutCommand`'s doc comment). A short delay before the actual
send gives the connection time to establish first.
-}
sendAuthenticateWithCodeCommand : String -> Command FrontendOnly ToBackend FrontendMsg
sendAuthenticateWithCodeCommand code =
    Effect.Task.perform
        (\() -> SendAuthenticateWithCode code)
        (Effect.Process.sleep (Duration.milliseconds 500))


update : FrontendMsg -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case model.githubUser of
                Nothing ->
                    ( model, Command.none )

                Just _ ->
                    case parseRoute url of
                        HomeRoute ->
                            ( { model
                                | page = RepoListPage
                                , selectedRepo = Nothing
                                , pullRequests = []
                                , selectedPR = Nothing
                                , diffs = DiffsNotStarted
                                , expandedContexts = Set.empty
                                , error = Nothing
                              }
                            , if List.isEmpty model.repos then
                                Effect.Lamdera.sendToBackend FetchRepos

                              else
                                Command.none
                            )

                        RepoRoute owner repo ->
                            let
                                selectedRepo =
                                    List.filter (\r -> r.owner == owner && r.name == repo) model.repos
                                        |> List.head
                                        |> Maybe.withDefault { owner = owner, name = repo, fullName = owner ++ "/" ++ repo }
                            in
                            ( { model
                                | page = PRListPage
                                , selectedRepo = Just selectedRepo
                                , pullRequests = []
                                , selectedPR = Nothing
                                , diffs = DiffsNotStarted
                                , expandedContexts = Set.empty
                                , error = Nothing
                              }
                            , Effect.Lamdera.sendToBackend (FetchPullRequests owner repo)
                            )

                        PRRoute owner repo prNumber ->
                            let
                                selectedRepo =
                                    List.filter (\r -> r.owner == owner && r.name == repo) model.repos
                                        |> List.head
                                        |> Maybe.withDefault { owner = owner, name = repo, fullName = owner ++ "/" ++ repo }

                                selectedPR =
                                    List.filter (\pr -> pr.number == prNumber) model.pullRequests
                                        |> List.head
                            in
                            ( { model
                                | page = PRDetailPage
                                , selectedRepo = Just selectedRepo
                                , selectedPR = selectedPR
                                , diffs = FetchingPRFiles
                                , expandedContexts = Set.empty
                                , error = Nothing
                              }
                            , Effect.Lamdera.sendToBackend (FetchPRDiff owner repo prNumber)
                            )

                        NotFoundRoute ->
                            ( model, Command.none )

        LoginClicked ->
            ( model
            , Nav.load (githubOAuthUrl Env.githubClientId)
            )

        SelectRepo repo ->
            ( model
            , Nav.pushUrl model.key ("/" ++ repo.owner ++ "/" ++ repo.name ++ "/pulls")
            )

        SelectPR pr ->
            case model.selectedRepo of
                Just repo ->
                    ( model
                    , Nav.pushUrl model.key ("/" ++ repo.owner ++ "/" ++ repo.name ++ "/pull/" ++ String.fromInt pr.number)
                    )

                Nothing ->
                    ( model, Command.none )

        BackToRepos ->
            ( model
            , Nav.pushUrl model.key "/"
            )

        BackToPRs ->
            case model.selectedRepo of
                Just repo ->
                    ( model
                    , Nav.pushUrl model.key ("/" ++ repo.owner ++ "/" ++ repo.name ++ "/pulls")
                    )

                Nothing ->
                    ( model
                    , Nav.pushUrl model.key "/"
                    )

        LogoutClicked ->
            ( emptyModel model.key
            , Command.batch
                [ Effect.Lamdera.sendToBackend LogoutSession
                , Nav.pushUrl model.key "/"
                ]
            )

        ExpandContext filename gapIndex ->
            ( { model
                | expandedContexts =
                    Set.insert (filename ++ ":" ++ String.fromInt gapIndex) model.expandedContexts
              }
            , Command.none
            )

        GotDiffsFromWasm result ->
            case result of
                Ok fileDiffs ->
                    ( { model | diffs = DiffsReady fileDiffs }, Command.none )

                Err errorMessage ->
                    ( { model | diffs = DiffsFetchFailed errorMessage }, Command.none )

        CancelAuthentication ->
            ( { model | page = LoginPage, error = Nothing }, Command.none )

        AuthAttemptTimedOut ->
            case model.page of
                LoadingPage ->
                    ( { model
                        | page = LoginPage
                        , error = Just "Authentication timed out. Please try signing in again."
                      }
                    , Command.none
                    )

                _ ->
                    ( model, Command.none )

        SendAuthenticateWithCode code ->
            ( model, Effect.Lamdera.sendToBackend (AuthenticateWithCode code) )


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
updateFromBackend msg model =
    case msg of
        AuthSuccess user ->
            ( { model
                | githubUser = Just user
                , page = RepoListPage
                , error = Nothing
              }
            , Effect.Lamdera.sendToBackend FetchRepos
            )

        AuthFailure error ->
            ( { model | page = LoginPage, error = Just error }
            , Command.none
            )

        ReposLoaded repos ->
            ( { model | repos = repos }, Command.none )

        PullRequestsLoaded prs ->
            ( { model | pullRequests = prs }, Command.none )

        PRFilesLoaded files ->
            ( { model | diffs = ComputingDiffsInBrowser files }
            , Command.sendToJs "requestDiffs" requestDiffs (encodePRFiles files)
            )

        PRDiffFetchFailed error ->
            ( { model | diffs = DiffsFetchFailed error }, Command.none )

        LoggedOut ->
            ( emptyModel model.key, Nav.replaceUrl model.key "/" )


githubOAuthUrl : String -> String
githubOAuthUrl clientId =
    "https://github.com/login/oauth/authorize?client_id="
        ++ clientId
        ++ "&scope=repo"



-- WASM PORT ENCODING / DECODING


encodePRFiles : List PRFile -> Encode.Value
encodePRFiles files =
    Encode.list encodePRFile files


encodePRFile : PRFile -> Encode.Value
encodePRFile file =
    Encode.object
        [ ( "filename", Encode.string file.filename )
        , ( "status", Encode.string (githubFileStatusToString file.status) )
        , ( "lhsContent", Encode.string file.lhsContent )
        , ( "rhsContent", Encode.string file.rhsContent )
        ]


githubFileStatusToString : GithubFileStatus -> String
githubFileStatusToString status =
    case status of
        GithubFileAdded ->
            "added"

        GithubFileRemoved ->
            "removed"

        GithubFileModified ->
            "modified"

        GithubFileRenamed ->
            "renamed"

        GithubFileCopied ->
            "copied"


githubFileStatusDecoder : Decode.Decoder GithubFileStatus
githubFileStatusDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "added" ->
                        Decode.succeed GithubFileAdded

                    "removed" ->
                        Decode.succeed GithubFileRemoved

                    "modified" ->
                        Decode.succeed GithubFileModified

                    "renamed" ->
                        Decode.succeed GithubFileRenamed

                    "copied" ->
                        Decode.succeed GithubFileCopied

                    other ->
                        Decode.fail ("Unknown file status: " ++ other)
            )


{-| The JS side (elm-pkg-js/difft-wasm.js) always responds with either
`{ ok: true, results: [...] }` or `{ ok: false, error: "..." }` — this
mirrors that shape as `Ok (List FileDiff)` / `Err String`, so a
JS-side failure and an Elm-side decode failure both end up in the same
place (`DiffsFetchFailed`) without a third "something went wrong, who
knows what shape it's in" case to handle.
-}
wasmResponseDecoder : Decode.Decoder (Result String (List FileDiff))
wasmResponseDecoder =
    Decode.field "ok" Decode.bool
        |> Decode.andThen
            (\ok ->
                if ok then
                    Decode.field "results" (Decode.list fileDiffDecoder) |> Decode.map Ok

                else
                    Decode.field "error" Decode.string |> Decode.map Err
            )


fileDiffDecoder : Decode.Decoder FileDiff
fileDiffDecoder =
    Decode.map5 FileDiff
        (Decode.field "filename" Decode.string)
        (Decode.field "status" githubFileStatusDecoder)
        (Decode.field "lhsContent" Decode.string)
        (Decode.field "rhsContent" Decode.string)
        diffOutcomeDecoder


diffOutcomeDecoder : Decode.Decoder DiffOutcome
diffOutcomeDecoder =
    Decode.oneOf
        [ Decode.field "diff" difftResultDecoder |> Decode.map DiffComputed
        , Decode.field "error" Decode.string |> Decode.map DiffFailed
        ]


difftResultDecoder : Decode.Decoder DifftResult
difftResultDecoder =
    Decode.map6 DifftResult
        (Decode.field "path" Decode.string)
        (Decode.field "language" Decode.string)
        (Decode.field "status" diffStatusDecoder)
        (optionalListField "chunks" (Decode.list diffHunkDecoder))
        (optionalListField "lhs_syntax" syntaxSpanDecoder)
        (optionalListField "rhs_syntax" syntaxSpanDecoder)


syntaxSpanDecoder : Decode.Decoder SyntaxSpan
syntaxSpanDecoder =
    Decode.map4 SyntaxSpan
        (Decode.field "line" Decode.int)
        (Decode.field "start" Decode.int)
        (Decode.field "end" Decode.int)
        (Decode.field "highlight" highlightDecoder)


{-| difftastic's own JSON serializer omits `chunks` (and `aligned_lines`)
entirely when there are no chunks, rather than emitting `[]` — notably for
created/deleted files, where it expects the caller to render the whole
file itself instead of computing hunks. See `viewDiffBody`, which does
exactly that when `chunks` comes back empty.
-}
optionalListField : String -> Decode.Decoder a -> Decode.Decoder (List a)
optionalListField fieldName itemDecoder =
    Decode.oneOf
        [ Decode.field fieldName (Decode.list itemDecoder)
        , Decode.succeed []
        ]


diffStatusDecoder : Decode.Decoder DiffStatus
diffStatusDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "unchanged" ->
                        Decode.succeed StatusUnchanged

                    "changed" ->
                        Decode.succeed StatusChanged

                    "created" ->
                        Decode.succeed StatusCreated

                    "deleted" ->
                        Decode.succeed StatusDeleted

                    other ->
                        Decode.fail ("Unknown diff status: " ++ other)
            )


diffHunkDecoder : Decode.Decoder DiffHunk
diffHunkDecoder =
    Decode.map2 DiffHunk
        (Decode.maybe (Decode.field "lhs" diffSideDecoder))
        (Decode.maybe (Decode.field "rhs" diffSideDecoder))


diffSideDecoder : Decode.Decoder DiffSide
diffSideDecoder =
    Decode.map2 DiffSide
        (Decode.field "line_number" Decode.int)
        (Decode.field "changes" (Decode.list diffChangeDecoder))


diffChangeDecoder : Decode.Decoder DiffChange
diffChangeDecoder =
    Decode.map4 DiffChange
        (Decode.field "start" Decode.int)
        (Decode.field "end" Decode.int)
        (Decode.field "content" Decode.string)
        (Decode.field "highlight" highlightDecoder)


highlightDecoder : Decode.Decoder Highlight
highlightDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "delimiter" ->
                        Decode.succeed HighlightDelimiter

                    "normal" ->
                        Decode.succeed HighlightNormal

                    "string" ->
                        Decode.succeed HighlightString

                    "type" ->
                        Decode.succeed HighlightType

                    "comment" ->
                        Decode.succeed HighlightComment

                    "keyword" ->
                        Decode.succeed HighlightKeyword

                    "tree_sitter_error" ->
                        Decode.succeed HighlightTreeSitterError

                    other ->
                        Decode.fail ("Unknown highlight: " ++ other)
            )



-- WHITESPACE RENDERING


renderWithWhitespace : String -> List (Html msg)
renderWithWhitespace str =
    groupCharRuns (String.toList str) []
        |> List.map
            (\( isWs, content ) ->
                if isWs then
                    span [ Attr.class "ws" ]
                        [ text (String.map wsChar content) ]

                else
                    text content
            )


wsChar : Char -> Char
wsChar c =
    case c of
        ' ' ->
            '·'

        '\t' ->
            '⇥'

        _ ->
            c


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t'


groupCharRuns : List Char -> List ( Bool, String ) -> List ( Bool, String )
groupCharRuns chars acc =
    case chars of
        [] ->
            List.reverse acc

        first :: rest ->
            let
                ws =
                    isWhitespace first

                ( same, remaining ) =
                    spanWhile (\c -> isWhitespace c == ws) rest
            in
            groupCharRuns remaining (( ws, String.fromList (first :: same) ) :: acc)


spanWhile : (a -> Bool) -> List a -> ( List a, List a )
spanWhile pred list =
    case list of
        [] ->
            ( [], [] )

        x :: xs ->
            if pred x then
                let
                    ( matching, rest ) =
                        spanWhile pred xs
                in
                ( x :: matching, rest )

            else
                ( [], list )



-- CONTEXT LINE COMPUTATION


contextSize : Int
contextSize =
    3


type alias ChunkRange =
    { lhsMin : Int
    , lhsMax : Int
    , rhsMin : Int
    , rhsMax : Int
    }


chunkGroupRange : List DiffHunk -> ChunkRange
chunkGroupRange hunks =
    let
        lhsLines =
            List.filterMap (Maybe.map .lineNumber << .lhs) hunks

        rhsLines =
            List.filterMap (Maybe.map .lineNumber << .rhs) hunks

        fallbackMin =
            List.minimum rhsLines
                |> Maybe.withDefault (List.minimum lhsLines |> Maybe.withDefault 0)

        fallbackMax =
            List.maximum rhsLines
                |> Maybe.withDefault (List.maximum lhsLines |> Maybe.withDefault 0)
    in
    { lhsMin = List.minimum lhsLines |> Maybe.withDefault fallbackMin
    , lhsMax = List.maximum lhsLines |> Maybe.withDefault fallbackMax
    , rhsMin = List.minimum rhsLines |> Maybe.withDefault fallbackMin
    , rhsMax = List.maximum rhsLines |> Maybe.withDefault fallbackMax
    }



-- VIEW


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Difft PR"
    , body =
        [ div [ Attr.class "app" ]
            [ viewHeader model
            , div [ Attr.class "content" ]
                [ case model.error of
                    Just err ->
                        div [ Attr.class "error" ] [ text err ]

                    Nothing ->
                        text ""
                , case model.page of
                    LoginPage ->
                        viewLogin

                    LoadingPage ->
                        viewLoading

                    RepoListPage ->
                        viewRepoList model

                    PRListPage ->
                        viewPRList model

                    PRDetailPage ->
                        viewPRDetail model
                ]
            ]
        , node "style" [] [ text css ]
        ]
    }


viewHeader : Model -> Html FrontendMsg
viewHeader model =
    header [ Attr.class "header" ]
        [ h1 [] [ text "Difft PR" ]
        , case model.githubUser of
            Just user ->
                div [ Attr.class "user-info" ]
                    [ img
                        [ Attr.src user.avatarUrl
                        , Attr.alt user.login
                        , Attr.class "avatar"
                        , Attr.width 32
                        , Attr.height 32
                        ]
                        []
                    , span [] [ text user.login ]
                    , button [ onClick LogoutClicked, Attr.class "btn btn-small" ] [ text "Logout" ]
                    ]

            Nothing ->
                text ""
        ]


viewLogin : Html FrontendMsg
viewLogin =
    div [ Attr.class "login-page" ]
        [ h2 [] [ text "View PR diffs with difftastic" ]
        , p [] [ text "Sign in with your GitHub account to browse pull requests and view structural diffs powered by difftastic." ]
        , button [ onClick LoginClicked, Attr.class "btn btn-primary" ] [ text "Sign in with GitHub" ]
        ]


viewLoading : Html FrontendMsg
viewLoading =
    div [ Attr.class "loading" ]
        [ p [] [ text "Authenticating..." ]
        , button [ onClick CancelAuthentication, Attr.class "btn btn-link" ] [ text "Taking too long? Cancel and go back" ]
        ]


viewRepoList : Model -> Html FrontendMsg
viewRepoList model =
    div []
        [ h2 [] [ text "Your Repositories" ]
        , if List.isEmpty model.repos then
            p [] [ text "Loading repositories..." ]

          else
            ul [ Attr.class "repo-list" ]
                (List.map viewRepoItem model.repos)
        ]


viewRepoItem : GithubRepo -> Html FrontendMsg
viewRepoItem repo =
    li []
        [ button
            [ onClick (SelectRepo repo)
            , Attr.class "btn btn-link"
            ]
            [ text repo.fullName ]
        ]


viewPRList : Model -> Html FrontendMsg
viewPRList model =
    div []
        [ div [ Attr.class "breadcrumb" ]
            [ button [ onClick BackToRepos, Attr.class "btn btn-link" ] [ text "Repos" ]
            , span [] [ text " > " ]
            , span []
                [ text
                    (model.selectedRepo
                        |> Maybe.map .fullName
                        |> Maybe.withDefault ""
                    )
                ]
            ]
        , h2 [] [ text "Pull Requests" ]
        , if List.isEmpty model.pullRequests then
            p [] [ text "Loading pull requests..." ]

          else
            ul [ Attr.class "pr-list" ]
                (List.map viewPRItem model.pullRequests)
        ]


viewPRItem : GithubPullRequest -> Html FrontendMsg
viewPRItem pr =
    li [ Attr.class "pr-item" ]
        [ button
            [ onClick (SelectPR pr)
            , Attr.class "btn btn-link"
            ]
            [ span [ Attr.class "pr-number" ] [ text ("#" ++ String.fromInt pr.number) ]
            , span [ Attr.class "pr-title" ] [ text pr.title ]
            , span [ Attr.class "pr-meta" ]
                [ text (pr.author ++ " · " ++ pr.baseBranch ++ " ← " ++ pr.headBranch) ]
            ]
        ]


viewPRDetail : Model -> Html FrontendMsg
viewPRDetail model =
    div []
        [ div [ Attr.class "breadcrumb" ]
            [ button [ onClick BackToRepos, Attr.class "btn btn-link" ] [ text "Repos" ]
            , span [] [ text " > " ]
            , button [ onClick BackToPRs, Attr.class "btn btn-link" ]
                [ text
                    (model.selectedRepo
                        |> Maybe.map .fullName
                        |> Maybe.withDefault ""
                    )
                ]
            , span [] [ text " > " ]
            , span []
                [ text
                    (model.selectedPR
                        |> Maybe.map (\pr -> "PR #" ++ String.fromInt pr.number)
                        |> Maybe.withDefault ""
                    )
                ]
            ]
        , case model.selectedPR of
            Just pr ->
                div []
                    [ h2 [] [ text pr.title ]
                    , p [ Attr.class "pr-meta" ]
                        [ text (pr.baseBranch ++ " ← " ++ pr.headBranch ++ " by " ++ pr.author) ]
                    ]

            Nothing ->
                text ""
        , viewDiffs model.expandedContexts model.diffs
        ]


viewDiffs : Set.Set String -> DiffsState -> Html FrontendMsg
viewDiffs expandedSet diffs =
    case diffs of
        DiffsNotStarted ->
            text ""

        FetchingPRFiles ->
            p [] [ text "Fetching changed files from GitHub..." ]

        ComputingDiffsInBrowser _ ->
            p [] [ text "Computing structural diffs..." ]

        DiffsFetchFailed err ->
            div [ Attr.class "error" ] [ text err ]

        DiffsReady fileDiffs ->
            div [ Attr.class "diff-results" ]
                (List.map (viewFileDiff expandedSet) fileDiffs)


viewFileDiff : Set.Set String -> FileDiff -> Html FrontendMsg
viewFileDiff expandedSet fileDiff =
    div [ Attr.class "diff-file" ]
        [ div [ Attr.class "diff-file-header" ]
            [ span [ Attr.class "diff-filename" ] [ text fileDiff.filename ]
            , case fileDiff.outcome of
                DiffComputed result ->
                    span [ Attr.class "diff-language" ] [ text result.language ]

                DiffFailed _ ->
                    text ""
            , span
                [ Attr.class ("diff-status diff-status-" ++ githubFileStatusToString fileDiff.status) ]
                [ text (githubFileStatusLabel fileDiff.status) ]
            ]
        , case fileDiff.outcome of
            DiffComputed result ->
                viewDiffBody expandedSet fileDiff result

            DiffFailed err ->
                div [ Attr.class "error" ] [ text ("Failed to diff this file: " ++ err) ]
        ]


githubFileStatusLabel : GithubFileStatus -> String
githubFileStatusLabel status =
    case status of
        GithubFileAdded ->
            "added"

        GithubFileRemoved ->
            "removed"

        GithubFileModified ->
            "modified"

        GithubFileRenamed ->
            "renamed"

        GithubFileCopied ->
            "copied"


{-| difftastic emits no chunks at all for a created or deleted file (see
`optionalListField`'s doc comment) — there's nothing to align against on
the other side, so it leaves rendering the whole file to the caller.
Since one side has no content by construction, these render as a single
full-width column of the side that exists, rather than a two-column
layout with one column permanently empty.
-}
viewDiffBody : Set.Set String -> FileDiff -> DifftResult -> Html FrontendMsg
viewDiffBody expandedSet fileDiff result =
    if List.isEmpty result.chunks then
        case result.status of
            StatusCreated ->
                viewWholeFile SideAdded result.rhsSyntax fileDiff.rhsContent

            StatusDeleted ->
                viewWholeFile SideRemoved result.lhsSyntax fileDiff.lhsContent

            StatusChanged ->
                text ""

            StatusUnchanged ->
                text ""

    else
        let
            lhsContext =
                makeSideContext fileDiff.lhsContent result.lhsSyntax

            rhsContext =
                makeSideContext fileDiff.rhsContent result.rhsSyntax

            ranges =
                List.map chunkGroupRange result.chunks

            totalRhsLines =
                Array.length rhsContext.lines

            sections =
                buildSections fileDiff.filename totalRhsLines result.chunks ranges
        in
        div [ Attr.class "diff-chunks" ]
            (List.map (viewSection lhsContext rhsContext expandedSet) sections)


{-| Everything needed to render one side (left or right) of a file's
diff: its lines, and its syntax spans grouped by line for O(log n)
per-line lookup.
-}
type alias SideContext =
    { lines : Array String
    , syntaxByLine : Dict Int (List SyntaxSpan)
    }


makeSideContext : String -> List SyntaxSpan -> SideContext
makeSideContext content syntaxSpans =
    { lines = Array.fromList (String.split "\n" content)
    , syntaxByLine = groupSyntaxByLine syntaxSpans
    }


groupSyntaxByLine : List SyntaxSpan -> Dict Int (List SyntaxSpan)
groupSyntaxByLine spans =
    List.foldr
        (\span_ acc ->
            Dict.update span_.line
                (\existing -> Just (span_ :: Maybe.withDefault [] existing))
                acc
        )
        Dict.empty
        spans


syntaxForLine : Int -> SideContext -> List SyntaxSpan
syntaxForLine lineNumber context =
    Dict.get lineNumber context.syntaxByLine
        |> Maybe.withDefault []


{-| Render a created or deleted file as a single full-width column.
There is no "both sides" layout here on purpose — a created file has no
left side and a deleted file has no right side, so an empty opposite
column isn't representable. The faint whole-column added/removed tint
carries the diff meaning; no vivid changed-span marking, since every
line changed (same as `changesCoverLine` lines in a regular diff).
-}
viewWholeFile : ChangeSide -> List SyntaxSpan -> String -> Html msg
viewWholeFile side syntaxSpans content =
    let
        syntaxByLine =
            groupSyntaxByLine syntaxSpans

        sideClass =
            case side of
                SideAdded ->
                    "hunk-rhs"

                SideRemoved ->
                    "hunk-lhs"
    in
    div [ Attr.class "diff-chunks" ]
        [ div [ Attr.class "chunk-group" ]
            (String.split "\n" content
                |> List.indexedMap
                    (\i line ->
                        div [ Attr.class "hunk hunk-single" ]
                            [ div [ Attr.class ("hunk-side " ++ sideClass) ]
                                [ span [ Attr.class "line-number" ]
                                    [ text (String.fromInt (i + 1)) ]
                                , span [ Attr.class "line-content" ]
                                    (viewHighlightedLine side
                                        (Dict.get i syntaxByLine |> Maybe.withDefault [])
                                        []
                                        line
                                    )
                                ]
                            ]
                    )
            )
        ]


type DiffSection
    = GapSection GapInfo
    | ChunkSection (List DiffHunk)


type alias GapInfo =
    { filename : String
    , gapIndex : Int
    , rhsStart : Int
    , rhsEnd : Int
    , lhsStart : Int
    , lhsEnd : Int
    }


buildSections : String -> Int -> List (List DiffHunk) -> List ChunkRange -> List DiffSection
buildSections filename totalRhsLines chunks ranges =
    let
        go lhsPos rhsPos gapIdx remainingChunks remainingRanges acc =
            case ( remainingChunks, remainingRanges ) of
                ( chunk :: restC, range :: restR ) ->
                    let
                        gap =
                            GapSection
                                { filename = filename
                                , gapIndex = gapIdx
                                , rhsStart = rhsPos
                                , rhsEnd = range.rhsMin - 1
                                , lhsStart = lhsPos
                                , lhsEnd = range.lhsMin - 1
                                }
                    in
                    go (range.lhsMax + 1) (range.rhsMax + 1) (gapIdx + 1) restC restR (acc ++ [ gap, ChunkSection chunk ])

                _ ->
                    let
                        finalGap =
                            GapSection
                                { filename = filename
                                , gapIndex = gapIdx
                                , rhsStart = rhsPos
                                , rhsEnd = totalRhsLines - 1
                                , lhsStart = lhsPos
                                , lhsEnd = totalRhsLines - 1
                                }
                    in
                    acc ++ [ finalGap ]
    in
    go 0 0 0 chunks ranges []


viewSection : SideContext -> SideContext -> Set.Set String -> DiffSection -> Html FrontendMsg
viewSection lhsContext rhsContext expandedSet section =
    case section of
        GapSection gap ->
            viewGapSection lhsContext rhsContext expandedSet gap

        ChunkSection hunks ->
            div [ Attr.class "chunk-group" ]
                (List.map (viewHunk lhsContext rhsContext) hunks)


viewGapSection : SideContext -> SideContext -> Set.Set String -> GapInfo -> Html FrontendMsg
viewGapSection lhsContext rhsContext expandedSet gap =
    let
        rhsGapSize =
            gap.rhsEnd - gap.rhsStart + 1

        lhsGapSize =
            gap.lhsEnd - gap.lhsStart + 1

        gapSize =
            min rhsGapSize lhsGapSize

        isExpanded =
            Set.member (gap.filename ++ ":" ++ String.fromInt gap.gapIndex) expandedSet
    in
    if gapSize <= 0 then
        text ""

    else if gapSize <= 2 * contextSize || isExpanded then
        div [ Attr.class "context-lines" ]
            (List.map
                (\i -> viewContextLine lhsContext (gap.lhsStart + i) rhsContext (gap.rhsStart + i))
                (List.range 0 (gapSize - 1))
            )

    else
        let
            hiddenCount =
                gapSize - 2 * contextSize
        in
        div [ Attr.class "context-lines" ]
            (List.map
                (\i -> viewContextLine lhsContext (gap.lhsStart + i) rhsContext (gap.rhsStart + i))
                (List.range 0 (contextSize - 1))
                ++ [ viewExpandButton gap.filename gap.gapIndex hiddenCount ]
                ++ List.map
                    (\i ->
                        let
                            offset =
                                gapSize - contextSize + i
                        in
                        viewContextLine lhsContext (gap.lhsStart + offset) rhsContext (gap.rhsStart + offset)
                    )
                    (List.range 0 (contextSize - 1))
            )


viewContextLine : SideContext -> Int -> SideContext -> Int -> Html msg
viewContextLine lhsContext lhsLineNum rhsContext rhsLineNum =
    div [ Attr.class "hunk context-line" ]
        [ div [ Attr.class "hunk-side" ]
            [ span [ Attr.class "line-number" ] [ text (String.fromInt (lhsLineNum + 1)) ]
            , span [ Attr.class "line-content" ]
                (viewHighlightedLine SideRemoved
                    (syntaxForLine lhsLineNum lhsContext)
                    []
                    (lineAt lhsLineNum lhsContext.lines)
                )
            ]
        , div [ Attr.class "hunk-side" ]
            [ span [ Attr.class "line-number" ] [ text (String.fromInt (rhsLineNum + 1)) ]
            , span [ Attr.class "line-content" ]
                (viewHighlightedLine SideAdded
                    (syntaxForLine rhsLineNum rhsContext)
                    []
                    (lineAt rhsLineNum rhsContext.lines)
                )
            ]
        ]


viewExpandButton : String -> Int -> Int -> Html FrontendMsg
viewExpandButton filename gapIndex hiddenCount =
    div [ Attr.class "expand-row" ]
        [ button
            [ onClick (ExpandContext filename gapIndex)
            , Attr.class "btn btn-expand"
            ]
            [ text ("↕ Show " ++ String.fromInt hiddenCount ++ " hidden lines") ]
        ]


viewHunk : SideContext -> SideContext -> DiffHunk -> Html msg
viewHunk lhsContext rhsContext hunk =
    div [ Attr.class "hunk" ]
        [ div [ Attr.class "hunk-side hunk-lhs" ]
            (viewHunkSide SideRemoved lhsContext hunk.lhs)
        , div [ Attr.class "hunk-side hunk-rhs" ]
            (viewHunkSide SideAdded rhsContext hunk.rhs)
        ]


viewHunkSide : ChangeSide -> SideContext -> Maybe DiffSide -> List (Html msg)
viewHunkSide changeSide context maybeSide =
    case maybeSide of
        Just side ->
            [ span [ Attr.class "line-number" ]
                [ text (String.fromInt (side.lineNumber + 1)) ]
            , span [ Attr.class "line-content" ]
                (viewHighlightedLine changeSide
                    (syntaxForLine side.lineNumber context)
                    side.changes
                    (lineAt side.lineNumber context.lines)
                )
            ]

        Nothing ->
            [ span [ Attr.class "line-number" ] []
            , span [ Attr.class "line-content" ] []
            ]


lineAt : Int -> Array String -> String
lineAt lineNumber lines =
    Array.get lineNumber lines |> Maybe.withDefault ""


{-| Whether a line (or a changed span within it) belongs to the removed
(left) or added (right) side of the diff — decides which background tint
changed text gets.
-}
type ChangeSide
    = SideRemoved
    | SideAdded


changeSideClass : ChangeSide -> String
changeSideClass side =
    case side of
        SideRemoved ->
            "removed"

        SideAdded ->
            "added"


{-| One run of characters within a rendered line: its text, its syntax
classification (`Nothing` renders in the default text color), and
whether it's part of a changed span (which adds the vivid diff
background — the text color always comes from syntax, GitHub-style).
-}
type alias LineSegment =
    { text : String
    , syntax : Maybe Highlight
    , changed : Bool
    }


{-| Split a line into segments at every syntax-span and change-span
boundary, so each segment has exactly one syntax classification and one
changed/unchanged state. difftastic's `changes` only lists the spans
that differ (as `start`/`end` column offsets), and its syntax maps only
list non-Normal tokens — text covered by neither renders plain.
-}
lineSegments : List SyntaxSpan -> List DiffChange -> String -> List LineSegment
lineSegments syntaxSpans changes fullLine =
    let
        len =
            String.length fullLine

        clampCol : Int -> Int
        clampCol col =
            clamp 0 len col

        boundaries =
            (List.concatMap (\s -> [ clampCol s.start, clampCol s.end ]) syntaxSpans
                ++ List.concatMap (\c -> [ clampCol c.start, clampCol c.end ]) changes
                ++ [ 0, len ]
            )
                |> Set.fromList
                |> Set.toList

        segmentAt : Int -> Int -> LineSegment
        segmentAt from to =
            { text = String.slice from to fullLine
            , syntax =
                syntaxSpans
                    |> List.filter (\s -> s.start <= from && from < s.end)
                    |> List.head
                    |> Maybe.map .highlight
            , changed = List.any (\c -> c.start <= from && from < c.end) changes
            }
    in
    List.map2 segmentAt boundaries (List.drop 1 boundaries)
        |> List.filter (\segment -> not (String.isEmpty segment.text))


{-| Merge changed spans separated only by whitespace into one contiguous
span, so a run of changed tokens reads as a single vivid region instead
of a patchy one (difftastic reports novel tokens individually, and the
whitespace between them belongs to no token).
-}
mergeChangesAcrossWhitespace : String -> List DiffChange -> List DiffChange
mergeChangesAcrossWhitespace fullLine changes =
    let
        merge : DiffChange -> List DiffChange -> List DiffChange
        merge change merged =
            case merged of
                previous :: rest ->
                    if String.all isWhitespace (String.slice previous.end change.start fullLine) then
                        { previous | end = change.end } :: rest

                    else
                        change :: merged

                [] ->
                    [ change ]
    in
    changes
        |> List.sortBy .start
        |> List.foldl merge []
        |> List.reverse


{-| True when the changed spans cover every non-whitespace character of
the line — i.e. the whole line is new/gone, not edited within. Such
lines skip the vivid changed-span background: the faint whole-line tint
already says it, and painting everything vivid would drown the syntax
colors (GitHub does the same — only intra-line word changes get the
vivid mark).
-}
changesCoverLine : String -> List DiffChange -> Bool
changesCoverLine fullLine changes =
    String.toList fullLine
        |> List.indexedMap Tuple.pair
        |> List.all
            (\( i, char ) ->
                isWhitespace char
                    || List.any (\c -> c.start <= i && i < c.end) changes
            )


{-| Render one line of a diff: syntax highlighting as text color across
the whole line, and the vivid changed-span background behind the parts
that differ.
-}
viewHighlightedLine : ChangeSide -> List SyntaxSpan -> List DiffChange -> String -> List (Html msg)
viewHighlightedLine side syntaxSpans changes fullLine =
    let
        mergedChanges =
            mergeChangesAcrossWhitespace fullLine changes

        visibleChanges =
            if changesCoverLine fullLine mergedChanges then
                []

            else
                mergedChanges
    in
    lineSegments syntaxSpans visibleChanges fullLine
        |> List.concatMap (viewSegment side)


viewSegment : ChangeSide -> LineSegment -> List (Html msg)
viewSegment side segment =
    let
        classes =
            List.filterMap identity
                [ if segment.changed then
                    Just ("diff-change diff-change-" ++ changeSideClass side)

                  else
                    Nothing
                , Maybe.map (\highlight -> "syn-" ++ highlightClass highlight) segment.syntax
                ]
    in
    case classes of
        [] ->
            renderWithWhitespace segment.text

        _ ->
            [ span [ Attr.class (String.join " " classes) ]
                (renderWithWhitespace segment.text)
            ]


highlightClass : Highlight -> String
highlightClass highlight =
    case highlight of
        HighlightDelimiter ->
            "delimiter"

        HighlightNormal ->
            "normal"

        HighlightString ->
            "string"

        HighlightType ->
            "type"

        HighlightComment ->
            "comment"

        HighlightKeyword ->
            "keyword"

        HighlightTreeSitterError ->
            "tree-sitter-error"


css : String
css =
    """
* { box-sizing: border-box; margin: 0; padding: 0; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #0d1117; color: #c9d1d9; }
.app { max-width: 1400px; margin: 0 auto; padding: 0 16px; }
.header { display: flex; justify-content: space-between; align-items: center; padding: 16px 0; border-bottom: 1px solid #30363d; }
.header h1 { font-size: 20px; color: #58a6ff; }
.user-info { display: flex; align-items: center; gap: 8px; }
.avatar { border-radius: 50%; }
.content { padding: 24px 0; }
.error { background: #3d1f1f; border: 1px solid #f85149; color: #f85149; padding: 12px; border-radius: 6px; margin-bottom: 16px; }
.login-page { text-align: center; padding: 64px 0; }
.login-page h2 { margin-bottom: 12px; color: #f0f6fc; }
.login-page p { margin-bottom: 24px; color: #8b949e; }
.loading { text-align: center; padding: 64px 0; color: #8b949e; }
.btn { cursor: pointer; border: none; background: none; color: #c9d1d9; font-size: 14px; font-family: inherit; }
.btn-primary { background: #238636; color: #fff; padding: 10px 24px; border-radius: 6px; font-size: 16px; font-weight: 600; }
.btn-primary:hover { background: #2ea043; }
.btn-small { padding: 4px 12px; border: 1px solid #30363d; border-radius: 6px; font-size: 12px; }
.btn-small:hover { border-color: #8b949e; }
.btn-link { color: #58a6ff; text-align: left; }
.btn-link:hover { text-decoration: underline; }
.breadcrumb { display: flex; align-items: center; gap: 4px; padding: 8px 0; font-size: 14px; color: #8b949e; }
.repo-list, .pr-list { list-style: none; }
.repo-list li, .pr-list li { border-bottom: 1px solid #21262d; }
.repo-list .btn-link, .pr-list .btn-link { display: block; padding: 12px 0; width: 100%; }
.pr-item .btn-link { display: flex; gap: 8px; align-items: baseline; flex-wrap: wrap; }
.pr-number { color: #8b949e; font-weight: 600; min-width: 48px; }
.pr-title { color: #c9d1d9; flex: 1; }
.pr-meta { color: #8b949e; font-size: 12px; }
.diff-file { border: 1px solid #30363d; border-radius: 6px; margin-bottom: 16px; overflow: hidden; }
.diff-file-header { display: flex; align-items: center; gap: 8px; padding: 8px 16px; background: #161b22; border-bottom: 1px solid #30363d; font-size: 13px; }
.diff-filename { font-family: 'SF Mono', 'Fira Code', monospace; font-weight: 600; color: #c9d1d9; flex: 1; }
.diff-language { color: #8b949e; }
.diff-status { padding: 2px 8px; border-radius: 12px; font-size: 11px; font-weight: 600; }
.diff-status-modified { background: #3fb95040; color: #3fb950; }
.diff-status-added { background: #23863640; color: #3fb950; }
.diff-status-removed { background: #f8514940; color: #f85149; }
.diff-status-renamed { background: #a37ade40; color: #a37ade; }
.diff-status-copied { background: #a37ade40; color: #a37ade; }
.diff-chunks { overflow-x: auto; }
.chunk-group { padding: 0; }
.hunk { display: flex; font-family: 'SF Mono', 'Fira Code', monospace; font-size: 13px; line-height: 20px; }
.hunk-side { flex: 1; display: flex; padding: 0 8px; min-width: 0; }
.hunk-lhs { background: #1c1210; border-right: 1px solid #30363d; }
.hunk-rhs { background: #0f1a10; }
.hunk-single .hunk-lhs { border-right: none; }
.context-line .hunk-side { background: #0d1117; }
.context-line .hunk-side:first-child { border-right: 1px solid #30363d; }
.line-number { color: #484f58; min-width: 48px; text-align: right; padding-right: 12px; user-select: none; flex-shrink: 0; }
.line-content { white-space: pre; tab-size: 4; flex: 1; min-width: 0; }
.diff-change { border-radius: 2px; }
.diff-change-removed { background: #8b1a1f99; }
.diff-change-added { background: #1c6b2eb3; }
.syn-keyword { color: #ff7b72; }
.syn-string { color: #a5d6ff; }
.syn-comment { color: #8b949e; }
.syn-type { color: #ffa657; }
.syn-delimiter { color: #79c0ff; }
.syn-normal { color: #c9d1d9; }
.syn-tree-sitter-error { text-decoration: wavy underline #f85149; }
.ws { color: #3b4048; }
.expand-row { text-align: center; background: #161b22; padding: 4px 0; border-top: 1px solid #21262d; border-bottom: 1px solid #21262d; }
.btn-expand { color: #58a6ff; font-size: 12px; font-family: 'SF Mono', 'Fira Code', monospace; }
.btn-expand:hover { text-decoration: underline; }
h2 { margin-bottom: 16px; color: #f0f6fc; }
"""
