module Backend exposing (..)

import Dict
import Duration exposing (Duration)
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Http
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription
import Env
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera
import Types exposing (..)
import Url


type alias Model =
    BackendModel


app =
    Effect.Lamdera.backend
        Lamdera.broadcast
        Lamdera.sendToFrontend
        app_


app_ =
    { init = init
    , update = update
    , updateFromFrontend = updateFromFrontend
    , subscriptions = \_ -> Subscription.none
    }


init : ( Model, Command BackendOnly ToFrontend BackendMsg )
init =
    ( { sessions = Dict.empty
      , users = Dict.empty
      , pendingPRDiffs = Dict.empty
      }
    , Command.none
    )


httpErrorToString : Effect.Http.Error -> String
httpErrorToString err =
    case err of
        Effect.Http.BadUrl url ->
            "Bad URL: " ++ url

        Effect.Http.Timeout ->
            "Request timed out"

        Effect.Http.NetworkError ->
            "Network error"

        Effect.Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Effect.Http.BadBody body ->
            "Bad body: " ++ body


{-| Every outbound call to GitHub gets a bounded timeout. Without one, a
connection that hangs at the TCP/TLS level (rather than failing outright)
leaves its BackendMsg callback never firing — the frontend then has no
signal beyond its own 20s `AuthAttemptTimedOut` fallback, and every other
GitHub-backed action just stalls forever with no error to show.
-}
githubRequestTimeout : Maybe Duration
githubRequestTimeout =
    Just (Duration.seconds 10)


expectJson : (Result String a -> msg) -> Decode.Decoder a -> Effect.Http.Expect msg
expectJson toMsg decoder =
    Effect.Http.expectJson
        (\result -> toMsg (Result.mapError httpErrorToString result))
        decoder


expectString : (Result String String -> msg) -> Effect.Http.Expect msg
expectString toMsg =
    Effect.Http.expectString
        (\result -> toMsg (Result.mapError httpErrorToString result))


{-| GitHub's content API 404s for a path that doesn't exist at that ref
(e.g. the base side of a file that was added in this PR). That's a
routine, expected outcome here, not a real error — every caller that
fetches file content wants "" in that case, not a failure to report.
-}
expectRawContentOrEmpty : (Result String String -> msg) -> Effect.Http.Expect msg
expectRawContentOrEmpty toMsg =
    Effect.Http.expectStringResponse
        (\result -> toMsg result)
        (\response ->
            case response of
                Effect.Http.GoodStatus_ _ body ->
                    Ok body

                Effect.Http.BadStatus_ metadata _ ->
                    if metadata.statusCode == 404 then
                        Ok ""

                    else
                        Err ("Bad status: " ++ String.fromInt metadata.statusCode)

                Effect.Http.BadUrl_ url ->
                    Err ("Bad URL: " ++ url)

                Effect.Http.Timeout_ ->
                    Err "Request timed out"

                Effect.Http.NetworkError_ ->
                    Err "Network error"
        )


update : BackendMsg -> Model -> ( Model, Command BackendOnly ToFrontend BackendMsg )
update msg model =
    case msg of
        GotAccessToken sessionIdStr clientIdStr result ->
            let
                clientId =
                    Effect.Lamdera.clientIdFromString clientIdStr
            in
            case result of
                Ok accessToken ->
                    ( { model | sessions = Dict.insert sessionIdStr accessToken model.sessions }
                    , fetchGithubUser sessionIdStr clientIdStr accessToken
                    )

                Err error ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (AuthFailure error)
                    )

        GotGithubUser sessionIdStr clientIdStr accessToken result ->
            let
                clientId =
                    Effect.Lamdera.clientIdFromString clientIdStr
            in
            case result of
                Ok user ->
                    ( { model
                        | sessions = Dict.insert sessionIdStr accessToken model.sessions
                        , users = Dict.insert sessionIdStr user model.users
                      }
                    , Effect.Lamdera.sendToFrontend clientId (AuthSuccess user)
                    )

                Err error ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (AuthFailure error)
                    )

        GotRepos clientIdStr result ->
            let
                clientId =
                    Effect.Lamdera.clientIdFromString clientIdStr
            in
            case result of
                Ok repos ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (ReposLoaded repos)
                    )

                Err error ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (AuthFailure error)
                    )

        GotPullRequests clientIdStr result ->
            let
                clientId =
                    Effect.Lamdera.clientIdFromString clientIdStr
            in
            case result of
                Ok prs ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (PullRequestsLoaded prs)
                    )

                Err error ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (AuthFailure error)
                    )

        GotPRMeta ctx result ->
            let
                clientId =
                    Effect.Lamdera.clientIdFromString ctx.clientIdStr
            in
            case result of
                Ok meta ->
                    ( model, fetchPRFileList ctx meta )

                Err error ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (PRDiffFetchFailed error)
                    )

        GotPRFileList ctx meta result ->
            let
                clientId =
                    Effect.Lamdera.clientIdFromString ctx.clientIdStr
            in
            case result of
                Ok entries ->
                    let
                        pending =
                            { clientId = ctx.clientIdStr
                            , owner = ctx.owner
                            , repo = ctx.repo
                            , files =
                                entries
                                    |> List.map (\entry -> ( entry.filename, initPendingFile entry.status ))
                                    |> Dict.fromList
                            }
                    in
                    ( { model | pendingPRDiffs = Dict.insert ctx.requestKey pending model.pendingPRDiffs }
                    , entries
                        |> List.concatMap (fetchFileContentCommands ctx meta)
                        |> Command.batch
                    )

                Err error ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (PRDiffFetchFailed error)
                    )

        GotFileContent requestKey filename side result ->
            case Dict.get requestKey model.pendingPRDiffs of
                Nothing ->
                    -- Already finalized (or never existed) — nothing to update.
                    ( model, Command.none )

                Just pending ->
                    case result of
                        Err error ->
                            let
                                clientId =
                                    Effect.Lamdera.clientIdFromString pending.clientId
                            in
                            ( { model | pendingPRDiffs = Dict.remove requestKey model.pendingPRDiffs }
                            , Effect.Lamdera.sendToFrontend clientId (PRDiffFetchFailed error)
                            )

                        Ok content ->
                            let
                                updatedFiles =
                                    Dict.update filename (Maybe.map (setContent side content)) pending.files

                                updatedPending =
                                    { pending | files = updatedFiles }
                            in
                            if allFilesResolved updatedFiles then
                                let
                                    clientId =
                                        Effect.Lamdera.clientIdFromString pending.clientId
                                in
                                ( { model | pendingPRDiffs = Dict.remove requestKey model.pendingPRDiffs }
                                , Effect.Lamdera.sendToFrontend clientId
                                    (PRFilesLoaded (resolvedPRFiles updatedFiles))
                                )

                            else
                                ( { model | pendingPRDiffs = Dict.insert requestKey updatedPending model.pendingPRDiffs }
                                , Command.none
                                )

        NoOpBackendMsg ->
            ( model, Command.none )


initPendingFile : GithubFileStatus -> PendingFile
initPendingFile status =
    case status of
        GithubFileAdded ->
            { status = status, lhs = NotNeeded, rhs = ContentPending }

        GithubFileRemoved ->
            { status = status, lhs = ContentPending, rhs = NotNeeded }

        GithubFileModified ->
            { status = status, lhs = ContentPending, rhs = ContentPending }

        GithubFileRenamed ->
            { status = status, lhs = ContentPending, rhs = ContentPending }

        GithubFileCopied ->
            { status = status, lhs = ContentPending, rhs = ContentPending }


setContent : FileSide -> String -> PendingFile -> PendingFile
setContent side content file =
    case side of
        LhsSide ->
            { file | lhs = ContentFetched content }

        RhsSide ->
            { file | rhs = ContentFetched content }


allFilesResolved : Dict.Dict String PendingFile -> Bool
allFilesResolved files =
    files
        |> Dict.values
        |> List.all (\file -> isResolved file.lhs && isResolved file.rhs)


isResolved : RemoteContent -> Bool
isResolved content =
    case content of
        ContentPending ->
            False

        NotNeeded ->
            True

        ContentFetched _ ->
            True


resolvedContentToString : RemoteContent -> String
resolvedContentToString content =
    case content of
        ContentFetched s ->
            s

        NotNeeded ->
            ""

        ContentPending ->
            ""


resolvedPRFiles : Dict.Dict String PendingFile -> List PRFile
resolvedPRFiles files =
    files
        |> Dict.toList
        |> List.map
            (\( filename, file ) ->
                { filename = filename
                , status = file.status
                , lhsContent = resolvedContentToString file.lhs
                , rhsContent = resolvedContentToString file.rhs
                }
            )


{-| What a freshly loaded page should learn about its session: the user
it already belongs to, or an explicit `NoSession` — never silence, so
the frontend doesn't have to guess whether an answer is still coming.
-}
sessionResponse : Dict.Dict String GithubUser -> String -> ToFrontend
sessionResponse users sessionIdStr =
    case Dict.get sessionIdStr users of
        Just user ->
            AuthSuccess user

        Nothing ->
            NoSession


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Command BackendOnly ToFrontend BackendMsg )
updateFromFrontend sessionId clientId toBackend model =
    let
        sessionIdStr =
            Effect.Lamdera.sessionIdToString sessionId

        clientIdStr =
            Effect.Lamdera.clientIdToString clientId
    in
    case toBackend of
        AuthenticateWithCode code ->
            ( model
            , exchangeCodeForToken sessionIdStr clientIdStr code
            )

        CheckSession ->
            ( model
            , Effect.Lamdera.sendToFrontend clientId (sessionResponse model.users sessionIdStr)
            )

        FetchRepos ->
            case Dict.get sessionIdStr model.sessions of
                Just token ->
                    ( model, fetchRepos clientIdStr token )

                Nothing ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (AuthFailure "Not authenticated")
                    )

        FetchPullRequests owner repo ->
            case Dict.get sessionIdStr model.sessions of
                Just token ->
                    ( model, fetchPullRequests clientIdStr token owner repo )

                Nothing ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (AuthFailure "Not authenticated")
                    )

        FetchPRDiff owner repo prNumber ->
            case Dict.get sessionIdStr model.sessions of
                Just token ->
                    let
                        ctx =
                            { clientIdStr = clientIdStr
                            , owner = owner
                            , repo = repo
                            , prNumber = prNumber
                            , token = token
                            , requestKey = sessionIdStr ++ "|" ++ owner ++ "/" ++ repo ++ "#" ++ String.fromInt prNumber
                            }
                    in
                    ( model, fetchPRMeta ctx )

                Nothing ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (AuthFailure "Not authenticated")
                    )

        LogoutSession ->
            ( { model
                | sessions = Dict.remove sessionIdStr model.sessions
                , users = Dict.remove sessionIdStr model.users
              }
            , Effect.Lamdera.sendToFrontend clientId LoggedOut
            )



-- GitHub API


githubAuthHeaders : String -> String -> List Effect.Http.Header
githubAuthHeaders token accept =
    [ Effect.Http.header "Authorization" ("Bearer " ++ token)
    , Effect.Http.header "Accept" accept
    ]


exchangeCodeForToken : String -> String -> String -> Command BackendOnly ToFrontend BackendMsg
exchangeCodeForToken sessionIdStr clientIdStr code =
    Effect.Http.request
        { method = "POST"
        , headers =
            [ Effect.Http.header "Accept" "application/json" ]
        , url = "https://github.com/login/oauth/access_token"
        , body =
            Effect.Http.jsonBody
                (Encode.object
                    [ ( "client_id", Encode.string Env.githubClientId )
                    , ( "client_secret", Encode.string Env.githubClientSecret )
                    , ( "code", Encode.string code )
                    ]
                )
        , expect =
            expectJson
                (GotAccessToken sessionIdStr clientIdStr)
                (Decode.field "access_token" Decode.string)
        , timeout = githubRequestTimeout
        , tracker = Nothing
        }


fetchGithubUser : String -> String -> String -> Command BackendOnly ToFrontend BackendMsg
fetchGithubUser sessionIdStr clientIdStr token =
    Effect.Http.request
        { method = "GET"
        , headers = githubAuthHeaders token "application/json"
        , url = "https://api.github.com/user"
        , body = Effect.Http.emptyBody
        , expect =
            expectJson
                (GotGithubUser sessionIdStr clientIdStr token)
                githubUserDecoder
        , timeout = githubRequestTimeout
        , tracker = Nothing
        }


fetchRepos : String -> String -> Command BackendOnly ToFrontend BackendMsg
fetchRepos clientIdStr token =
    Effect.Http.request
        { method = "GET"
        , headers = githubAuthHeaders token "application/json"
        , url = "https://api.github.com/user/repos?sort=updated&per_page=100&visibility=all&affiliation=owner,collaborator,organization_member"
        , body = Effect.Http.emptyBody
        , expect =
            expectJson
                (GotRepos clientIdStr)
                (Decode.list githubRepoDecoder)
        , timeout = githubRequestTimeout
        , tracker = Nothing
        }


fetchPullRequests : String -> String -> String -> String -> Command BackendOnly ToFrontend BackendMsg
fetchPullRequests clientIdStr token owner repo =
    Effect.Http.request
        { method = "GET"
        , headers = githubAuthHeaders token "application/json"
        , url = "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/pulls?state=open&per_page=30"
        , body = Effect.Http.emptyBody
        , expect =
            expectJson
                (GotPullRequests clientIdStr)
                (Decode.list githubPRDecoder)
        , timeout = githubRequestTimeout
        , tracker = Nothing
        }


fetchPRMeta : PRDiffContext -> Command BackendOnly ToFrontend BackendMsg
fetchPRMeta ctx =
    Effect.Http.request
        { method = "GET"
        , headers = githubAuthHeaders ctx.token "application/json"
        , url = "https://api.github.com/repos/" ++ ctx.owner ++ "/" ++ ctx.repo ++ "/pulls/" ++ String.fromInt ctx.prNumber
        , body = Effect.Http.emptyBody
        , expect = expectJson (GotPRMeta ctx) prMetaDecoder
        , timeout = githubRequestTimeout
        , tracker = Nothing
        }


fetchPRFileList : PRDiffContext -> PRMeta -> Command BackendOnly ToFrontend BackendMsg
fetchPRFileList ctx meta =
    Effect.Http.request
        { method = "GET"
        , headers = githubAuthHeaders ctx.token "application/json"
        , url =
            "https://api.github.com/repos/"
                ++ ctx.owner
                ++ "/"
                ++ ctx.repo
                ++ "/pulls/"
                ++ String.fromInt ctx.prNumber
                ++ "/files?per_page=100"
        , body = Effect.Http.emptyBody
        , expect = expectJson (GotPRFileList ctx meta) (Decode.list githubPRFileEntryDecoder)
        , timeout = githubRequestTimeout
        , tracker = Nothing
        }


fetchFileContentCommands : PRDiffContext -> PRMeta -> GithubPRFileEntry -> List (Command BackendOnly ToFrontend BackendMsg)
fetchFileContentCommands ctx meta entry =
    let
        fetchAt sha side =
            Effect.Http.request
                { method = "GET"
                , headers = githubAuthHeaders ctx.token "application/vnd.github.raw+json"
                , url =
                    "https://api.github.com/repos/"
                        ++ ctx.owner
                        ++ "/"
                        ++ ctx.repo
                        ++ "/contents/"
                        ++ percentEncodePath entry.filename
                        ++ "?ref="
                        ++ sha
                , body = Effect.Http.emptyBody
                , expect = expectRawContentOrEmpty (GotFileContent ctx.requestKey entry.filename side)
                , timeout = githubRequestTimeout
                , tracker = Nothing
                }
    in
    case entry.status of
        GithubFileAdded ->
            [ fetchAt meta.headSha RhsSide ]

        GithubFileRemoved ->
            [ fetchAt meta.baseSha LhsSide ]

        _ ->
            [ fetchAt meta.baseSha LhsSide, fetchAt meta.headSha RhsSide ]


percentEncodePath : String -> String
percentEncodePath path =
    path
        |> String.split "/"
        |> List.map (\segment -> Url.percentEncode segment)
        |> String.join "/"



-- JSON Decoders


githubUserDecoder : Decode.Decoder GithubUser
githubUserDecoder =
    Decode.map2 GithubUser
        (Decode.field "login" Decode.string)
        (Decode.field "avatar_url" Decode.string)


githubRepoDecoder : Decode.Decoder GithubRepo
githubRepoDecoder =
    Decode.map3 GithubRepo
        (Decode.at [ "owner", "login" ] Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "full_name" Decode.string)


githubPRDecoder : Decode.Decoder GithubPullRequest
githubPRDecoder =
    Decode.map5 GithubPullRequest
        (Decode.field "number" Decode.int)
        (Decode.field "title" Decode.string)
        (Decode.at [ "user", "login" ] Decode.string)
        (Decode.at [ "base", "ref" ] Decode.string)
        (Decode.at [ "head", "ref" ] Decode.string)


prMetaDecoder : Decode.Decoder PRMeta
prMetaDecoder =
    Decode.map2 PRMeta
        (Decode.at [ "base", "sha" ] Decode.string)
        (Decode.at [ "head", "sha" ] Decode.string)


githubFileStatusDecoder : Decode.Decoder GithubFileStatus
githubFileStatusDecoder =
    Decode.string
        |> Decode.map
            (\s ->
                case s of
                    "added" ->
                        GithubFileAdded

                    "removed" ->
                        GithubFileRemoved

                    "modified" ->
                        GithubFileModified

                    "renamed" ->
                        GithubFileRenamed

                    "copied" ->
                        GithubFileCopied

                    -- GitHub also reports "changed" and "unchanged" for
                    -- some file kinds (e.g. submodules); treat those as
                    -- a normal two-sided diff like "modified".
                    _ ->
                        GithubFileModified
            )


githubPRFileEntryDecoder : Decode.Decoder GithubPRFileEntry
githubPRFileEntryDecoder =
    Decode.map2 GithubPRFileEntry
        (Decode.field "filename" Decode.string)
        (Decode.field "status" githubFileStatusDecoder)
