module Frontend exposing (..)

import Browser
import Effect.Browser.Navigation as Nav
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription
import Env
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Lamdera
import Types exposing (..)
import Url
import Url.Parser
import Url.Parser.Query


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
    , subscriptions = \_ -> Subscription.none
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
    , diffResults = []
    , error = Nothing
    }


extractCode : Url.Url -> Maybe String
extractCode url =
    { url | path = "/" }
        |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string "code"))
        |> Maybe.andThen identity


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
              , diffResults = []
              , error = Nothing
              }
            , Command.batch
                [ Effect.Lamdera.sendToBackend (AuthenticateWithCode code)
                , Nav.replaceUrl key "/"
                ]
            )

        Nothing ->
            ( emptyModel key
            , Command.none
            )


update : FrontendMsg -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged _ ->
            ( model, Command.none )

        LoginClicked ->
            ( model
            , Nav.load (githubOAuthUrl Env.githubClientId)
            )

        SelectRepo repo ->
            ( { model
                | selectedRepo = Just repo
                , page = PRListPage
                , pullRequests = []
                , error = Nothing
              }
            , Effect.Lamdera.sendToBackend (FetchPullRequests repo.owner repo.name)
            )

        SelectPR pr ->
            case model.selectedRepo of
                Just repo ->
                    ( { model
                        | selectedPR = Just pr
                        , page = PRDetailPage
                        , diffResults = []
                        , error = Nothing
                      }
                    , Effect.Lamdera.sendToBackend (FetchPRDiff repo.owner repo.name pr.number)
                    )

                Nothing ->
                    ( model, Command.none )

        BackToRepos ->
            ( { model
                | page = RepoListPage
                , selectedRepo = Nothing
                , pullRequests = []
                , selectedPR = Nothing
                , diffResults = []
                , error = Nothing
              }
            , Command.none
            )

        BackToPRs ->
            ( { model
                | page = PRListPage
                , selectedPR = Nothing
                , diffResults = []
                , error = Nothing
              }
            , Command.none
            )

        LogoutClicked ->
            ( emptyModel model.key
            , Effect.Lamdera.sendToBackend LogoutSession
            )


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

        DiffResultsLoaded results ->
            ( { model | diffResults = results }, Command.none )

        LoggedOut ->
            ( emptyModel model.key, Command.none )


githubOAuthUrl : String -> String
githubOAuthUrl clientId =
    "https://github.com/login/oauth/authorize?client_id="
        ++ clientId
        ++ "&scope=repo"



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


viewLoading : Html msg
viewLoading =
    div [ Attr.class "loading" ] [ text "Authenticating..." ]


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
        , if List.isEmpty model.diffResults then
            p [] [ text "Loading diffs..." ]

          else
            div [ Attr.class "diff-results" ]
                (List.map viewDifftResult model.diffResults)
        ]


viewDifftResult : DifftResult -> Html msg
viewDifftResult result =
    div [ Attr.class "diff-file" ]
        [ div [ Attr.class "diff-file-header" ]
            [ span [ Attr.class "diff-filename" ] [ text result.filename ]
            , span [ Attr.class "diff-language" ] [ text result.language ]
            , span [ Attr.class ("diff-status diff-status-" ++ result.status) ] [ text result.status ]
            ]
        , if List.isEmpty result.chunks then
            div [ Attr.class "diff-empty" ]
                [ text
                    (if result.status == "unchanged" then
                        "No changes"

                     else
                        ""
                    )
                ]

          else
            div [ Attr.class "diff-chunks" ]
                (List.indexedMap viewChunkGroup result.chunks)
        ]


viewChunkGroup : Int -> List DiffHunk -> Html msg
viewChunkGroup index hunks =
    div [ Attr.class "chunk-group" ]
        (if index > 0 then
            div [ Attr.class "chunk-separator" ] [ text "···" ] :: List.map viewHunk hunks

         else
            List.map viewHunk hunks
        )


viewHunk : DiffHunk -> Html msg
viewHunk hunk =
    div [ Attr.class "hunk" ]
        [ div [ Attr.class "hunk-side hunk-lhs" ]
            [ span [ Attr.class "line-number" ]
                [ text (String.fromInt (hunk.lhs.lineNumber + 1)) ]
            , span [ Attr.class "line-content" ]
                (viewChanges "removed" hunk.lhs.changes)
            ]
        , div [ Attr.class "hunk-side hunk-rhs" ]
            [ span [ Attr.class "line-number" ]
                [ text (String.fromInt (hunk.rhs.lineNumber + 1)) ]
            , span [ Attr.class "line-content" ]
                (viewChanges "added" hunk.rhs.changes)
            ]
        ]


viewChanges : String -> List DiffChange -> List (Html msg)
viewChanges side changes =
    List.map
        (\change ->
            span
                [ Attr.class ("diff-change diff-change-" ++ side ++ " highlight-" ++ change.highlight) ]
                [ text change.content ]
        )
        changes


css : String
css =
    """
* { box-sizing: border-box; margin: 0; padding: 0; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #0d1117; color: #c9d1d9; }
.app { max-width: 1200px; margin: 0 auto; padding: 0 16px; }
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
.diff-status-changed { background: #3fb95040; color: #3fb950; }
.diff-status-added { background: #23863640; color: #3fb950; }
.diff-status-removed { background: #f8514940; color: #f85149; }
.diff-empty { padding: 16px; color: #8b949e; text-align: center; }
.chunk-group { padding: 0; }
.chunk-separator { text-align: center; padding: 4px 0; color: #484f58; background: #0d1117; }
.hunk { display: flex; font-family: 'SF Mono', 'Fira Code', monospace; font-size: 13px; line-height: 20px; }
.hunk-side { flex: 1; display: flex; padding: 0 8px; min-width: 0; }
.hunk-lhs { background: #1c1210; border-right: 1px solid #30363d; }
.hunk-rhs { background: #0f1a10; }
.line-number { color: #484f58; min-width: 48px; text-align: right; padding-right: 12px; user-select: none; flex-shrink: 0; }
.line-content { white-space: pre-wrap; word-break: break-all; flex: 1; min-width: 0; }
.diff-change-removed { background: #67060c80; color: #ffa198; }
.diff-change-added { background: #03501580; color: #7ee787; }
.highlight-string { font-style: italic; }
.highlight-keyword { font-weight: bold; }
h2 { margin-bottom: 16px; color: #f0f6fc; }
"""
