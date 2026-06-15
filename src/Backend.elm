module Backend exposing (..)

import Dict
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Http
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Subscription as Subscription
import Env
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera
import Types exposing (..)


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


expectJson : (Result String a -> msg) -> Decode.Decoder a -> Effect.Http.Expect msg
expectJson toMsg decoder =
    Effect.Http.expectJson
        (\result -> toMsg (Result.mapError httpErrorToString result))
        decoder


expectString : (Result String String -> msg) -> Effect.Http.Expect msg
expectString toMsg =
    Effect.Http.expectString
        (\result -> toMsg (Result.mapError httpErrorToString result))


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

        GotPRDiffResults clientIdStr result ->
            let
                clientId =
                    Effect.Lamdera.clientIdFromString clientIdStr
            in
            case result of
                Ok results ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (DiffResultsLoaded results)
                    )

                Err error ->
                    ( model
                    , Effect.Lamdera.sendToFrontend clientId (AuthFailure error)
                    )

        NoOpBackendMsg ->
            ( model, Command.none )


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
                    ( model, fetchPRDiff clientIdStr token owner repo prNumber )

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


exchangeCodeForToken : String -> String -> String -> Command BackendOnly ToFrontend BackendMsg
exchangeCodeForToken sessionIdStr clientIdStr code =
    Effect.Http.post
        { url = "https://github.com/login/oauth/access_token"
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
        }


fetchGithubUser : String -> String -> String -> Command BackendOnly ToFrontend BackendMsg
fetchGithubUser sessionIdStr clientIdStr token =
    Effect.Http.request
        { method = "GET"
        , headers =
            [ Effect.Http.header "Authorization" ("Bearer " ++ token)
            , Effect.Http.header "Accept" "application/json"
            ]
        , url = "https://api.github.com/user"
        , body = Effect.Http.emptyBody
        , expect =
            expectJson
                (GotGithubUser sessionIdStr clientIdStr token)
                githubUserDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchRepos : String -> String -> Command BackendOnly ToFrontend BackendMsg
fetchRepos clientIdStr token =
    Effect.Http.request
        { method = "GET"
        , headers =
            [ Effect.Http.header "Authorization" ("Bearer " ++ token)
            , Effect.Http.header "Accept" "application/json"
            ]
        , url = "https://api.github.com/user/repos?sort=updated&per_page=30&type=owner"
        , body = Effect.Http.emptyBody
        , expect =
            expectJson
                (GotRepos clientIdStr)
                (Decode.list githubRepoDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


fetchPullRequests : String -> String -> String -> String -> Command BackendOnly ToFrontend BackendMsg
fetchPullRequests clientIdStr token owner repo =
    Effect.Http.request
        { method = "GET"
        , headers =
            [ Effect.Http.header "Authorization" ("Bearer " ++ token)
            , Effect.Http.header "Accept" "application/json"
            ]
        , url = "https://api.github.com/repos/" ++ owner ++ "/" ++ repo ++ "/pulls?state=open&per_page=30"
        , body = Effect.Http.emptyBody
        , expect =
            expectJson
                (GotPullRequests clientIdStr)
                (Decode.list githubPRDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


fetchPRDiff : String -> String -> String -> String -> Int -> Command BackendOnly ToFrontend BackendMsg
fetchPRDiff clientIdStr token owner repo prNumber =
    Effect.Http.post
        { url = Env.difftServiceUrl ++ "/pr-diff"
        , body =
            Effect.Http.jsonBody
                (Encode.object
                    [ ( "owner", Encode.string owner )
                    , ( "repo", Encode.string repo )
                    , ( "pr_number", Encode.int prNumber )
                    , ( "token", Encode.string token )
                    ]
                )
        , expect =
            expectJson
                (GotPRDiffResults clientIdStr)
                (Decode.list difftResultDecoder)
        }



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


difftResultDecoder : Decode.Decoder DifftResult
difftResultDecoder =
    Decode.map4 DifftResult
        (Decode.field "filename" Decode.string)
        (Decode.field "language" Decode.string)
        (Decode.field "status" Decode.string)
        (Decode.field "chunks" (Decode.list (Decode.list diffHunkDecoder)))


diffHunkDecoder : Decode.Decoder DiffHunk
diffHunkDecoder =
    Decode.map2 DiffHunk
        (Decode.field "lhs" diffSideDecoder)
        (Decode.field "rhs" diffSideDecoder)


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
        (Decode.field "highlight" Decode.string)
