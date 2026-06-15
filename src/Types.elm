module Types exposing (..)

import Browser
import Dict exposing (Dict)
import Effect.Browser.Navigation
import Url exposing (Url)


type Page
    = LoginPage
    | LoadingPage
    | RepoListPage
    | PRListPage
    | PRDetailPage


type alias GithubUser =
    { login : String
    , avatarUrl : String
    }


type alias GithubRepo =
    { owner : String
    , name : String
    , fullName : String
    }


type alias GithubPullRequest =
    { number : Int
    , title : String
    , author : String
    , baseBranch : String
    , headBranch : String
    }


type alias DiffHunk =
    { lhs : DiffSide
    , rhs : DiffSide
    }


type alias DiffSide =
    { lineNumber : Int
    , changes : List DiffChange
    }


type alias DiffChange =
    { start : Int
    , end : Int
    , content : String
    , highlight : String
    }


type alias DifftResult =
    { filename : String
    , language : String
    , status : String
    , chunks : List (List DiffHunk)
    }


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , page : Page
    , githubUser : Maybe GithubUser
    , repos : List GithubRepo
    , selectedRepo : Maybe GithubRepo
    , pullRequests : List GithubPullRequest
    , selectedPR : Maybe GithubPullRequest
    , diffResults : List DifftResult
    , error : Maybe String
    }


type alias BackendModel =
    { sessions : Dict String String
    , users : Dict String GithubUser
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | LoginClicked
    | SelectRepo GithubRepo
    | SelectPR GithubPullRequest
    | BackToRepos
    | BackToPRs
    | LogoutClicked


type ToBackend
    = AuthenticateWithCode String
    | FetchRepos
    | FetchPullRequests String String
    | FetchPRDiff String String Int
    | LogoutSession


type BackendMsg
    = GotAccessToken String String (Result String String)
    | GotGithubUser String String String (Result String GithubUser)
    | GotRepos String (Result String (List GithubRepo))
    | GotPullRequests String (Result String (List GithubPullRequest))
    | GotPRDiffResults String (Result String (List DifftResult))
    | NoOpBackendMsg


type ToFrontend
    = AuthSuccess GithubUser
    | AuthFailure String
    | ReposLoaded (List GithubRepo)
    | PullRequestsLoaded (List GithubPullRequest)
    | DiffResultsLoaded (List DifftResult)
    | LoggedOut
