module Types exposing (..)

import Browser
import Dict exposing (Dict)
import Effect.Browser.Navigation
import Set exposing (Set)
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


{-| A changed file's status, as reported by the GitHub PR files API.
Distinct from `DiffStatus`, which is difftastic's own assessment after
actually comparing the two texts (they usually agree, but GitHub's is the
one we know before diffing, so it drives what content we need to fetch).
-}
type GithubFileStatus
    = GithubFileAdded
    | GithubFileRemoved
    | GithubFileModified
    | GithubFileRenamed
    | GithubFileCopied


{-| One changed file's raw before/after text, fetched from GitHub but not
yet diffed. `lhsContent`/`rhsContent` are always plain strings (never
absent) — for an added file there's simply nothing on the base side, so
that side is "", not a missing value to check for.
-}
type alias PRFile =
    { filename : String
    , status : GithubFileStatus
    , lhsContent : String
    , rhsContent : String
    }


{-| Where a PR's diffs are in the fetch-then-compute pipeline. There is no
field anywhere that can independently say "loading" while another field
already holds file data, or "ready" while secretly still missing a
result — each stage carries exactly the data that exists at that stage
and no other.
-}
type DiffsState
    = DiffsNotStarted
    | FetchingPRFiles
    | ComputingDiffsInBrowser (List PRFile)
    | DiffsReady (List FileDiff)
    | DiffsFetchFailed String


{-| The result of asking the wasm module to diff one file. A file can fail
to diff on its own (e.g. a decode error) without sinking every other
file in the PR.
-}
type alias FileDiff =
    { filename : String
    , status : GithubFileStatus
    , lhsContent : String
    , rhsContent : String
    , outcome : DiffOutcome
    }


type DiffOutcome
    = DiffComputed DifftResult
    | DiffFailed String


{-| difftastic's own verdict on the pair of texts it was given, from
`display/json.rs`'s `Status` enum (`unchanged` | `changed` | `created` |
`deleted`).
-}
type DiffStatus
    = StatusUnchanged
    | StatusChanged
    | StatusCreated
    | StatusDeleted


{-| difftastic's syntax highlight classification for one changed span,
from `display/json.rs`'s `Highlight` enum.
-}
type Highlight
    = HighlightDelimiter
    | HighlightNormal
    | HighlightString
    | HighlightType
    | HighlightComment
    | HighlightKeyword
    | HighlightTreeSitterError


type alias DiffHunk =
    { lhs : Maybe DiffSide
    , rhs : Maybe DiffSide
    }


type alias DiffSide =
    { lineNumber : Int
    , changes : List DiffChange
    }


type alias DiffChange =
    { start : Int
    , end : Int
    , content : String
    , highlight : Highlight
    }


{-| One syntax-classified token span, from difftastic's whole-file
`lhs_syntax`/`rhs_syntax` maps. Unlike `DiffChange`, these cover the
entire file (changed or not) so unchanged text can be syntax-colored
too. Only non-Normal tokens are emitted; text not covered by any span
renders in the default color. `line` is 0-based; `start`/`end` are
column offsets.
-}
type alias SyntaxSpan =
    { line : Int
    , start : Int
    , end : Int
    , highlight : Highlight
    }


{-| One file's structural diff, decoded from the wasm module's JSON
output (the same shape as `difft --display=json` for a single file).
-}
type alias DifftResult =
    { filename : String
    , language : String
    , status : DiffStatus
    , chunks : List (List DiffHunk)
    , lhsSyntax : List SyntaxSpan
    , rhsSyntax : List SyntaxSpan
    }


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , page : Page
    , githubUser : Maybe GithubUser
    , repos : List GithubRepo
    , selectedRepo : Maybe GithubRepo
    , pullRequests : List GithubPullRequest
    , selectedPR : Maybe GithubPullRequest
    , diffs : DiffsState
    , expandedContexts : Set String
    , error : Maybe String
    }


{-| One PR-diff fetch in flight on the backend: which files still need
their base and/or head content before the whole set can be sent to the
frontend.
-}
type alias PendingPRDiff =
    { clientId : String
    , owner : String
    , repo : String
    , files : Dict String PendingFile
    }


type alias PendingFile =
    { status : GithubFileStatus
    , lhs : RemoteContent
    , rhs : RemoteContent
    }


{-| One side (base or head) of one file's content, as tracked while a PR
diff fetch is in flight. `NotNeeded` (e.g. the base side of an added
file) and "still waiting on the network" are different states — neither
can be confused with an empty string that was actually fetched.
-}
type RemoteContent
    = NotNeeded
    | ContentPending
    | ContentFetched String


type alias BackendModel =
    { sessions : Dict String String
    , users : Dict String GithubUser
    , pendingPRDiffs : Dict String PendingPRDiff
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
    | ExpandContext String Int
    | GotDiffsFromWasm (Result String (List FileDiff))
    | CancelAuthentication
    | AuthAttemptTimedOut
    | SendAuthenticateWithCode String


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
    | GotPRMeta PRDiffContext (Result String PRMeta)
    | GotPRFileList PRDiffContext PRMeta (Result String (List GithubPRFileEntry))
    | GotFileContent String String FileSide (Result String String)
    | NoOpBackendMsg


{-| Everything needed to continue a PR-diff fetch after each async GitHub
API response, bundled so the chain of `BackendMsg`s doesn't have to pass
five separate positional strings through every step.
-}
type alias PRDiffContext =
    { clientIdStr : String
    , owner : String
    , repo : String
    , prNumber : Int
    , token : String
    , requestKey : String
    }


{-| Which half of a file's content a single GitHub content-fetch request
was for. Needed to route the response back into the right slot of the
`PendingFile` it belongs to.
-}
type FileSide
    = LhsSide
    | RhsSide


type alias PRMeta =
    { baseSha : String
    , headSha : String
    }


type alias GithubPRFileEntry =
    { filename : String
    , status : GithubFileStatus
    }


type ToFrontend
    = AuthSuccess GithubUser
    | AuthFailure String
    | ReposLoaded (List GithubRepo)
    | PullRequestsLoaded (List GithubPullRequest)
    | PRFilesLoaded (List PRFile)
    | PRDiffFetchFailed String
    | LoggedOut
