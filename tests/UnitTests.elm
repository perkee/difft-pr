module UnitTests exposing (..)

import Backend
import Dict
import Expect
import FileTree
import Frontend
import Html
import Html.Attributes as Attr
import Json.Decode
import Set
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Types exposing (..)
import Url


suite : Test
suite =
    describe "difft-pr"
        [ describe "JSON decoders"
            [ test "decodes GitHub user" <|
                \_ ->
                    let
                        json =
                            """{"login":"octocat","avatar_url":"https://example.com/avatar.png","id":1}"""
                    in
                    Json.Decode.decodeString Backend.githubUserDecoder json
                        |> Expect.equal
                            (Ok { login = "octocat", avatarUrl = "https://example.com/avatar.png" })
            , test "decodes GitHub repo" <|
                \_ ->
                    let
                        json =
                            """{"name":"hello","full_name":"octocat/hello","owner":{"login":"octocat"}}"""
                    in
                    Json.Decode.decodeString Backend.githubRepoDecoder json
                        |> Expect.equal
                            (Ok { owner = "octocat", name = "hello", fullName = "octocat/hello" })
            , test "decodes GitHub PR" <|
                \_ ->
                    let
                        json =
                            """{"number":42,"title":"Fix bug","user":{"login":"dev"},"base":{"ref":"main"},"head":{"ref":"fix-it"}}"""
                    in
                    Json.Decode.decodeString Backend.githubPRDecoder json
                        |> Expect.equal
                            (Ok
                                { number = 42
                                , title = "Fix bug"
                                , author = "dev"
                                , baseBranch = "main"
                                , headBranch = "fix-it"
                                }
                            )
            , test "decodes a GitHub PR file entry's status" <|
                \_ ->
                    let
                        json =
                            """{"filename":"src/Foo.elm","status":"modified"}"""
                    in
                    Json.Decode.decodeString Backend.githubPRFileEntryDecoder json
                        |> Expect.equal
                            (Ok { filename = "src/Foo.elm", status = GithubFileModified })
            , test "decodes a difft JSON result (the wasm module's own output shape)" <|
                \_ ->
                    let
                        json =
                            """{"path":"test.js","language":"JavaScript","status":"changed","chunks":[[{"lhs":{"line_number":0,"changes":[{"start":10,"end":15,"content":"hello","highlight":"string"}]},"rhs":{"line_number":0,"changes":[{"start":10,"end":13,"content":"bye","highlight":"string"}]}}]]}"""
                    in
                    Json.Decode.decodeString Frontend.difftResultDecoder json
                        |> Expect.equal
                            (Ok
                                { filename = "test.js"
                                , language = "JavaScript"
                                , status = StatusChanged
                                , chunks =
                                    [ [ { lhs =
                                            Just
                                                { lineNumber = 0
                                                , changes =
                                                    [ { start = 10, end = 15, content = "hello", highlight = HighlightString } ]
                                                }
                                        , rhs =
                                            Just
                                                { lineNumber = 0
                                                , changes =
                                                    [ { start = 10, end = 13, content = "bye", highlight = HighlightString } ]
                                                }
                                        }
                                      ]
                                    ]
                                , lhsSyntax = []
                                , rhsSyntax = []
                                }
                            )
            , test "decodes a difft result with an explicit empty chunks array" <|
                \_ ->
                    let
                        json =
                            """{"path":"new.txt","language":"Text","status":"unchanged","chunks":[]}"""
                    in
                    Json.Decode.decodeString Frontend.difftResultDecoder json
                        |> Expect.equal
                            (Ok
                                { filename = "new.txt"
                                , language = "Text"
                                , status = StatusUnchanged
                                , chunks = []
                                , lhsSyntax = []
                                , rhsSyntax = []
                                }
                            )
            , test "decodes a difft result with chunks omitted entirely (a created or deleted file)" <|
                \_ ->
                    let
                        json =
                            """{"path":"new.elm","language":"Elm","status":"created"}"""
                    in
                    Json.Decode.decodeString Frontend.difftResultDecoder json
                        |> Expect.equal
                            (Ok
                                { filename = "new.elm"
                                , language = "Elm"
                                , status = StatusCreated
                                , chunks = []
                                , lhsSyntax = []
                                , rhsSyntax = []
                                }
                            )
            , test "decodes a difft result carrying whole-file syntax maps" <|
                \_ ->
                    let
                        json =
                            """{"path":"a.elm","language":"Elm","status":"changed","chunks":[],"lhs_syntax":[{"line":0,"start":0,"end":6,"highlight":"keyword"}],"rhs_syntax":[{"line":2,"start":4,"end":11,"highlight":"string"}]}"""
                    in
                    Json.Decode.decodeString Frontend.difftResultDecoder json
                        |> Expect.equal
                            (Ok
                                { filename = "a.elm"
                                , language = "Elm"
                                , status = StatusChanged
                                , chunks = []
                                , lhsSyntax = [ { line = 0, start = 0, end = 6, highlight = HighlightKeyword } ]
                                , rhsSyntax = [ { line = 2, start = 4, end = 11, highlight = HighlightString } ]
                                }
                            )
            , test "wasmResponseDecoder decodes a successful batch" <|
                \_ ->
                    let
                        json =
                            """{"ok":true,"results":[{"filename":"a.txt","status":"modified","lhsContent":"old","rhsContent":"new","diff":{"path":"a.txt","language":"Text","status":"changed","chunks":[]}}]}"""
                    in
                    Json.Decode.decodeString Frontend.wasmResponseDecoder json
                        |> Expect.equal
                            (Ok
                                (Ok
                                    [ { filename = "a.txt"
                                      , status = GithubFileModified
                                      , lhsContent = "old"
                                      , rhsContent = "new"
                                      , outcome =
                                            DiffComputed
                                                { filename = "a.txt"
                                                , language = "Text"
                                                , status = StatusChanged
                                                , chunks = []
                                                , lhsSyntax = []
                                                , rhsSyntax = []
                                                }
                                      }
                                    ]
                                )
                            )
            , test "wasmResponseDecoder decodes a per-file failure inside a successful batch" <|
                \_ ->
                    let
                        json =
                            """{"ok":true,"results":[{"filename":"weird.bin","status":"modified","lhsContent":"","rhsContent":"","error":"boom"}]}"""
                    in
                    Json.Decode.decodeString Frontend.wasmResponseDecoder json
                        |> Expect.equal
                            (Ok
                                (Ok
                                    [ { filename = "weird.bin"
                                      , status = GithubFileModified
                                      , lhsContent = ""
                                      , rhsContent = ""
                                      , outcome = DiffFailed "boom"
                                      }
                                    ]
                                )
                            )
            , test "wasmResponseDecoder decodes a whole-batch failure" <|
                \_ ->
                    let
                        json =
                            """{"ok":false,"error":"wasm module crashed"}"""
                    in
                    Json.Decode.decodeString Frontend.wasmResponseDecoder json
                        |> Expect.equal (Ok (Err "wasm module crashed"))
            ]
        , describe "Frontend helpers"
            [ test "extractCode finds code in URL" <|
                \_ ->
                    let
                        url =
                            { protocol = Url.Https
                            , host = "localhost"
                            , port_ = Nothing
                            , path = "/"
                            , query = Just "code=abc123"
                            , fragment = Nothing
                            }
                    in
                    Frontend.extractCode url
                        |> Expect.equal (Just "abc123")
            , test "extractCode returns Nothing when no code" <|
                \_ ->
                    let
                        url =
                            { protocol = Url.Https
                            , host = "localhost"
                            , port_ = Nothing
                            , path = "/"
                            , query = Nothing
                            , fragment = Nothing
                            }
                    in
                    Frontend.extractCode url
                        |> Expect.equal Nothing
            ]
        , describe "Backend PR-file content aggregation"
            [ test "an added file only needs its head (rhs) content" <|
                \_ ->
                    Backend.initPendingFile GithubFileAdded
                        |> Expect.equal { status = GithubFileAdded, lhs = NotNeeded, rhs = ContentPending }
            , test "a removed file only needs its base (lhs) content" <|
                \_ ->
                    Backend.initPendingFile GithubFileRemoved
                        |> Expect.equal { status = GithubFileRemoved, lhs = ContentPending, rhs = NotNeeded }
            , test "a modified file needs both sides" <|
                \_ ->
                    Backend.initPendingFile GithubFileModified
                        |> Expect.equal { status = GithubFileModified, lhs = ContentPending, rhs = ContentPending }
            , test "allFilesResolved is False while any needed side is still pending" <|
                \_ ->
                    Dict.fromList
                        [ ( "a.txt", { status = GithubFileAdded, lhs = NotNeeded, rhs = ContentFetched "x" } )
                        , ( "b.txt", { status = GithubFileModified, lhs = ContentFetched "y", rhs = ContentPending } )
                        ]
                        |> Backend.allFilesResolved
                        |> Expect.equal False
            , test "allFilesResolved is True once every needed side is fetched" <|
                \_ ->
                    Dict.fromList
                        [ ( "a.txt", { status = GithubFileAdded, lhs = NotNeeded, rhs = ContentFetched "x" } )
                        , ( "b.txt", { status = GithubFileModified, lhs = ContentFetched "y", rhs = ContentFetched "z" } )
                        ]
                        |> Backend.allFilesResolved
                        |> Expect.equal True
            , test "resolvedPRFiles turns NotNeeded sides into empty strings, not missing data" <|
                \_ ->
                    Dict.fromList
                        [ ( "new.txt", { status = GithubFileAdded, lhs = NotNeeded, rhs = ContentFetched "hello" } ) ]
                        |> Backend.resolvedPRFiles
                        |> Expect.equal
                            [ { filename = "new.txt", status = GithubFileAdded, lhsContent = "", rhsContent = "hello" } ]
            ]
        , describe "viewDiffBody single-column rendering for created and deleted files"
            [ test "an added file renders one full-width column with no left (lhs) side at all" <|
                \_ ->
                    let
                        fileDiff =
                            { filename = "new.txt"
                            , status = GithubFileAdded
                            , lhsContent = ""
                            , rhsContent = "first line\nsecond line"
                            , outcome = DiffComputed addedResult
                            }

                        addedResult =
                            { filename = "new.txt", language = "Text", status = StatusCreated, chunks = [], lhsSyntax = [], rhsSyntax = [] }
                    in
                    Frontend.viewDiffBody Set.empty fileDiff addedResult
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.hasNot [ Selector.class "hunk-lhs" ]
                            , Query.has
                                [ Selector.class "hunk-single"
                                , Selector.text "first"
                                , Selector.text "second"
                                ]
                            ]
            , test "a deleted file renders one full-width column with no right (rhs) side at all" <|
                \_ ->
                    let
                        fileDiff =
                            { filename = "old.txt"
                            , status = GithubFileRemoved
                            , lhsContent = "goodbye line"
                            , rhsContent = ""
                            , outcome = DiffComputed deletedResult
                            }

                        deletedResult =
                            { filename = "old.txt", language = "Text", status = StatusDeleted, chunks = [], lhsSyntax = [], rhsSyntax = [] }
                    in
                    Frontend.viewDiffBody Set.empty fileDiff deletedResult
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.hasNot [ Selector.class "hunk-rhs" ]
                            , Query.has
                                [ Selector.class "hunk-single"
                                , Selector.text "goodbye"
                                ]
                            ]
            , test "a modified file still renders both columns" <|
                \_ ->
                    let
                        fileDiff =
                            { filename = "mod.txt"
                            , status = GithubFileModified
                            , lhsContent = "old"
                            , rhsContent = "new"
                            , outcome = DiffComputed modifiedResult
                            }

                        modifiedResult =
                            { filename = "mod.txt"
                            , language = "Text"
                            , status = StatusChanged
                            , chunks =
                                [ [ { lhs = Just { lineNumber = 0, changes = [ { start = 0, end = 3, content = "old", highlight = HighlightNormal } ] }
                                    , rhs = Just { lineNumber = 0, changes = [ { start = 0, end = 3, content = "new", highlight = HighlightNormal } ] }
                                    }
                                  ]
                                ]
                            , lhsSyntax = []
                            , rhsSyntax = []
                            }
                    in
                    Frontend.viewDiffBody Set.empty fileDiff modifiedResult
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.class "hunk-lhs" ]
                            , Query.has [ Selector.class "hunk-rhs" ]
                            , Query.hasNot [ Selector.class "hunk-single" ]
                            ]
            ]
        , describe "FileTree.build"
            [ test "nests files under their directories, directories before files" <|
                \_ ->
                    FileTree.build
                        [ { path = "README.md", status = GithubFileModified }
                        , { path = "src/B.elm", status = GithubFileAdded }
                        , { path = "src/A.elm", status = GithubFileModified }
                        ]
                        |> Expect.equal
                            [ FileTree.Directory "src"
                                [ FileTree.Leaf { path = "src/A.elm", status = GithubFileModified }
                                , FileTree.Leaf { path = "src/B.elm", status = GithubFileAdded }
                                ]
                            , FileTree.Leaf { path = "README.md", status = GithubFileModified }
                            ]
            , test "compresses single-child directory chains into one node" <|
                \_ ->
                    FileTree.build [ { path = "src/elm/App.elm", status = GithubFileModified } ]
                        |> Expect.equal
                            [ FileTree.Directory "src/elm"
                                [ FileTree.Leaf { path = "src/elm/App.elm", status = GithubFileModified } ]
                            ]
            , test "does not compress a directory that has its own files" <|
                \_ ->
                    FileTree.build
                        [ { path = "src/elm/App.elm", status = GithubFileModified }
                        , { path = "src/top.txt", status = GithubFileRemoved }
                        ]
                        |> Expect.equal
                            [ FileTree.Directory "src"
                                [ FileTree.Directory "elm"
                                    [ FileTree.Leaf { path = "src/elm/App.elm", status = GithubFileModified } ]
                                , FileTree.Leaf { path = "src/top.txt", status = GithubFileRemoved }
                                ]
                            ]
            ]
        , describe "FileTree.filter"
            [ test "matches substrings case-insensitively against the full path" <|
                \_ ->
                    FileTree.filter "ELM"
                        [ { path = "src/App.elm", status = GithubFileModified }
                        , { path = "README.md", status = GithubFileModified }
                        ]
                        |> Expect.equal [ { path = "src/App.elm", status = GithubFileModified } ]
            , test "a blank query keeps everything" <|
                \_ ->
                    FileTree.filter "  "
                        [ { path = "a.txt", status = GithubFileAdded } ]
                        |> Expect.equal [ { path = "a.txt", status = GithubFileAdded } ]
            ]
        , describe "FileTree.extensionOf"
            [ test "takes the last extension, lowercased" <|
                \_ ->
                    FileTree.extensionOf "src/a.test.JS"
                        |> Expect.equal "js"
            , test "a file without a dot has no extension" <|
                \_ ->
                    FileTree.extensionOf "Makefile"
                        |> Expect.equal ""
            ]
        , describe "FileTree.view"
            [ test "renders a filter input and a status-classed button per file" <|
                \_ ->
                    FileTree.view
                        { inputText = ""
                        , activeFilter = ""
                        , onFilterChanged = always ()
                        , onFileSelected = always ()
                        }
                        [ { path = "src/App.elm", status = GithubFileRenamed } ]
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.class "file-tree-filter" ]
                            , Query.find [ Selector.class "tree-file" ]
                                >> Query.has [ Selector.class "tree-status-renamed", Selector.text "App.elm" ]
                            ]
            , test "shows an empty state when the filter matches nothing" <|
                \_ ->
                    FileTree.view
                        { inputText = "zzz"
                        , activeFilter = "zzz"
                        , onFilterChanged = always ()
                        , onFileSelected = always ()
                        }
                        [ { path = "src/App.elm", status = GithubFileModified } ]
                        |> Query.fromHtml
                        |> Query.has [ Selector.class "file-tree-empty" ]
            , test "filters by the settled activeFilter, not the still-debouncing input text" <|
                \_ ->
                    -- Mid-typing, inputText is ahead of activeFilter: the box
                    -- shows the keystrokes but the tree must not re-shape
                    -- until the debounce settles.
                    FileTree.view
                        { inputText = "zzz"
                        , activeFilter = ""
                        , onFilterChanged = always ()
                        , onFileSelected = always ()
                        }
                        [ { path = "src/App.elm", status = GithubFileModified } ]
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.class "tree-file" ]
                            , Query.hasNot [ Selector.class "file-tree-empty" ]
                            ]
            ]
        , describe "viewViewOptions"
            [ test "the wrap toggle is a real checkbox, unchecked when wrapping is off" <|
                \_ ->
                    Frontend.viewViewOptions False
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.text "Wrap lines" ]
                            , Query.find [ Selector.tag "input" ]
                                >> Query.has
                                    [ Selector.attribute (Attr.type_ "checkbox")
                                    , Selector.checked False
                                    ]
                            ]
            , test "the wrap toggle checkbox is checked when wrapping is on" <|
                \_ ->
                    Frontend.viewViewOptions True
                        |> Query.fromHtml
                        |> Query.find [ Selector.tag "input" ]
                        |> Query.has [ Selector.checked True ]
            ]
        , describe "session restore"
            [ test "a known session restores its user" <|
                \_ ->
                    let
                        user =
                            { login = "octocat", avatarUrl = "https://example.com/a.png" }
                    in
                    Backend.sessionResponse (Dict.fromList [ ( "session-1", user ) ]) "session-1"
                        |> Expect.equal (AuthSuccess user)
            , test "an unknown session gets an explicit NoSession, not silence" <|
                \_ ->
                    Backend.sessionResponse Dict.empty "session-1"
                        |> Expect.equal NoSession
            ]
        , describe "lineSegments"
            [ test "no spans at all yields one plain segment" <|
                \_ ->
                    Frontend.lineSegments [] [] "plain text"
                        |> Expect.equal
                            [ { text = "plain text", syntax = Nothing, changed = False } ]
            , test "a syntax span in the middle splits the line into three segments" <|
                \_ ->
                    Frontend.lineSegments
                        [ { line = 0, start = 4, end = 8, highlight = HighlightKeyword } ]
                        []
                        "let case of"
                        |> Expect.equal
                            [ { text = "let ", syntax = Nothing, changed = False }
                            , { text = "case", syntax = Just HighlightKeyword, changed = False }
                            , { text = " of", syntax = Nothing, changed = False }
                            ]
            , test "a change inside a syntax span carries both the syntax and the changed flag" <|
                \_ ->
                    -- The tw-ml-auto → tw-ms-auto scenario: the whole literal is
                    -- string-highlighted, only "ml" changed within it.
                    Frontend.lineSegments
                        [ { line = 0, start = 0, end = 11, highlight = HighlightString } ]
                        [ { start = 3, end = 5, content = "ml", highlight = HighlightNormal } ]
                        "tw-ml-auto\""
                        |> Expect.equal
                            [ { text = "tw-", syntax = Just HighlightString, changed = False }
                            , { text = "ml", syntax = Just HighlightString, changed = True }
                            , { text = "-auto\"", syntax = Just HighlightString, changed = False }
                            ]
            ]
        , describe "viewHighlightedLine"
            [ test "renders the unchanged text around a change, not just the changed span" <|
                \_ ->
                    let
                        fullLine =
                            "            , Semantic.triggerChevron [ class \"tw-ml-auto\" ] []"

                        change =
                            { start = 50, end = 52, content = "ml", highlight = HighlightNormal }
                    in
                    Html.div [] (Frontend.viewHighlightedLine Frontend.SideRemoved [] [ change ] fullLine)
                        |> Query.fromHtml
                        |> Query.has
                            [ Selector.text "Semantic.triggerChevron"
                            , Selector.text "tw-"
                            , Selector.text "-auto\""
                            ]
            , test "the changed span is wrapped in a diff-change element" <|
                \_ ->
                    let
                        change =
                            { start = 7, end = 9, content = "ml", highlight = HighlightNormal }
                    in
                    Html.div [] (Frontend.viewHighlightedLine Frontend.SideRemoved [] [ change ] "before ml after")
                        |> Query.fromHtml
                        |> Query.find [ Selector.class "diff-change-removed" ]
                        |> Query.has [ Selector.text "ml" ]
            , test "a line with no changes renders with no diff-change element" <|
                \_ ->
                    Html.div [] (Frontend.viewHighlightedLine Frontend.SideRemoved [] [] "unchanged line")
                        |> Query.fromHtml
                        |> Query.hasNot [ Selector.class "diff-change" ]
            , test "syntax spans render as syn-* colored elements" <|
                \_ ->
                    Html.div []
                        (Frontend.viewHighlightedLine Frontend.SideAdded
                            [ { line = 0, start = 0, end = 6, highlight = HighlightKeyword } ]
                            []
                            "module A"
                        )
                        |> Query.fromHtml
                        |> Query.find [ Selector.class "syn-keyword" ]
                        |> Query.has [ Selector.text "module" ]
            , test "changes covering the whole line get no vivid marking (the line tint suffices)" <|
                \_ ->
                    Html.div []
                        (Frontend.viewHighlightedLine Frontend.SideAdded
                            []
                            [ { start = 0, end = 5, content = "whole", highlight = HighlightNormal }
                            , { start = 6, end = 10, content = "line", highlight = HighlightNormal }
                            ]
                            "whole line"
                        )
                        |> Query.fromHtml
                        |> Query.hasNot [ Selector.class "diff-change" ]
            , test "changed spans separated only by whitespace merge into one vivid run" <|
                \_ ->
                    Html.div []
                        (Frontend.viewHighlightedLine Frontend.SideAdded
                            []
                            [ { start = 5, end = 7, content = "aa", highlight = HighlightNormal }
                            , { start = 8, end = 10, content = "bb", highlight = HighlightNormal }
                            ]
                            "left aa bb right"
                        )
                        |> Query.fromHtml
                        |> Query.findAll [ Selector.class "diff-change" ]
                        |> Query.count (Expect.equal 1)
            ]
        ]
