module UnitTests exposing (..)

import Backend
import Dict
import Expect
import Frontend
import Json.Decode
import Test exposing (Test, describe, test)
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
        ]
