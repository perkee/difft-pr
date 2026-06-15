module UnitTests exposing (..)

import Backend
import Expect
import Frontend
import Json.Decode
import Test exposing (Test, describe, test)
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
            , test "decodes difft result with chunks" <|
                \_ ->
                    let
                        json =
                            """{"filename":"test.js","language":"JavaScript","status":"changed","chunks":[[{"lhs":{"line_number":0,"changes":[{"start":10,"end":15,"content":"hello","highlight":"string"}]},"rhs":{"line_number":0,"changes":[{"start":10,"end":13,"content":"bye","highlight":"string"}]}}]]}"""
                    in
                    Json.Decode.decodeString Backend.difftResultDecoder json
                        |> Expect.equal
                            (Ok
                                { filename = "test.js"
                                , language = "JavaScript"
                                , status = "changed"
                                , chunks =
                                    [ [ { lhs =
                                            { lineNumber = 0
                                            , changes =
                                                [ { start = 10, end = 15, content = "hello", highlight = "string" } ]
                                            }
                                        , rhs =
                                            { lineNumber = 0
                                            , changes =
                                                [ { start = 10, end = 13, content = "bye", highlight = "string" } ]
                                            }
                                        }
                                      ]
                                    ]
                                }
                            )
            , test "decodes difft result with empty chunks" <|
                \_ ->
                    let
                        json =
                            """{"filename":"new.txt","language":"Text","status":"added","chunks":[]}"""
                    in
                    Json.Decode.decodeString Backend.difftResultDecoder json
                        |> Expect.equal
                            (Ok
                                { filename = "new.txt"
                                , language = "Text"
                                , status = "added"
                                , chunks = []
                                }
                            )
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
        ]
