module FileTree exposing
    ( FileTree(..)
    , TreeFile
    , build
    , extensionOf
    , filter
    , view
    )

{-| The PR file tree sidebar: the changed files of a PR arranged by
directory, filterable by substring, each file carrying an icon for its
change status (added/modified/moved/deleted) and a chip for its file
type.

The tree-shaping logic (`build`, `filter`) is pure and separately
testable; `view` is parameterized by the caller's message constructors
so this module doesn't depend on the app's Msg type.

-}

import Html exposing (Html, button, div, input, span, text)
import Html.Attributes as Attr
import Html.Events
import Types exposing (GithubFileStatus(..))


{-| One changed file as the tree sees it: its full path (also the key
used when the caller is told it was selected) and why it's in the PR.
-}
type alias TreeFile =
    { path : String
    , status : GithubFileStatus
    }


{-| A directory's children are never empty by construction — `build`
only creates a directory when at least one file lives under it, so an
empty folder isn't representable. The directory name may be a
compressed chain (e.g. `"src/elm"`) when every level in the chain had
exactly one child directory and no files.
-}
type FileTree
    = Directory String (List FileTree)
    | Leaf TreeFile



-- TREE SHAPING


{-| Arrange flat file paths into a nested tree: directories first (in
alphabetical order), then files (likewise), single-child directory
chains compressed into one node — the same presentation GitHub's file
tree uses.
-}
build : List TreeFile -> List FileTree
build files =
    files
        |> List.map (\file -> ( String.split "/" file.path, file ))
        |> buildLevel


buildLevel : List ( List String, TreeFile ) -> List FileTree
buildLevel entries =
    let
        leaves =
            List.filterMap
                (\( segments, file ) ->
                    case segments of
                        [ _ ] ->
                            Just (Leaf file)

                        _ ->
                            Nothing
                )
                entries

        directories =
            entries
                |> List.filterMap
                    (\( segments, file ) ->
                        case segments of
                            first :: rest ->
                                if List.isEmpty rest then
                                    Nothing

                                else
                                    Just ( first, ( rest, file ) )

                            [] ->
                                Nothing
                    )
                |> gatherByFirst
                |> List.map
                    (\( name, children ) ->
                        compressChain (Directory name (buildLevel children))
                    )
    in
    List.sortBy nodeSortKey directories ++ List.sortBy nodeSortKey leaves


{-| Group (name, entry) pairs by name, preserving each group's entry
order. Insertion-ordered by first appearance of each name — the final
alphabetical ordering happens in `buildLevel`.
-}
gatherByFirst : List ( String, a ) -> List ( String, List a )
gatherByFirst pairs =
    List.foldr
        (\( name, entry ) groups ->
            case groups of
                ( headName, headEntries ) :: rest ->
                    if headName == name then
                        ( headName, entry :: headEntries ) :: rest

                    else
                        ( name, [ entry ] ) :: groups

                [] ->
                    [ ( name, [ entry ] ) ]
        )
        []
        (List.sortBy Tuple.first pairs)


{-| Collapse `a/` containing only `b/` into a single `a/b` node,
repeatedly, so deep lonely paths don't waste indentation.
-}
compressChain : FileTree -> FileTree
compressChain node =
    case node of
        Directory name [ Directory childName grandchildren ] ->
            compressChain (Directory (name ++ "/" ++ childName) grandchildren)

        _ ->
            node


nodeSortKey : FileTree -> String
nodeSortKey node =
    case node of
        Directory name _ ->
            String.toLower name

        Leaf file ->
            String.toLower (fileName file.path)


fileName : String -> String
fileName path =
    String.split "/" path
        |> List.reverse
        |> List.head
        |> Maybe.withDefault path


{-| Keep the files whose full path contains the query,
case-insensitively. A blank query keeps everything.
-}
filter : String -> List TreeFile -> List TreeFile
filter query files =
    let
        needle =
            String.toLower (String.trim query)
    in
    if String.isEmpty needle then
        files

    else
        List.filter (\file -> String.contains needle (String.toLower file.path)) files


{-| The file's extension (lowercased, without the dot), or "" when it
has none. Only the last dot counts: `a.test.js` is a `js` file.
-}
extensionOf : String -> String
extensionOf path =
    case String.split "." (fileName path) of
        [ _ ] ->
            ""

        segments ->
            segments
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""
                |> String.toLower



-- VIEW


type alias Config msg =
    { filterText : String
    , onFilterChanged : String -> msg
    , onFileSelected : String -> msg
    }


view : Config msg -> List TreeFile -> Html msg
view config files =
    let
        visible =
            filter config.filterText files
    in
    div [ Attr.class "file-tree" ]
        [ input
            [ Attr.type_ "search"
            , Attr.placeholder "Filter files"
            , Attr.value config.filterText
            , Html.Events.onInput config.onFilterChanged
            , Attr.class "file-tree-filter"
            ]
            []
        , if List.isEmpty visible then
            div [ Attr.class "file-tree-empty" ] [ text "No files match" ]

          else
            div [] (List.map (viewNode config 0) (build visible))
        ]


viewNode : Config msg -> Int -> FileTree -> Html msg
viewNode config depth node =
    case node of
        Directory name children ->
            div []
                (div
                    [ Attr.class "tree-dir"
                    , Attr.style "padding-left" (indent depth)
                    ]
                    [ span [ Attr.class "tree-dir-icon" ] [ text "▸" ]
                    , text name
                    ]
                    :: List.map (viewNode config (depth + 1)) children
                )

        Leaf file ->
            button
                [ Html.Events.onClick (config.onFileSelected file.path)
                , Attr.class "btn tree-file"
                , Attr.style "padding-left" (indent depth)
                , Attr.title file.path
                ]
                [ viewStatusIcon file.status
                , viewExtensionChip file.path
                , span [ Attr.class "tree-file-name" ] [ text (fileName file.path) ]
                ]


indent : Int -> String
indent depth =
    String.fromInt (6 + depth * 14) ++ "px"


{-| Why the file is in the PR, as a compact colored glyph: `+` added,
`●` modified, `−` deleted, `→` moved/renamed, `⧉` copied.
-}
viewStatusIcon : GithubFileStatus -> Html msg
viewStatusIcon status =
    let
        ( glyph, statusClass, label ) =
            case status of
                GithubFileAdded ->
                    ( "+", "added", "added" )

                GithubFileModified ->
                    ( "●", "modified", "modified" )

                GithubFileRemoved ->
                    ( "−", "removed", "deleted" )

                GithubFileRenamed ->
                    ( "→", "renamed", "moved" )

                GithubFileCopied ->
                    ( "⧉", "copied", "copied" )
    in
    span
        [ Attr.class ("tree-status tree-status-" ++ statusClass)
        , Attr.title label
        ]
        [ text glyph ]


viewExtensionChip : String -> Html msg
viewExtensionChip path =
    case extensionOf path of
        "" ->
            span [ Attr.class "tree-ext tree-ext-none" ] [ text "·" ]

        extension ->
            span [ Attr.class ("tree-ext tree-ext-" ++ extension) ] [ text extension ]
