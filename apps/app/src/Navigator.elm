module Navigator exposing (..)

{-| -}

import Element as Ui
import Elm.CallGraph
import Ports
import Ui


view : List Elm.CallGraph.Node -> Ui.Element msg
view nodes =
    -- Ui.column
    --     [ Ui.width Ui.fill
    --     , Ui.height (Ui.px 1000)
    --     , Ui.htmlAttribute (Html.Attributes.style "overflow" "auto")
    --     ]
    --     (List.map
    --         viewNode
    --         nodes
    --     )
    Ui.text (toMermaid nodes)


viewNode node =
    Ui.column
        [ Ui.pad.lg
        , Ui.font.dark.light
        , Ui.background.black
        , Ui.rounded.md
        , Ui.width (Ui.px 500)
        ]
        [ Ui.text node.id
        , let
            calls =
                node.calls
                    |> List.filterMap
                        (\call ->
                            if List.member call.callType [ Elm.CallGraph.TopLevel ] then
                                Just (Ui.text call.id)

                            else
                                Nothing
                        )
          in
          case calls of
            [] ->
                Ui.none

            _ ->
                Ui.column [ Ui.pad.lg ] calls
        ]


idToFnName : String -> String
idToFnName id =
    id



-- |> String.split "."
-- |> List.reverse
-- |> List.take 1
-- |> String.join ""
-- |> sanitizeFnName


sanitizeFnName : String -> String
sanitizeFnName name =
    name
        |> String.replace "note" "noote"
        |> String.replace "state" "staate"



-- toIdDict : List Ports.CallGraphNode -> Dict.Dict String String
-- toIdDict nodes =
--     let
--         nodeToIdDict node =
--             let
--                 calls =
--                     node.calls
--                         |> List.filterMap
--                             (\call ->
--                                 if List.member call.callType [ Ports.TopLevel ] then
--                                     Just call.id
--                                 else
--                                     Nothing
--                             )
--             in
--             ( node.id, String.join ", " calls )
--     in
--     nodes
--         |> List.map nodeToIdDict
--         |> Dict.fromList


toMermaid : List Elm.CallGraph.Node -> String
toMermaid nodes =
    let
        toSimpleId id =
            idToFnName id

        nodeToMermaid node =
            node.calls
                |> List.map
                    (\call ->
                        toSimpleId node.id ++ " --> " ++ toSimpleId call.id ++ parens (callTypeToString call.callType)
                    )
    in
    "stateDiagram-v2\n    " ++ String.join "\n    " (List.concatMap nodeToMermaid nodes)


parens : String -> String
parens s =
    " (" ++ s ++ ")"


{-| type CallType
= Local
| TopLevel
| Foreign
| Constructor
| Debug
| Operator
-}
callTypeToString : Elm.CallGraph.CallType -> String
callTypeToString callType =
    case callType of
        Elm.CallGraph.Local ->
            "local"

        Elm.CallGraph.TopLevel ->
            "top-level"

        Elm.CallGraph.Foreign ->
            "foreign"

        Elm.CallGraph.Constructor ->
            "constructor"

        Elm.CallGraph.Debug ->
            "debug"

        Elm.CallGraph.Operator ->
            "operator"
