module Navigator exposing (..)

{-| -}

import Dict
import Element as Ui
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import Html.Attributes
import Ports
import Ui


view : List Ports.CallGraphNode -> Ui.Element msg
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
                            if List.member call.callType [ Ports.TopLevel ] then
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
        |> String.split "."
        |> List.reverse
        |> List.take 1
        |> String.join ""
        |> sanitizeFnName


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


toMermaid : List Ports.CallGraphNode -> String
toMermaid nodes =
    let
        -- idDict =
        --     toIdDict nodes
        toSimpleId id =
            -- Dict.get id idDict
            --     |> Maybe.withDefault id
            idToFnName id

        nodeToMermaid node =
            let
                calls =
                    node.calls
                        |> List.filterMap
                            (\call ->
                                case call.callType of
                                    Ports.TopLevel ->
                                        Just (toSimpleId call.id)

                                    _ ->
                                        Nothing
                            )
            in
            case calls of
                [] ->
                    []

                _ ->
                    calls
                        |> List.map
                            (\call ->
                                toSimpleId node.id ++ " --> " ++ toSimpleId call
                            )
    in
    "stateDiagram-v2\n    " ++ String.join "\n    " (List.concatMap nodeToMermaid nodes)
