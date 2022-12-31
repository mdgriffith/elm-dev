module Navigator exposing (..)

{-| -}

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
    Ui.column
        [ Ui.width Ui.fill
        , Ui.height (Ui.px 1000)
        , Ui.htmlAttribute (Html.Attributes.style "overflow" "auto")
        ]
        (List.map
            viewNode
            nodes
        )


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
