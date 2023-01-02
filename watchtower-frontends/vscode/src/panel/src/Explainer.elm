module Explainer exposing (..)

{-| -}

import Element as Ui
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import Html.Attributes
import Ports
import Ui


view : List Ports.Fact -> Ui.Element msg
view facts =
    Ui.column
        [ Ui.width Ui.fill
        , Ui.height (Ui.px 1000)
        , Ui.htmlAttribute (Html.Attributes.style "overflow" "auto")
        ]
        (List.map
            viewFact
            facts
        )


{-| -}
viewFact : Ports.Fact -> Ui.Element msg
viewFact fact =
    Ui.column
        [ Ui.pad.lg
        , Ui.font.dark.light
        , Ui.background.black
        , Ui.rounded.md
        , Ui.width (Ui.px 500)
        ]
        [ viewModuleName fact.source
        , Ui.text (prefixModule fact.source fact.name ++ " : " ++ fact.type_)
        ]


prefixModule : Ports.Source -> String -> String
prefixModule source name =
    case source of
        Ports.LocalSource ->
            name

        Ports.External mod ->
            mod.name ++ "." ++ name


viewModuleName : Ports.Source -> Ui.Element msg
viewModuleName source =
    case source of
        Ports.LocalSource ->
            Ui.none

        Ports.External mod ->
            case mod.pkg of
                "author/project" ->
                    Ui.none

                _ ->
                    Ui.el [] (Ui.text mod.pkg)