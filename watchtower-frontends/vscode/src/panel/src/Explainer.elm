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
            (List.reverse facts)
        )


{-| -}
viewFact : Ports.Fact -> Ui.Element msg
viewFact fact =
    case fact of
        Ports.Value value ->
            Ui.column
                [ Ui.pad.lg
                , Ui.font.dark.light
                , Ui.background.black
                , Ui.rounded.md
                , Ui.width (Ui.px 500)
                ]
                [ viewModuleName value.source
                , Ui.text (prefixModule value.source value.name ++ " : " ++ value.type_)
                ]

        Ports.Union union ->
            Ui.column
                [ Ui.pad.lg
                , Ui.font.info
                , Ui.background.black
                , Ui.rounded.md
                ]
                [ Ui.el [] (Ui.text ("type " ++ union.name ++ " ="))
                , Ui.column
                    [ Ui.pad.md
                    , Ui.space.sm
                    ]
                    (List.map
                        viewUnionCase
                        union.cases
                    )
                , viewMaybe union.comment <|
                    \comment ->
                        Ui.el [ Ui.pad.md ] (Ui.text comment)
                ]

        Ports.Alias alias_ ->
            Ui.column
                [ Ui.pad.lg
                , Ui.font.dark.light
                , Ui.background.black
                , Ui.rounded.md
                , Ui.width (Ui.px 500)
                ]
                [ Ui.el [] (Ui.text ("type alias " ++ alias_.name ++ " ="))
                , Ui.el [ Ui.pad.md ] (Ui.text alias_.type_)
                , viewMaybe alias_.comment <|
                    \comment ->
                        Ui.el [ Ui.pad.md ] (Ui.text comment)
                ]

        Ports.Def def ->
            Ui.column
                [ Ui.pad.lg
                , Ui.font.dark.light
                , Ui.background.black
                , Ui.rounded.md
                , Ui.width (Ui.px 500)
                ]
                [ Ui.el [] (Ui.text def.name)
                , viewMaybe def.type_ <|
                    \type_ ->
                        Ui.el [] (Ui.text type_)
                , Ui.el [ Ui.pad.md ] (Ui.text def.comment)
                ]


viewMaybe maybe viewer =
    case maybe of
        Nothing ->
            Ui.none

        Just val ->
            viewer val


viewUnionCase variant =
    Ui.row
        []
        [ Ui.text "| "
        , Ui.text variant.name
        , Ui.text " "
        , Ui.row [ Ui.space.md ]
            (List.map Ui.text variant.types_)
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
