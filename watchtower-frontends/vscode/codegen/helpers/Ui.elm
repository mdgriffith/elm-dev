module Ui exposing (card, code)

{-| -}

import Element
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Html exposing (Html)
import Html.Attributes as Attr
import SyntaxHighlight exposing (elm, monokai, toBlockHtml, useTheme)


code : String -> String -> Element.Element msg
code label content =
    Element.el
        [ Element.padding 24
        , Element.width Element.fill
        , Border.width 1
        , Border.rounded 6
        , Border.color (Element.rgb 0.2 0.2 0.2)
        , Font.family
            [ Font.typeface "Fira Code"
            , Font.sansSerif
            ]
        , Element.htmlAttribute (Attr.style "background" "#1a1a1a")
        , Element.htmlAttribute (Attr.style "line-height" "1.2")
        , Element.above
            (Element.text label)
        ]
        (Element.html
            (SyntaxHighlight.elm content
                |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
                |> Result.withDefault
                    (Html.pre [] [ Html.code [] [ Html.text content ] ])
            )
        )


card : List (Html msg) -> Html msg
card children =
    Html.div [] children
