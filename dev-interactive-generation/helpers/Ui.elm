module Ui exposing (card, code, divider, lightCode, pointer)

{-| -}

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Html exposing (Html)
import Html.Attributes as Attr
import SyntaxHighlight exposing (elm, monokai, toBlockHtml, useTheme)


rgb r g b =
    Element.rgb (r / 255) (g / 255) (b / 255)


divider : Element.Element msg
divider =
    Element.el
        [ Background.color (Element.rgb 0.2 0.2 0.2)
        , Element.width Element.fill
        , Element.height (Element.px 1)
        ]
        Element.none


lightDivider : Element.Element msg
lightDivider =
    Element.el
        [ Background.color (rgb 231 234 236)
        , Element.width Element.fill
        , Element.height (Element.px 1)
        ]
        Element.none



-- 231	234	236


pointer : Element.Attribute msg
pointer =
    Element.htmlAttribute (Attr.style "cursor" "pointer")


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
        , Element.htmlAttribute (Attr.style "white-space" "pre-wrap")

        -- , Element.above
        --     (Element.text label)
        ]
        (Element.html
            (SyntaxHighlight.elm content
                |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
                |> Result.withDefault
                    (Html.pre [] [ Html.code [] [ Html.text content ] ])
            )
        )


lightCode : String -> Element.Element msg
lightCode content =
    Element.el
        [ Element.width Element.fill
        , Element.padding 20
        , Element.height
            (Element.shrink
                |> Element.minimum 200
            )
        , Font.family
            [ Font.typeface "Fira Code"
            , Font.sansSerif
            ]
        , Border.width 1
        , Border.rounded 6
        , Border.color (Element.rgb 0.2 0.2 0.2)
        , Element.htmlAttribute (Attr.style "line-height" "1.2")
        , Element.htmlAttribute (Attr.style "background" "#1a1a1a")
        , Element.htmlAttribute (Attr.style "white-space" "pre-wrap")
        ]
        (Element.html
            (SyntaxHighlight.elm content
                |> Result.map (SyntaxHighlight.toBlockHtml (Just 1))
                |> Result.withDefault
                    (Html.pre [] [ Html.code [] [ Html.text content ] ])
            )
            |> Element.el [ Element.centerY ]
        )


card : List (Html msg) -> Html msg
card children =
    Html.div [] children
