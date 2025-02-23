module Ui.Attr exposing
    ( alignTop
    , borderBox
    , ellipsis
    , gap
    , noGrow
    , noShrink
    , none
    , overflowHidden
    , pad
    , padXY
    , scrollbars
    , width
    , widthMax
    )

import Html exposing (Html)
import Html.Attributes as Attr


none : Html.Attribute msg
none =
    Attr.class ""


alignTop : Html.Attribute msg
alignTop =
    -- Attr.style "align-items" "flex-start"
    Attr.class ""


overflowHidden : Html.Attribute msg
overflowHidden =
    Attr.style "overflow" "hidden"


scrollbars : Html.Attribute msg
scrollbars =
    Attr.style "overflow" "auto"


borderBox : Html.Attribute msg
borderBox =
    Attr.style "box-sizing" "border-box"


ellipsis : Html.Attribute msg
ellipsis =
    Attr.class "ellipsis"


noGrow : Html.Attribute msg
noGrow =
    Attr.style "flex-grow" "0"


noShrink : Html.Attribute msg
noShrink =
    Attr.style "flex-shrink" "0"


widthMax : Int -> Html.Attribute msg
widthMax n =
    Attr.style "max-width" (String.fromInt n ++ "px")


width : Int -> Html.Attribute msg
width n =
    Attr.style "width" (String.fromInt n ++ "px")


padXY : Int -> Int -> Html.Attribute msg
padXY x y =
    Attr.style "padding" (String.fromInt y ++ "px " ++ String.fromInt x ++ "px")


pad : Int -> Html.Attribute msg
pad n =
    Attr.style "padding" (String.fromInt n ++ "px")


gap : Int -> Html.Attribute msg
gap n =
    Attr.style "gap" (String.fromInt n ++ "px")
