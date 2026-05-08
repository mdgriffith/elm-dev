module Ui exposing
    ( Attribute, Element
    , el, column, row
    , htmlAttribute, spacing
    , padding, paddingXY, paddingTop, paddingBottom, paddingLeft, paddingRight
    , border
    )

{-|

@docs Attribute, Element

@docs el, column, row
@docs htmlAttribute, row, spacing

@docs padding, paddingXY, paddingTop, paddingBottom, paddingLeft, paddingRight

@docs border

-}

import Html exposing (Html)
import Html.Attributes as Attr


type alias Attribute msg =
    Html.Attribute msg


type alias Element msg =
    Html msg


el : List (Attribute msg) -> Element msg -> Element msg
el attrs child =
    Html.div attrs [ child ]


spacing : Int -> Attribute msg
spacing n =
    Attr.style "gap" (String.fromInt n ++ "px")


padding : Int -> Attribute msg
padding n =
    Attr.style "padding" (String.fromInt n ++ "px")


paddingXY : Int -> Int -> Attribute msg
paddingXY x y =
    Attr.style "padding" (String.fromInt x ++ "px " ++ String.fromInt y ++ "px")


paddingTop : Int -> Attribute msg
paddingTop n =
    Attr.style "padding-top" (String.fromInt n ++ "px")


paddingBottom : Int -> Attribute msg
paddingBottom n =
    Attr.style "padding-bottom" (String.fromInt n ++ "px")


paddingLeft : Int -> Attribute msg
paddingLeft n =
    Attr.style "padding-left" (String.fromInt n ++ "px")


paddingRight : Int -> Attribute msg
paddingRight n =
    Attr.style "padding-right" (String.fromInt n ++ "px")


border : Int -> Attribute msg
border n =
    Attr.style "border" (String.fromInt n ++ "px")


htmlAttribute : Html.Attribute msg -> Attribute msg
htmlAttribute =
    identity


column : List (Html.Attribute msg) -> List (Html msg) -> Html msg
column attrs children =
    Html.div (Attr.class "column" :: attrs) children


row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    Html.div (Attr.class "row" :: attrs) children

