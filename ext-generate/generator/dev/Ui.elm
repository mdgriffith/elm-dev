module Ui exposing
    ( alignRight
    , annotation_
    , background
    , border
    , borderColor
    , column
    , el
    , fill
    , fontColor
    , fontFamily
    , fontSize
    , height
    , htmlAttribute
    , layers
    , layout
    , move
    , none
    , padding
    , paddingXY
    , pointer
    , rgb
    , right
    , rounded
    , row
    , spacing
    , text
    , width
    )

{-| This module is a wrapper around the Gen.Html module.

It is used to maintain some sort of sanity when generating the theme stuff.

-}

import Elm
import Elm.Annotation
import Gen.Html
import Gen.Html.Attributes as Attr



-- Layout functions


row : List Elm.Expression -> List Elm.Expression -> Elm.Expression
row attrs children =
    Gen.Html.div
        (List.append
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "row"
            ]
            attrs
        )
        children


column : List Elm.Expression -> List Elm.Expression -> Elm.Expression
column attrs children =
    Gen.Html.div
        (List.append
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            ]
            attrs
        )
        children


el : List Elm.Expression -> Elm.Expression -> Elm.Expression
el attrs child =
    Gen.Html.div attrs [ child ]


text : String -> Elm.Expression
text str =
    Gen.Html.text str


layout : List Elm.Expression -> Elm.Expression -> Elm.Expression
layout attrs child =
    Gen.Html.div attrs [ child ]


none : Elm.Expression
none =
    Gen.Html.text ""



-- Spacing and sizing


spacing : Int -> Elm.Expression
spacing value =
    Attr.style "gap" (String.fromInt value ++ "px")


padding : Int -> Elm.Expression
padding value =
    Attr.style "padding" (String.fromInt value ++ "px")


paddingXY : Int -> Int -> Elm.Expression
paddingXY x y =
    Attr.style "padding" (String.fromInt x ++ "px " ++ String.fromInt y ++ "px")


height : String -> Elm.Expression
height value =
    Attr.style "height" value


width : String -> Elm.Expression
width value =
    Attr.style "width" value


fill : String
fill =
    "100%"



-- Alignment and positioning


alignRight : Elm.Expression
alignRight =
    Attr.style "justify-content" "flex-end"


pointer : Elm.Expression
pointer =
    Attr.style "cursor" "pointer"



-- Borders and styling


border : Int -> Elm.Expression
border px =
    Attr.style "border" (String.fromInt px ++ "px solid")


rounded : Int -> Elm.Expression
rounded radius =
    Attr.style "border-radius" (String.fromInt radius ++ "px")


borderColor : String -> Elm.Expression
borderColor color =
    Attr.style "border-color" color


background : String -> Elm.Expression
background color =
    Attr.style "background-color" color



-- Colors


rgb : Float -> Float -> Float -> Elm.Expression
rgb r g b =
    Elm.string ("rgb(" ++ String.fromFloat r ++ "," ++ String.fromFloat g ++ "," ++ String.fromFloat b ++ ")")



-- Positioning


type alias Xy =
    { x : Int
    , y : Int
    }


move : Xy -> Elm.Expression
move value =
    Attr.style "transform" ("translate(" ++ String.fromInt value.x ++ "px, " ++ String.fromInt value.y ++ "px)")


right : Int -> Xy
right value =
    { x = value, y = 0 }


layers : List Elm.Expression -> List Elm.Expression -> Elm.Expression
layers attrs children =
    Gen.Html.div
        (List.append
            [ Attr.style "position" "relative"
            , Attr.style "display" "grid"
            , Attr.style "grid-template-columns" "1fr"
            , Attr.style "grid-template-rows" "1fr"
            ]
            attrs
        )
        (List.map
            (\child ->
                Gen.Html.div
                    [ Attr.style "grid-column" "1"
                    , Attr.style "grid-row" "1"
                    ]
                    [ child ]
            )
            children
        )



-- below : Elm.Expression -> Elm.Expression
-- below child =
--     Attr.style "position" "relative"
--         |> Elm.apply
--             (Elm.apply
--                 (Elm.value
--                     { importFrom = []
--                     , name = "andThen"
--                     , annotation = Nothing
--                     }
--                 )
--                 [ child ]
--             )
-- inFront : Elm.Expression -> Elm.Expression
-- inFront child =
--     Attr.style "position" "relative"
--         |> Elm.apply
--             (Elm.apply
--                 (Elm.value
--                     { importFrom = []
--                     , name = "andThen"
--                     , annotation = Nothing
--                     }
--                 )
--                 [ child ]
--             )


htmlAttribute : Elm.Expression -> Elm.Expression
htmlAttribute attr =
    attr



-- Annotations


annotation_ : { element : Elm.Annotation.Annotation -> Elm.Annotation.Annotation }
annotation_ =
    { element = Gen.Html.annotation_.html
    }


fontSize : Int -> Elm.Expression
fontSize size =
    Attr.style "font-size" (String.fromInt size ++ "px")


fontColor : Elm.Expression -> Elm.Expression
fontColor color =
    Attr.call_.style (Elm.string "color") color


fontFamily : List String -> Elm.Expression
fontFamily fonts =
    Attr.style "font-family" (String.join ", " (List.map fontQuote fonts))


fontQuote : String -> String
fontQuote str =
    if str == "sans-serif" then
        str

    else
        "\"" ++ str ++ "\""
