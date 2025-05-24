module Ui exposing (column, row)

{-| -}

import Html exposing (Html)
import Html.Attributes as Attr


column : List (Html.Attribute msg) -> List (Html msg) -> Html msg
column attrs children =
    Html.div (Attr.class "column" :: attrs) children


row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
row attrs children =
    Html.div (Attr.class "row" :: attrs) children
