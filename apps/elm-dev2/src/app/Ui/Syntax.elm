module Ui.Syntax exposing (field, keyword, punctuation, type_, typevar)

import Html
import Html.Attributes as Attr


typevar : Html.Attribute msg
typevar =
    Attr.class "typevar"


punctuation : Html.Attribute msg
punctuation =
    Attr.class "punctuation"


type_ : Html.Attribute msg
type_ =
    Attr.class "type"


field : Html.Attribute msg
field =
    Attr.class "field"


keyword : Html.Attribute msg
keyword =
    Attr.class "keyword"
