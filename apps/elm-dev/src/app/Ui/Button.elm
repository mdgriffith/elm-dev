module Ui.Button exposing (..)

import Html
import Html.Attributes


text : String
text =
    "Button"


view : String -> Html.Html msg
view string =
    Html.button [] [ Html.text string ]
