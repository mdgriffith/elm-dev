module Ui.Input exposing (bool, int, string)

{-| -}

import Element
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Html exposing (Html)


{-| -}
string : String -> (String -> msg) -> String -> Element.Element msg
string label onChange text =
    Element.Input.multiline [ Font.color (Element.rgb 0 0 0) ]
        { onChange = onChange
        , text = text
        , placeholder = Nothing
        , spellcheck = False
        , label =
            Element.Input.labelAbove []
                (Element.text label)
        }


{-| -}
bool : String -> (Bool -> msg) -> Bool -> Element.Element msg
bool label onChange checked =
    Element.Input.checkbox []
        { onChange = onChange
        , icon = Element.Input.defaultCheckbox
        , checked = checked
        , label =
            Element.Input.labelRight []
                (Element.text label)
        }


{-| -}
int : String -> (Int -> msg) -> Int -> Element.Element msg
int label onChange i =
    Element.Input.text []
        { onChange =
            \str ->
                onChange (Maybe.withDefault 0 (String.toInt str))
        , text = String.fromInt i
        , placeholder = Nothing
        , label =
            Element.Input.labelAbove []
                (Element.text label)
        }
