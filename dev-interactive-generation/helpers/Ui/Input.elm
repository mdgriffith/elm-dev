module Ui.Input exposing
    ( bool, int, string
    , maybeString, maybeBool
    )

{-|

@docs bool, int, string

@docs maybeString, maybeBool

-}

import Element
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input
import Html exposing (Html)


{-| -}
maybeString : String -> (Maybe String -> msg) -> Maybe String -> Element.Element msg
maybeString label onChange maybeText =
    Element.row []
        [ string label
            (Just >> onChange)
            (Maybe.withDefault "" maybeText)
        , case maybeText of
            Nothing ->
                Element.none

            Just _ ->
                cancelMaybe (onChange Nothing)
        ]


{-| -}
maybeBool : String -> (Maybe Bool -> msg) -> Maybe Bool -> Element.Element msg
maybeBool label onChange maybeChecked =
    Element.row []
        [ bool label
            (Just >> onChange)
            (Maybe.withDefault False maybeChecked)
        , case maybeChecked of
            Nothing ->
                Element.none

            Just _ ->
                cancelMaybe (onChange Nothing)
        ]


cancelMaybe onCancel =
    Element.el
        [ Font.color (Element.rgb 0.2 0.2 0.2)
        , Element.Events.onClick onCancel
        ]
        (Element.text "x")


{-| -}
string : String -> (String -> msg) -> String -> Element.Element msg
string label onChange text =
    Element.Input.multiline
        [ Font.color (Element.rgb 0 0 0)
        ]
        { onChange = onChange
        , text = text
        , placeholder = Nothing
        , spellcheck = False
        , label =
            Element.Input.labelHidden
                label
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
