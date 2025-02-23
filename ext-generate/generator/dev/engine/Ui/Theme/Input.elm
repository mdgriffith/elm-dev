module Ui.Theme.Input exposing
    ( bool, int, string
    , maybeString, maybeBool
    )

{-|

@docs bool, int, string

@docs maybeString, maybeBool

-}

import Html exposing (Html)
import Ui
import Ui.Events
import Ui.Font as Font
import Ui.Input


{-| -}
maybeString : String -> (Maybe String -> msg) -> Maybe String -> Ui.Ui msg
maybeString label onChange maybeText =
    Ui.row []
        [ string label
            (Just >> onChange)
            (Maybe.withDefault "" maybeText)
        , case maybeText of
            Nothing ->
                Ui.none

            Just _ ->
                cancelMaybe (onChange Nothing)
        ]


{-| -}
maybeBool : String -> (Maybe Bool -> msg) -> Maybe Bool -> Ui.Ui msg
maybeBool label onChange maybeChecked =
    Ui.row []
        [ bool label
            (Just >> onChange)
            (Maybe.withDefault False maybeChecked)
        , case maybeChecked of
            Nothing ->
                Ui.none

            Just _ ->
                cancelMaybe (onChange Nothing)
        ]


cancelMaybe onCancel =
    Ui.el
        [ Font.color (Ui.rgb 0.2 0.2 0.2)
        , Ui.Events.onClick onCancel
        ]
        (Ui.text "x")


{-| -}
string : String -> (String -> msg) -> String -> Ui.Ui msg
string label onChange text =
    Ui.Input.multiline
        [ Font.color (Ui.rgb 0 0 0)
        ]
        { onChange = onChange
        , text = text
        , placeholder = Nothing
        , spellcheck = False
        , label =
            Ui.Input.labelHidden
                label
        }


{-| -}
bool : String -> (Bool -> msg) -> Bool -> Ui.Ui msg
bool label onChange checked =
    Ui.Input.checkbox []
        { onChange = onChange
        , icon = Ui.Input.defaultCheckbox
        , checked = checked
        , label =
            Ui.Input.labelRight []
                (Ui.text label)
        }


{-| -}
int : String -> (Int -> msg) -> Int -> Ui.Ui msg
int label onChange i =
    Ui.Input.text []
        { onChange =
            \str ->
                onChange (Maybe.withDefault 0 (String.toInt str))
        , text = String.fromInt i
        , placeholder = Nothing
        , label =
            Ui.Input.labelAbove []
                (Ui.text label)
        }
