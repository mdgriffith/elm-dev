module Ui.Card exposing (view)

{-| -}

import Element as Ui
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import Ui


view :
    { title : String
    , hint : Maybe String
    , highlight : Bool
    , onClick : Maybe msg
    }
    -> List (Ui.Element msg)
    -> Ui.Element msg
view options content =
    Ui.column
        [ Ui.width Ui.fill
        , Ui.space.md
        , Ui.pad.xl
        , Ui.rounded.md
        , Ui.background.dark
        , if options.highlight then
            Ui.border.dark.light

          else
            Ui.border.dark.medium
        , Border.width 1
        , Ui.maybeAttr (Maybe.map Events.onClick options.onClick)
        , Ui.maybeAttr (Maybe.map (\_ -> Ui.pointer) options.onClick)
        ]
        (viewHeader options :: content)


viewHeader : { a | hint : Maybe String, title : String } -> Ui.Element msg
viewHeader options =
    Ui.row [ Ui.space.md, Ui.width Ui.fill ]
        [ Ui.el []
            (Ui.text (String.trim options.title))
        , Ui.whenJust options.hint <|
            \text ->
                Ui.el
                    [ Ui.font.dark.light
                    , Ui.alignRight
                    ]
                    (Ui.text text)
        ]
