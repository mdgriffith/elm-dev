module Ui.Tooltip exposing (..)

{-| -}

import Ui
import Theme
import Theme.Color


{-| -}
tooltip : String -> Ui.Attribute msg
tooltip label =
    Ui.above
        (Ui.el
            [ Theme.Color.backgroundSurface
            , Theme.Color.textDefault
            , Theme.Color.borderDefault
            , Theme.borderWidth 1
            , Theme.borderRadius.sm
            , Theme.pad 1
            ]
            (Ui.text label)
        )
