module Ui.Tooltip exposing (..)

{-| -}

import Ui
import Ui.Theme
import Ui.Theme.Palette
import WebComponents.Portal as Portal


{-| -}
tooltip : String -> Ui.Attribute msg
tooltip label =
    Ui.above
        (Ui.el [ Ui.Theme.Palette.neutralInverted ]
            (Ui.text label)
        )
