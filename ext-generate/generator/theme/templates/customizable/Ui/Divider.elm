module Ui.Divider exposing (horizontal, vertical)

{-| -}

import Ui
import Ui.Theme
import Ui.Theme.Palette


{-| -}
horizontal : Ui.Element msg
horizontal =
    Ui.el
        [ Ui.height (Ui.px 1)
        , Ui.width Ui.fill
        , Ui.Theme.Palette.neutralInverted
        ]
        Ui.none


{-| -}
vertical : Ui.Element msg
vertical =
    Ui.el
        [ Ui.width (Ui.px 1)
        , Ui.height Ui.fill
        , Ui.Theme.Palette.neutralInverted
        ]
        Ui.none
