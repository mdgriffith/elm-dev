module Theme.Generate exposing (..)

import Elm
import Theme
import Theme.Generate.Ui


generate : Theme.Theme -> List Elm.File
generate theme =
    Theme.Generate.Ui.generate theme
