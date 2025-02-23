module Ui.Dropdown exposing (Visible, init, withMenu)

{-| This is for attaching a dropdown menu
-}

import Ui
import WebComponent.Portal


init : Visible
init =
    Visible WebComponent.Portal.closed


type Visible
    = Visible WebComponent.Portal.Model


withMenu :
    { open : Visible
    , onOpen : Visible -> msg
    , menu : Ui.Element msg
    }
    -> Ui.Element msg
    -> Ui.Element msg
withMenu options root =
    Ui.html <|
        WebComponent.Portal.view
            { position = WebComponent.Portal.ToRightOf
            , model =
                case options.open of
                    Visible portal ->
                        portal
            , onMsg = options.onOpen << Visible
            , button = Ui.embed [] root
            , menu = Ui.embed [] options.menu
            }
