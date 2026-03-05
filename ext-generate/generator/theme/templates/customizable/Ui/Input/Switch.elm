module Ui.Input.Switch exposing
    ( Switch, switch
    , view
    )

{-|

@docs Switch, switch
@docs view

-}

import Theme
import Theme.Color
import Theme.Color.Palette as Palette
import Ui
import Ui.Input


type Switch msg
    = Switch (Details msg)


type alias Details msg =
    { label : String
    , value : Bool
    , onToggle : Bool -> msg
    }


switch :
    { onToggle : Bool -> msg
    , value : Bool
    }
    -> Switch msg
switch options =
    Switch
        { label = ""
        , value = options.value
        , onToggle = options.onToggle
        }


view : Switch msg -> Ui.Element msg
view (Switch options) =
    Ui.Input.checkbox []
        { onChange = options.onToggle
        , icon = Just (viewToggle options)
        , checked = options.value
        , label =
            Ui.Input.labelRight []
                (Ui.text options.label)
        }


viewToggle : Details msg -> Bool -> Ui.Element msg
viewToggle details on =
    Ui.el
        ([ Ui.width (Ui.px 36)
         , Ui.height (Ui.px 16)
         , Ui.circle
         , Theme.pad 1
         ]
            ++ (if on then
                    [ Theme.Color.backgroundPrimary ]

                else
                    [ Ui.background Palette.neutral70 ]
               )
        )
        (Ui.el
            [ Theme.Color.backgroundSurface
            , Theme.Color.borderDefault
            , Theme.borderWidth 1
            , Ui.width (Ui.px 20)
            , Ui.height (Ui.px 20)
            , Ui.circle
            , Ui.move
                (if details.value then
                    Ui.right 16

                 else
                    Ui.right 0
                )
            ]
            Ui.none
        )
