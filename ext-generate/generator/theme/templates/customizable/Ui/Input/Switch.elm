module Ui.Input.Switch exposing
    ( Switch, switch
    , view
    )

{-|

@docs Switch, switch

@docs view

-}

import Ui
import Ui.Input
import Ui.Theme
import Ui.Theme.Palette


type Switch msg
    = Switch (Details msg)


type alias Details msg =
    { label : String
    , value : Bool
    , onToggle : Bool -> msg
    }


{-| -}
switch :
    { onToggle : Bool -> msg
    , value : Bool
    }
    -> Switch msg
switch options =
    Switch
        { label = Nothing
        , value = options.value
        , onToggle = options.onToggle
        }


{-| -}
view : Switch msg -> Ui.Element msg
view (Switch options) =
    Ui.Input.checkbox []
        { onChange = options.onToggle
        , icon = Just (viewToggle options)
        , checked = options.value
        , label =
            Ui.Input.labelRight []
                (Ui.text optinos.label)
        }


viewToggle : Details msg -> Bool -> Ui.Element msg
viewToggle details on =
    Ui.el
        (List.concat
            [ [ Ui.width (Ui.px 36)
              , Ui.height (Ui.px 16)
              , Ui.circle
              , Ui.Theme.padding.sm4

              --   , Ui.disabledIf config.isDisabled
              --   , Ui.Transition.create
              --         [ Ui.Transition.FontColor
              --         , Ui.Transition.BackgroundColor
              --         ]
              ]
            , -- if config.isDisabled then
              --     if on then
              --         [ Ui.backgroundColor.primaryHighlight
              --         , Ui.fontColor.greyscale600
              --         ]
              --     else
              --         [ Ui.backgroundColor.greyscale200
              --         , Ui.fontColor.greyscale500
              --         ]
              --   else
              if on then
                -- [ Ui.backgroundColor.primary
                -- , Ui.fontColor.greyscale0
                -- , Ui.hover.backgroundColor.primaryHover
                -- , Ui.active.backgroundColor.primaryActive
                -- ]
                Ui.Theme.Palette.primary

              else
                -- [ Ui.backgroundColor.greyscale300
                -- , Ui.fontColor.greyscale800
                -- , Ui.hover.backgroundColor.greyscale400
                -- , Ui.active.backgroundColor.greyscale500
                -- ]
                Ui.Theme.Palette.inactive
            ]
        )
        (Ui.el
            [ Ui.Theme.Palette.secondary

            -- , Ui.Theme.border.small
            , Ui.width (Ui.px 20)
            , Ui.height (Ui.px 20)
            , Ui.circle
            , Ui.move
                (if config.value then
                    Ui.right 16

                 else
                    Ui.right 0
                )

            -- , Ui.Transition.create [ Ui.Transition.Transform ]
            ]
            Ui.none
        )
