module Ui.Modal exposing
    ( Modal, modal
    , withCloseOnBackdropClick
    , view
    )

{-|

@docs Modal, modal

@docs withCloseOnBackdropClick

@docs view

-}

import Ui
import Ui.Input
import Ui.Theme


type Modal msg
    = Modal (Details msg)


type alias Details msg =
    { visible : Bool
    , closeOnClickBackdrop : Maybe (Bool -> msg)
    , content : Ui.Element msg
    }


{-| -}
modal :
    { visible : Bool
    , content : Ui.Element msg
    }
    -> Switch msg
modal options =
    Modal
        { visible = options.visible
        , content = options.content
        , closeOnClickBackdrop = Nothing
        }


withCloseOnBackdropClick : (Bool -> msg) -> Modal msg -> Modal msg
withCloseOnBackdropClick closeOnClickBackdrop (Modal details) =
    Modal
        { details
            | closeOnClickBackdrop = Just closeOnClickBackdrop
        }


{-| -}
view : Modal msg -> Ui.Element msg
view (Modal options) =
    Ui.el
        [ Ui.width Ui.fill
        , Ui.height Ui.fill
        , Ui.Theme.background.backdrop
        , case options.closeOnClickBackdrop of
            Nothing ->
                Ui.noAttr

            Just msg ->
                -- TODO: Ui.clickOnThisElement?
                Ui.onClick msg
        ]
        (Ui.el
            [ Ui.width (Ui.px 800)
            , Ui.height (Ui.px 600)
            , Ui.centerX
            , Ui.centerY
            ]
            options.content
        )
