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
import Html.Attributes as Attr
import Theme.Color


type Modal msg
    = Modal (Details msg)


type alias Details msg =
    { visible : Bool
    , closeOnClickBackdrop : Maybe msg
    , content : Ui.Element msg
    }


{-| -}
modal :
    { visible : Bool
    , content : Ui.Element msg
    }
    -> Modal msg
modal options =
    Modal
        { visible = options.visible
        , content = options.content
        , closeOnClickBackdrop = Nothing
        }


withCloseOnBackdropClick : msg -> Modal msg -> Modal msg
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
        , Theme.Color.backgroundCanvas
        , Ui.htmlAttribute (Attr.style "background-color" "rgba(0,0,0,0.45)")
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
