module Ui.WindowHeader exposing (..)

{- https://tauri.app/v1/guides/features/window-customization/ -}

import Element as Ui exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Flags
import Html
import Html.Attributes as Attr


view : Ui.Element msg
view =
    Ui.html
        (Html.div
            [ Attr.attribute "data-tauri-drag-region" ""
            , Attr.class "titlebar"
            , Attr.style "height" "30px"
            ]
            [ Html.div
                [ Attr.class "titlebar-button"
                , Attr.id "titlebar-minimize"
                ]
                [ Html.img
                    [ Attr.src "https://api.iconify.design/mdi:window-minimize.svg"
                    , Attr.alt "minimize"
                    ]
                    []
                ]
            , Html.div
                [ Attr.class "titlebar-button"
                , Attr.id "titlebar-maximize"
                ]
                [ Html.img
                    [ Attr.src "https://api.iconify.design/mdi:window-maximize.svg"
                    , Attr.alt "maximize"
                    ]
                    []
                ]
            , Html.div
                [ Attr.class "titlebar-button"
                , Attr.id "titlebar-close"
                ]
                [ Html.img
                    [ Attr.src "https://api.iconify.design/mdi:close.svg"
                    , Attr.alt "close"
                    ]
                    []
                ]
            ]
        )
