module Ui.WindowHeader exposing (..)

{- https://tauri.app/v1/guides/features/window-customization/ -}

import Element as Ui exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Flags
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Ui


view :
    { onMinimize : msg
    , onMaximize : msg
    , onClose : msg
    , title : Ui.Element msg
    , platform : Flags.Platform
    }
    -> Ui.Element msg
view options =
    if isWindows options.platform then
        Ui.row
            [ Ui.width Ui.fill
            , Ui.htmlAttribute (Attr.style "user-select" "none")
            ]
            [ options.title
            , viewWindowBar options
            ]

    else if isMac options.platform then
        Ui.row [ Ui.width Ui.fill ]
            [ viewMacBar options
            , options.title
            ]

    else if isLinux options.platform then
        Ui.row [ Ui.width Ui.fill ]
            [ options.title
            , viewWindowBar options
            ]

    else
        Ui.row [ Ui.width Ui.fill ]
            [ options.title
            ]


isWindows : Flags.Platform -> Bool
isWindows platform =
    platform == Flags.Windows


isMac : Flags.Platform -> Bool
isMac platform =
    platform == Flags.Mac


isLinux : Flags.Platform -> Bool
isLinux platform =
    platform == Flags.Linux


viewMacBar : { a | onClose : msg, onMaximize : msg, onMinimize : msg } -> Element msg
viewMacBar options =
    Ui.row
        [ Ui.spacing 8
        , Ui.padding 8
        , Ui.alignTop
        , Ui.width Ui.fill
        , Ui.htmlAttribute (Attr.attribute "data-tauri-drag-region" "")
        , Ui.htmlAttribute (Attr.class "titlebar")
        ]
        [ circle
            [ Ui.htmlAttribute (Events.onClick options.onClose)
            ]
            (Html.img
                [ Attr.src "https://api.iconify.design/mdi:close.svg"
                , Attr.alt "close"
                , Attr.style "width" "10px"
                ]
                []
                |> Ui.html
            )
        , circle
            [ Ui.htmlAttribute (Events.onClick options.onMinimize)
            ]
            (Html.img
                [ Attr.src "https://api.iconify.design/mdi:minimize.svg"
                , Attr.alt "minimize"
                , Attr.style "width" "10px"
                ]
                []
                |> Ui.html
            )
        , circle
            [ Ui.htmlAttribute (Events.onClick options.onMaximize)
            ]
            (Html.img
                [ Attr.src "https://api.iconify.design/mdi:expand-horizontal.svg"
                , Attr.alt "maximize"
                , Attr.style "transform" "rotate(45deg)"
                , Attr.style "width" "10px"
                ]
                []
                |> Ui.html
            )
        ]


circle : List (Attribute msg) -> Element msg -> Element msg
circle attributes content =
    Ui.el
        ([ Ui.width (Ui.px 12)
         , Ui.height (Ui.px 12)
         , Border.rounded 12
         , Background.color Ui.colors.grey.medium
         ]
            ++ attributes
        )
        (Ui.el
            [ Ui.centerX
            , Ui.centerY
            ]
            content
        )


viewWindowBar : { a | onClose : msg, onMaximize : msg, onMinimize : msg } -> Element msg
viewWindowBar options =
    Ui.el
        [ Ui.width Ui.fill
        , Ui.alignTop
        , Ui.htmlAttribute (Attr.attribute "data-tauri-drag-region" "")
        , Ui.htmlAttribute (Attr.class "titlebar")
        ]
        (Ui.row [ Ui.spacing 8, Ui.alignRight, Ui.moveUp 4 ]
            [ Ui.el
                [ Ui.htmlAttribute (Attr.class "titlebar-button")
                , Ui.htmlAttribute (Attr.id "titlebar-minimize")
                , Ui.htmlAttribute (Events.onClick options.onMinimize)
                , Ui.width (Ui.px 16)
                ]
                (Html.img
                    [ Attr.src "https://api.iconify.design/mdi:window-minimize.svg"
                    , Attr.alt "minimize"
                    ]
                    []
                    |> Ui.html
                )
            , Ui.el
                [ Ui.htmlAttribute (Attr.class "titlebar-button")
                , Ui.htmlAttribute (Attr.id "titlebar-maximize")
                , Ui.htmlAttribute (Events.onClick options.onMaximize)
                , Ui.width (Ui.px 16)
                , Ui.moveRight 2
                ]
                (Html.img
                    [ Attr.src "https://api.iconify.design/mdi:window-maximize.svg"
                    , Attr.alt "maximize"
                    ]
                    []
                    |> Ui.html
                )
            , Ui.el
                [ Ui.htmlAttribute (Attr.class "titlebar-button")
                , Ui.htmlAttribute (Attr.id "titlebar-close")
                , Ui.htmlAttribute (Events.onClick options.onClose)
                , Ui.width (Ui.px 16)
                ]
                (Html.img
                    [ Attr.src "https://api.iconify.design/mdi:close.svg"
                    , Attr.alt "close"
                    ]
                    []
                    |> Ui.html
                )
            ]
        )
