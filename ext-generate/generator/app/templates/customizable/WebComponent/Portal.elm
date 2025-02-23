module WebComponent.Portal exposing
    ( Model, closed
    , view
    , MenuPosition(..)
    , isOpen
    , Element, Window
    )

{-| This custom element is helping us with 2 things.

First, we need to be able to render dropdown menus at the top of the DOM so they don't accidently get clipped when there are scrollbars.

The scrollbar clipping thing is a hard blocker from how CSS and stacking contexts work, so our solution is to render it in one place in
the DOM in Elm, but have some javascript that moves the element to a place at the top of the DOM behind the scenes.

This is called a "portal" in React land. So, we're just copying that here.

Second! We also want a drop down to be a drop _up_ if it is too low on the screen. We can do this calculation by capturing bounding boxes and doing some math with the window.

This element does not care about other styling or behavior though!

@docs Model, closed
@docs view

@docs MenuPosition
@docs isOpen
@docs Element, Window

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode


type Model
    = Open Viewport
    | Closed


closed : Model
closed =
    Closed


isOpen : Model -> Bool
isOpen model =
    model /= Closed


type alias Viewport =
    { parent : Element
    , window : Window
    }


type alias Element =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias Window =
    { width : Float
    , height : Float
    }


onToggle :
    { options
        | model : Model
        , onMsg : Model -> msg
    }
    -> Html.Attribute msg
onToggle { model, onMsg } =
    Html.Events.on "click"
        (Json.Decode.map2
            (\parent window ->
                case model of
                    Closed ->
                        onMsg
                            (Open
                                { parent = parent
                                , window = window
                                }
                            )

                    Open _ ->
                        onMsg Closed
            )
            (Json.Decode.at [ "currentTarget", "__getParentClientRect" ] elementDecoder)
            (Json.Decode.at [ "currentTarget", "__getWindowSize" ] windowDecode)
        )


windowDecode : Json.Decode.Decoder Window
windowDecode =
    Json.Decode.map2 Window
        (Json.Decode.field "width" Json.Decode.float)
        (Json.Decode.field "height" Json.Decode.float)


elementDecoder : Json.Decode.Decoder Element
elementDecoder =
    Json.Decode.map4 Element
        (Json.Decode.field "x" Json.Decode.float)
        (Json.Decode.field "y" Json.Decode.float)
        (Json.Decode.field "width" Json.Decode.float)
        (Json.Decode.field "height" Json.Decode.float)


view :
    { position : MenuPosition
    , model : Model
    , onMsg : Model -> msg
    , button : Html msg
    , menu : Html msg
    }
    -> Html msg
view options =
    let
        dismissMenuOnEscapeEvent =
            Html.Events.on "keyup"
                (Json.Decode.field "key" Json.Decode.string
                    |> Json.Decode.andThen
                        (\key ->
                            if key == "Escape" then
                                Json.Decode.succeed (options.onMsg Closed)

                            else
                                Json.Decode.fail key
                        )
                )

        viewMenu : Viewport -> Html msg
        viewMenu viewport =
            Html.div
                [ Attr.style "position" "fixed"
                , Attr.style "top" (px viewport.parent.y)
                , Attr.style "left" (px viewport.parent.x)
                , Attr.style "width" (px viewport.parent.width)
                , Attr.style "height" (px viewport.parent.height)
                , dismissMenuOnEscapeEvent
                ]
                [ Html.div
                    [ Html.Events.onClick (options.onMsg Closed)
                    , Attr.style "position" "absolute"
                    , Attr.style "top" "0"
                    , Attr.style "left" "0"
                    , Attr.style "right" "0"
                    , Attr.style "bottom" "0"
                    ]
                    []
                , Html.div
                    (toPositionAttributes
                        { position = options.position
                        , viewport = viewport
                        }
                    )
                    [ options.menu ]
                ]

        viewDismissOverlay : Html msg
        viewDismissOverlay =
            Html.div
                [ Html.Events.onClick (options.onMsg Closed)
                , Attr.style "position" "fixed"
                , Attr.style "top" "0"
                , Attr.style "left" "0"
                , Attr.style "width" "100%"
                , Attr.style "height" "100%"
                ]
                []
    in
    Html.div []
        [ Html.div [ onToggle options ] [ options.button ]
        , case options.model of
            Open viewport ->
                Html.node "elm-portal"
                    []
                    [ viewDismissOverlay
                    , viewMenu viewport
                    ]

            Closed ->
                Html.text ""
        ]


type MenuPosition
    = -- Render below toggle button
      Below { isAlignedLeft : Bool }
      -- Render to the right of the toggle button
    | ToRightOf


toPositionAttributes :
    { position : MenuPosition, viewport : Viewport }
    -> List (Html.Attribute msg)
toPositionAttributes { position, viewport } =
    let
        menuBelowRightAligned =
            [ Attr.style "position" "absolute"
            , Attr.style "top" "calc(100% + 4px)"
            , Attr.style "right" "0"
            , Attr.style "transform" "none"
            , Attr.style "min-width" "max-content"
            ]

        menuBelowLeftAligned =
            [ Attr.style "position" "absolute"
            , Attr.style "top" "calc(100% + 4px)"
            , Attr.style "left" "0"
            , Attr.style "transform" "none"
            , Attr.style "min-width" "max-content"
            ]

        menuOnRightTopAligned =
            [ Attr.style "position" "absolute"
            , Attr.style "left" "calc(100% + 5px)"
            , Attr.style "top" "0px"
            , Attr.style "transform" "none"
            , Attr.style "min-width" "max-content"
            ]

        menuOnRightBottomAligned =
            [ Attr.style "position" "absolute"
            , Attr.style "left" "calc(100% + 5px)"
            , Attr.style "bottom" "0px"
            , Attr.style "transform" "none"
            , Attr.style "min-width" "max-content"
            ]

        attemptToAlignLeft =
            if viewport.window.width - viewport.parent.x < 400 then
                menuBelowRightAligned

            else
                menuBelowLeftAligned

        attemptToAlignRight =
            if viewport.parent.x < 400 then
                menuBelowLeftAligned

            else
                menuBelowRightAligned
    in
    case position of
        Below { isAlignedLeft } ->
            if isAlignedLeft then
                attemptToAlignLeft

            else
                attemptToAlignRight

        ToRightOf ->
            if viewport.window.height - viewport.parent.y < 400 then
                menuOnRightBottomAligned

            else
                menuOnRightTopAligned


px : Float -> String
px float =
    String.fromFloat float ++ "px"
