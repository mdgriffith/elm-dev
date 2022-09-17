module Gen.Browser.Events exposing (annotation_, call_, caseOf_, make_, moduleName_, onAnimationFrame, onAnimationFrameDelta, onClick, onKeyDown, onKeyPress, onKeyUp, onMouseDown, onMouseMove, onMouseUp, onResize, onVisibilityChange, values_)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, onVisibilityChange, onResize, onMouseUp, onMouseDown, onMouseMove, onClick, onKeyUp, onKeyDown, onKeyPress, onAnimationFrameDelta, onAnimationFrame, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Browser", "Events" ]


{-| An animation frame triggers about 60 times per second. Get the POSIX time
on each frame. (See [`elm/time`](/packages/elm/time/latest) for more info on
POSIX times.)

**Note:** Browsers have their own render loop, repainting things as fast as
possible. If you want smooth animations in your application, it is helpful to
sync up with the browsers natural refresh rate. This hooks into JavaScript's
`requestAnimationFrame` function.

onAnimationFrame: (Time.Posix -> msg) -> Platform.Sub.Sub msg
-}
onAnimationFrame : (Elm.Expression -> Elm.Expression) -> Elm.Expression
onAnimationFrame onAnimationFrameArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onAnimationFrame"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith [ "Time" ] "Posix" [] ]
                            (Type.var "msg")
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.functionReduced "onAnimationFrameUnpack" onAnimationFrameArg ]


{-| Just like `onAnimationFrame`, except message is the time in milliseconds
since the previous frame. So you should get a sequence of values all around
`1000 / 60` which is nice for stepping animations by a time delta.

onAnimationFrameDelta: (Float -> msg) -> Platform.Sub.Sub msg
-}
onAnimationFrameDelta : (Elm.Expression -> Elm.Expression) -> Elm.Expression
onAnimationFrameDelta onAnimationFrameDeltaArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onAnimationFrameDelta"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.float ] (Type.var "msg") ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.functionReduced
            "onAnimationFrameDeltaUnpack"
            onAnimationFrameDeltaArg
        ]


{-| Subscribe to key presses that normally produce characters. So you should
not rely on this for arrow keys.

**Note:** Check out [this advice][note] to learn more about decoding key codes.
It is more complicated than it should be.

[note]: https://github.com/elm/browser/blob/1.0.2/notes/keyboard.md

onKeyPress: Json.Decode.Decoder msg -> Platform.Sub.Sub msg
-}
onKeyPress : Elm.Expression -> Elm.Expression
onKeyPress onKeyPressArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onKeyPress"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ onKeyPressArg ]


{-| Subscribe to get codes whenever a key goes down. This can be useful for
creating games. Maybe you want to know if people are pressing `w`, `a`, `s`,
or `d` at any given time.

**Note:** Check out [this advice][note] to learn more about decoding key codes.
It is more complicated than it should be.

[note]: https://github.com/elm/browser/blob/1.0.2/notes/keyboard.md

onKeyDown: Json.Decode.Decoder msg -> Platform.Sub.Sub msg
-}
onKeyDown : Elm.Expression -> Elm.Expression
onKeyDown onKeyDownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onKeyDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ onKeyDownArg ]


{-| Subscribe to get codes whenever a key goes up. Often used in combination
with [`onVisibilityChange`](#onVisibilityChange) to be sure keys do not appear
to down and never come back up.

onKeyUp: Json.Decode.Decoder msg -> Platform.Sub.Sub msg
-}
onKeyUp : Elm.Expression -> Elm.Expression
onKeyUp onKeyUpArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onKeyUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ onKeyUpArg ]


{-| Subscribe to mouse clicks anywhere on screen. Maybe you need to create a
custom drop down. You could listen for clicks when it is open, letting you know
if someone clicked out of it:

    import Browser.Events as Events
    import Json.Decode as D

    type Msg
       = ClickOut

    subscriptions : Model -> Sub Msg
    subscriptions model =
      case model.dropDown of
        Closed _ ->
          Sub.none

        Open _ ->
          Events.onClick (D.succeed ClickOut)

onClick: Json.Decode.Decoder msg -> Platform.Sub.Sub msg
-}
onClick : Elm.Expression -> Elm.Expression
onClick onClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ onClickArg ]


{-| Subscribe to mouse moves anywhere on screen.

You could use this to implement resizable panels like in Elm's online code
editor. Check out the example imprementation [here][drag].

[drag]: https://github.com/elm/browser/blob/1.0.2/examples/src/Drag.elm

**Note:** Unsubscribe if you do not need these events! Running code on every
single mouse movement can be very costly, and it is recommended to only
subscribe when absolutely necessary.

onMouseMove: Json.Decode.Decoder msg -> Platform.Sub.Sub msg
-}
onMouseMove : Elm.Expression -> Elm.Expression
onMouseMove onMouseMoveArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onMouseMove"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ onMouseMoveArg ]


{-| Subscribe to get mouse information whenever the mouse button goes down.

onMouseDown: Json.Decode.Decoder msg -> Platform.Sub.Sub msg
-}
onMouseDown : Elm.Expression -> Elm.Expression
onMouseDown onMouseDownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onMouseDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ onMouseDownArg ]


{-| Subscribe to get mouse information whenever the mouse button goes up.
Often used in combination with [`onVisibilityChange`](#onVisibilityChange)
to be sure keys do not appear to down and never come back up.

onMouseUp: Json.Decode.Decoder msg -> Platform.Sub.Sub msg
-}
onMouseUp : Elm.Expression -> Elm.Expression
onMouseUp onMouseUpArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onMouseUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ onMouseUpArg ]


{-| Subscribe to any changes in window size.

For example, you could track the current width by saying:

    import Browser.Events as E

    type Msg
      = GotNewWidth Int

    subscriptions : model -> Cmd Msg
    subscriptions _ =
      E.onResize (\w h -> GotNewWidth w)

**Note:** This is equivalent to getting events from [`window.onresize`][resize].

[resize]: https://developer.mozilla.org/en-US/docs/Web/API/GlobalEventHandlers/onresize

onResize: (Int -> Int -> msg) -> Platform.Sub.Sub msg
-}
onResize :
    (Elm.Expression -> Elm.Expression -> Elm.Expression) -> Elm.Expression
onResize onResizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onResize"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.int, Type.int ] (Type.var "msg")
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.functionReduced
            "onResizeUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced "unpack" (onResizeArg functionReducedUnpack)
            )
        ]


{-| Subscribe to any visibility changes, like if the user switches to a
different tab or window. When the user looks away, you may want to:

- Pause a timer.
- Pause an animation.
- Pause video or audio.
- Pause an image carousel.
- Stop polling a server for new information.
- Stop waiting for an [`onKeyUp`](#onKeyUp) event.

onVisibilityChange: (Browser.Events.Visibility -> msg) -> Platform.Sub.Sub msg
-}
onVisibilityChange : (Elm.Expression -> Elm.Expression) -> Elm.Expression
onVisibilityChange onVisibilityChangeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onVisibilityChange"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith
                                [ "Browser", "Events" ]
                                "Visibility"
                                []
                            ]
                            (Type.var "msg")
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.functionReduced "onVisibilityChangeUnpack" onVisibilityChangeArg ]


annotation_ : { visibility : Type.Annotation }
annotation_ =
    { visibility = Type.namedWith [ "Browser", "Events" ] "Visibility" [] }


make_ : { visible : Elm.Expression, hidden : Elm.Expression }
make_ =
    { visible =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "Visible"
            , annotation = Just (Type.namedWith [] "Visibility" [])
            }
    , hidden =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "Hidden"
            , annotation = Just (Type.namedWith [] "Visibility" [])
            }
    }


caseOf_ :
    { visibility :
        Elm.Expression
        -> { visibilityTags_0_0
            | visible : Elm.Expression
            , hidden : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { visibility =
        \visibilityExpression visibilityTags ->
            Elm.Case.custom
                visibilityExpression
                (Type.namedWith [ "Browser", "Events" ] "Visibility" [])
                [ Elm.Case.branch0 "Visible" visibilityTags.visible
                , Elm.Case.branch0 "Hidden" visibilityTags.hidden
                ]
    }


call_ :
    { onAnimationFrame : Elm.Expression -> Elm.Expression
    , onAnimationFrameDelta : Elm.Expression -> Elm.Expression
    , onKeyPress : Elm.Expression -> Elm.Expression
    , onKeyDown : Elm.Expression -> Elm.Expression
    , onKeyUp : Elm.Expression -> Elm.Expression
    , onClick : Elm.Expression -> Elm.Expression
    , onMouseMove : Elm.Expression -> Elm.Expression
    , onMouseDown : Elm.Expression -> Elm.Expression
    , onMouseUp : Elm.Expression -> Elm.Expression
    , onResize : Elm.Expression -> Elm.Expression
    , onVisibilityChange : Elm.Expression -> Elm.Expression
    }
call_ =
    { onAnimationFrame =
        \onAnimationFrameArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Events" ]
                    , name = "onAnimationFrame"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.namedWith [ "Time" ] "Posix" [] ]
                                    (Type.var "msg")
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ onAnimationFrameArg ]
    , onAnimationFrameDelta =
        \onAnimationFrameDeltaArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Events" ]
                    , name = "onAnimationFrameDelta"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.float ] (Type.var "msg")
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ onAnimationFrameDeltaArg ]
    , onKeyPress =
        \onKeyPressArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Events" ]
                    , name = "onKeyPress"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ onKeyPressArg ]
    , onKeyDown =
        \onKeyDownArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Events" ]
                    , name = "onKeyDown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ onKeyDownArg ]
    , onKeyUp =
        \onKeyUpArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Events" ]
                    , name = "onKeyUp"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ onKeyUpArg ]
    , onClick =
        \onClickArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Events" ]
                    , name = "onClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ onClickArg ]
    , onMouseMove =
        \onMouseMoveArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Events" ]
                    , name = "onMouseMove"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ onMouseMoveArg ]
    , onMouseDown =
        \onMouseDownArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Events" ]
                    , name = "onMouseDown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ onMouseDownArg ]
    , onMouseUp =
        \onMouseUpArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Events" ]
                    , name = "onMouseUp"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Json", "Decode" ]
                                    "Decoder"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ onMouseUpArg ]
    , onResize =
        \onResizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Events" ]
                    , name = "onResize"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.int, Type.int ]
                                    (Type.var "msg")
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ onResizeArg ]
    , onVisibilityChange =
        \onVisibilityChangeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Events" ]
                    , name = "onVisibilityChange"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.namedWith
                                        [ "Browser", "Events" ]
                                        "Visibility"
                                        []
                                    ]
                                    (Type.var "msg")
                                ]
                                (Type.namedWith [] "Sub" [ Type.var "msg" ])
                            )
                    }
                )
                [ onVisibilityChangeArg ]
    }


values_ :
    { onAnimationFrame : Elm.Expression
    , onAnimationFrameDelta : Elm.Expression
    , onKeyPress : Elm.Expression
    , onKeyDown : Elm.Expression
    , onKeyUp : Elm.Expression
    , onClick : Elm.Expression
    , onMouseMove : Elm.Expression
    , onMouseDown : Elm.Expression
    , onMouseUp : Elm.Expression
    , onResize : Elm.Expression
    , onVisibilityChange : Elm.Expression
    }
values_ =
    { onAnimationFrame =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onAnimationFrame"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith [ "Time" ] "Posix" [] ]
                            (Type.var "msg")
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , onAnimationFrameDelta =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onAnimationFrameDelta"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.float ] (Type.var "msg") ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , onKeyPress =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onKeyPress"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , onKeyDown =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onKeyDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , onKeyUp =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onKeyUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , onClick =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , onMouseMove =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onMouseMove"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , onMouseDown =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onMouseDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , onMouseUp =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onMouseUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Json", "Decode" ]
                            "Decoder"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , onResize =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onResize"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.int, Type.int ] (Type.var "msg")
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    , onVisibilityChange =
        Elm.value
            { importFrom = [ "Browser", "Events" ]
            , name = "onVisibilityChange"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith
                                [ "Browser", "Events" ]
                                "Visibility"
                                []
                            ]
                            (Type.var "msg")
                        ]
                        (Type.namedWith [] "Sub" [ Type.var "msg" ])
                    )
            }
    }


