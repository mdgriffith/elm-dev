module Gen.Test.Html.Event exposing (annotation_, blur, call_, check, click, custom, doubleClick, expect, focus, input, moduleName_, mouseDown, mouseEnter, mouseLeave, mouseOut, mouseOver, mouseUp, simulate, submit, toResult, values_)

{-| 
@docs values_, call_, annotation_, focus, blur, submit, check, input, mouseOut, mouseOver, mouseLeave, mouseEnter, mouseUp, mouseDown, doubleClick, click, custom, toResult, expect, simulate, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Test", "Html", "Event" ]


{-| Simulate an event on a node.

    import Test.Html.Event as Event

    type Msg
        = Change String


    test "Input produces expected Msg" <|
        \() ->
            Html.input [ onInput Change ] [ ]
                |> Query.fromHtml
                |> Event.simulate (Event.input "cats")
                |> Event.expect (Change "cats")

simulate: 
    ( String, Json.Encode.Value )
    -> Test.Html.Query.Single msg
    -> Test.Html.Event.Event msg
-}
simulate : Elm.Expression -> Elm.Expression -> Elm.Expression
simulate simulateArg simulateArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "simulate"
            , annotation =
                Just
                    (Type.function
                        [ Type.tuple
                            Type.string
                            (Type.namedWith [ "Json", "Encode" ] "Value" [])
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Event" ]
                            "Event"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ simulateArg, simulateArg0 ]


{-| Passes if the given message is triggered by the simulated event.

    import Test.Html.Event as Event

    type Msg
        = Change String


    test "Input produces expected Msg" <|
        \() ->
            Html.input [ onInput Change ] [ ]
                |> Query.fromHtml
                |> Event.simulate (Event.input "cats")
                |> Event.expect (Change "cats")

expect: msg -> Test.Html.Event.Event msg -> Expect.Expectation
-}
expect : Elm.Expression -> Elm.Expression -> Elm.Expression
expect expectArg expectArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "expect"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg"
                        , Type.namedWith
                            [ "Test", "Html", "Event" ]
                            "Event"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ expectArg, expectArg0 ]


{-| Returns a Result with the Msg produced by the event simulated on a node.
Note that Event.expect gives nicer messages; this is generally more useful
when testing that an event handler is _not_ present.

    import Test.Html.Event as Event


    test "Input produces expected Msg" <|
        \() ->
            Html.input [ onInput Change ] [ ]
                |> Query.fromHtml
                |> Event.simulate (Event.input "cats")
                |> Event.toResult
                |> Expect.equal (Ok (Change "cats"))

toResult: Test.Html.Event.Event msg -> Result.Result String msg
-}
toResult : Elm.Expression -> Elm.Expression
toResult toResultArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "toResult"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Test", "Html", "Event" ]
                            "Event"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Result" ]
                            "Result"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
        )
        [ toResultArg ]


{-| Simulate a custom event. The `String` is the event name, and the `Value` is the event object
the browser would send to the event listener callback.

    import Test.Html.Event as Event
    import Json.Encode as Encode exposing (Value)


    type Msg
        = Change String


    test "Input produces expected Msg" <|
        \() ->
            let
                simulatedEventObject : Value
                simulatedEventObject =
                    Encode.object
                        [ ( "target"
                          , Encode.object [ ( "value", Encode.string "cats" ) ]
                          )
                        ]
            in
                Html.input [ onInput Change ] [ ]
                    |> Query.fromHtml
                    |> Event.simulate (Event.custom "input" simulatedEventObject)
                    |> Event.expect (Change "cats")

custom: String -> Json.Encode.Value -> ( String, Json.Encode.Value )
-}
custom : String -> Elm.Expression -> Elm.Expression
custom customArg customArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Json", "Encode" ] "Value" []
                        ]
                        (Type.tuple
                            Type.string
                            (Type.namedWith [ "Json", "Encode" ] "Value" [])
                        )
                    )
            }
        )
        [ Elm.string customArg, customArg0 ]


{-| A [`click`](https://developer.mozilla.org/en-US/docs/Web/Events/click) event.

click: ( String, Json.Encode.Value )
-}
click : Elm.Expression
click =
    Elm.value
        { importFrom = [ "Test", "Html", "Event" ]
        , name = "click"
        , annotation =
            Just
                (Type.tuple
                    Type.string
                    (Type.namedWith [ "Json", "Encode" ] "Value" [])
                )
        }


{-| A [`dblclick`](https://developer.mozilla.org/en-US/docs/Web/Events/dblclick) event.

doubleClick: ( String, Json.Encode.Value )
-}
doubleClick : Elm.Expression
doubleClick =
    Elm.value
        { importFrom = [ "Test", "Html", "Event" ]
        , name = "doubleClick"
        , annotation =
            Just
                (Type.tuple
                    Type.string
                    (Type.namedWith [ "Json", "Encode" ] "Value" [])
                )
        }


{-| A [`mousedown`](https://developer.mozilla.org/en-US/docs/Web/Events/mousedown) event.

mouseDown: ( String, Json.Encode.Value )
-}
mouseDown : Elm.Expression
mouseDown =
    Elm.value
        { importFrom = [ "Test", "Html", "Event" ]
        , name = "mouseDown"
        , annotation =
            Just
                (Type.tuple
                    Type.string
                    (Type.namedWith [ "Json", "Encode" ] "Value" [])
                )
        }


{-| A [`mouseup`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseup) event.

mouseUp: ( String, Json.Encode.Value )
-}
mouseUp : Elm.Expression
mouseUp =
    Elm.value
        { importFrom = [ "Test", "Html", "Event" ]
        , name = "mouseUp"
        , annotation =
            Just
                (Type.tuple
                    Type.string
                    (Type.namedWith [ "Json", "Encode" ] "Value" [])
                )
        }


{-| A [`mouseenter`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter) event.

mouseEnter: ( String, Json.Encode.Value )
-}
mouseEnter : Elm.Expression
mouseEnter =
    Elm.value
        { importFrom = [ "Test", "Html", "Event" ]
        , name = "mouseEnter"
        , annotation =
            Just
                (Type.tuple
                    Type.string
                    (Type.namedWith [ "Json", "Encode" ] "Value" [])
                )
        }


{-| A [`mouseleave`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave) event.

mouseLeave: ( String, Json.Encode.Value )
-}
mouseLeave : Elm.Expression
mouseLeave =
    Elm.value
        { importFrom = [ "Test", "Html", "Event" ]
        , name = "mouseLeave"
        , annotation =
            Just
                (Type.tuple
                    Type.string
                    (Type.namedWith [ "Json", "Encode" ] "Value" [])
                )
        }


{-| A [`mouseover`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseover) event.

mouseOver: ( String, Json.Encode.Value )
-}
mouseOver : Elm.Expression
mouseOver =
    Elm.value
        { importFrom = [ "Test", "Html", "Event" ]
        , name = "mouseOver"
        , annotation =
            Just
                (Type.tuple
                    Type.string
                    (Type.namedWith [ "Json", "Encode" ] "Value" [])
                )
        }


{-| A [`mouseout`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseout) event.

mouseOut: ( String, Json.Encode.Value )
-}
mouseOut : Elm.Expression
mouseOut =
    Elm.value
        { importFrom = [ "Test", "Html", "Event" ]
        , name = "mouseOut"
        , annotation =
            Just
                (Type.tuple
                    Type.string
                    (Type.namedWith [ "Json", "Encode" ] "Value" [])
                )
        }


{-| An [`input`](https://developer.mozilla.org/en-US/docs/Web/Events/input) event.

input: String -> ( String, Json.Encode.Value )
-}
input : String -> Elm.Expression
input inputArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "input"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.tuple
                            Type.string
                            (Type.namedWith [ "Json", "Encode" ] "Value" [])
                        )
                    )
            }
        )
        [ Elm.string inputArg ]


{-| A [`change`](https://developer.mozilla.org/en-US/docs/Web/Events/change) event
where `event.target.checked` is set to the given `Bool` value.

check: Bool -> ( String, Json.Encode.Value )
-}
check : Bool -> Elm.Expression
check checkArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "check"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.tuple
                            Type.string
                            (Type.namedWith [ "Json", "Encode" ] "Value" [])
                        )
                    )
            }
        )
        [ Elm.bool checkArg ]


{-| A [`submit`](https://developer.mozilla.org/en-US/docs/Web/Events/submit) event.

submit: ( String, Json.Encode.Value )
-}
submit : Elm.Expression
submit =
    Elm.value
        { importFrom = [ "Test", "Html", "Event" ]
        , name = "submit"
        , annotation =
            Just
                (Type.tuple
                    Type.string
                    (Type.namedWith [ "Json", "Encode" ] "Value" [])
                )
        }


{-| A [`blur`](https://developer.mozilla.org/en-US/docs/Web/Events/blur) event.

blur: ( String, Json.Encode.Value )
-}
blur : Elm.Expression
blur =
    Elm.value
        { importFrom = [ "Test", "Html", "Event" ]
        , name = "blur"
        , annotation =
            Just
                (Type.tuple
                    Type.string
                    (Type.namedWith [ "Json", "Encode" ] "Value" [])
                )
        }


{-| A [`focus`](https://developer.mozilla.org/en-US/docs/Web/Events/focus) event.

focus: ( String, Json.Encode.Value )
-}
focus : Elm.Expression
focus =
    Elm.value
        { importFrom = [ "Test", "Html", "Event" ]
        , name = "focus"
        , annotation =
            Just
                (Type.tuple
                    Type.string
                    (Type.namedWith [ "Json", "Encode" ] "Value" [])
                )
        }


annotation_ : { event : Type.Annotation -> Type.Annotation }
annotation_ =
    { event =
        \eventArg0 ->
            Type.namedWith [ "Test", "Html", "Event" ] "Event" [ eventArg0 ]
    }


call_ :
    { simulate : Elm.Expression -> Elm.Expression -> Elm.Expression
    , expect : Elm.Expression -> Elm.Expression -> Elm.Expression
    , toResult : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression -> Elm.Expression
    , input : Elm.Expression -> Elm.Expression
    , check : Elm.Expression -> Elm.Expression
    }
call_ =
    { simulate =
        \simulateArg simulateArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Event" ]
                    , name = "simulate"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.tuple
                                    Type.string
                                    (Type.namedWith
                                        [ "Json", "Encode" ]
                                        "Value"
                                        []
                                    )
                                , Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Single"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Test", "Html", "Event" ]
                                    "Event"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ simulateArg, simulateArg0 ]
    , expect =
        \expectArg expectArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Event" ]
                    , name = "expect"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg"
                                , Type.namedWith
                                    [ "Test", "Html", "Event" ]
                                    "Event"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ expectArg, expectArg0 ]
    , toResult =
        \toResultArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Event" ]
                    , name = "toResult"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Test", "Html", "Event" ]
                                    "Event"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Result" ]
                                    "Result"
                                    [ Type.string, Type.var "msg" ]
                                )
                            )
                    }
                )
                [ toResultArg ]
    , custom =
        \customArg customArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Event" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith [ "Json", "Encode" ] "Value" []
                                ]
                                (Type.tuple
                                    Type.string
                                    (Type.namedWith
                                        [ "Json", "Encode" ]
                                        "Value"
                                        []
                                    )
                                )
                            )
                    }
                )
                [ customArg, customArg0 ]
    , input =
        \inputArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Event" ]
                    , name = "input"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.tuple
                                    Type.string
                                    (Type.namedWith
                                        [ "Json", "Encode" ]
                                        "Value"
                                        []
                                    )
                                )
                            )
                    }
                )
                [ inputArg ]
    , check =
        \checkArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Event" ]
                    , name = "check"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.tuple
                                    Type.string
                                    (Type.namedWith
                                        [ "Json", "Encode" ]
                                        "Value"
                                        []
                                    )
                                )
                            )
                    }
                )
                [ checkArg ]
    }


values_ :
    { simulate : Elm.Expression
    , expect : Elm.Expression
    , toResult : Elm.Expression
    , custom : Elm.Expression
    , click : Elm.Expression
    , doubleClick : Elm.Expression
    , mouseDown : Elm.Expression
    , mouseUp : Elm.Expression
    , mouseEnter : Elm.Expression
    , mouseLeave : Elm.Expression
    , mouseOver : Elm.Expression
    , mouseOut : Elm.Expression
    , input : Elm.Expression
    , check : Elm.Expression
    , submit : Elm.Expression
    , blur : Elm.Expression
    , focus : Elm.Expression
    }
values_ =
    { simulate =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "simulate"
            , annotation =
                Just
                    (Type.function
                        [ Type.tuple
                            Type.string
                            (Type.namedWith [ "Json", "Encode" ] "Value" [])
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Event" ]
                            "Event"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , expect =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "expect"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg"
                        , Type.namedWith
                            [ "Test", "Html", "Event" ]
                            "Event"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , toResult =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "toResult"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Test", "Html", "Event" ]
                            "Event"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Result" ]
                            "Result"
                            [ Type.string, Type.var "msg" ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Json", "Encode" ] "Value" []
                        ]
                        (Type.tuple
                            Type.string
                            (Type.namedWith [ "Json", "Encode" ] "Value" [])
                        )
                    )
            }
    , click =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "click"
            , annotation =
                Just
                    (Type.tuple
                        Type.string
                        (Type.namedWith [ "Json", "Encode" ] "Value" [])
                    )
            }
    , doubleClick =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "doubleClick"
            , annotation =
                Just
                    (Type.tuple
                        Type.string
                        (Type.namedWith [ "Json", "Encode" ] "Value" [])
                    )
            }
    , mouseDown =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "mouseDown"
            , annotation =
                Just
                    (Type.tuple
                        Type.string
                        (Type.namedWith [ "Json", "Encode" ] "Value" [])
                    )
            }
    , mouseUp =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "mouseUp"
            , annotation =
                Just
                    (Type.tuple
                        Type.string
                        (Type.namedWith [ "Json", "Encode" ] "Value" [])
                    )
            }
    , mouseEnter =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "mouseEnter"
            , annotation =
                Just
                    (Type.tuple
                        Type.string
                        (Type.namedWith [ "Json", "Encode" ] "Value" [])
                    )
            }
    , mouseLeave =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "mouseLeave"
            , annotation =
                Just
                    (Type.tuple
                        Type.string
                        (Type.namedWith [ "Json", "Encode" ] "Value" [])
                    )
            }
    , mouseOver =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "mouseOver"
            , annotation =
                Just
                    (Type.tuple
                        Type.string
                        (Type.namedWith [ "Json", "Encode" ] "Value" [])
                    )
            }
    , mouseOut =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "mouseOut"
            , annotation =
                Just
                    (Type.tuple
                        Type.string
                        (Type.namedWith [ "Json", "Encode" ] "Value" [])
                    )
            }
    , input =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "input"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.tuple
                            Type.string
                            (Type.namedWith [ "Json", "Encode" ] "Value" [])
                        )
                    )
            }
    , check =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "check"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.tuple
                            Type.string
                            (Type.namedWith [ "Json", "Encode" ] "Value" [])
                        )
                    )
            }
    , submit =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "submit"
            , annotation =
                Just
                    (Type.tuple
                        Type.string
                        (Type.namedWith [ "Json", "Encode" ] "Value" [])
                    )
            }
    , blur =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "blur"
            , annotation =
                Just
                    (Type.tuple
                        Type.string
                        (Type.namedWith [ "Json", "Encode" ] "Value" [])
                    )
            }
    , focus =
        Elm.value
            { importFrom = [ "Test", "Html", "Event" ]
            , name = "focus"
            , annotation =
                Just
                    (Type.tuple
                        Type.string
                        (Type.namedWith [ "Json", "Encode" ] "Value" [])
                    )
            }
    }


