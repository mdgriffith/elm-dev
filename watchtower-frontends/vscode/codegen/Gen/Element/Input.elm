module Gen.Element.Input exposing (annotation_, button, call_, caseOf_, checkbox, currentPassword, defaultCheckbox, defaultThumb, email, focusedOnLoad, labelAbove, labelBelow, labelHidden, labelLeft, labelRight, make_, moduleName_, multiline, newPassword, option, optionWith, placeholder, radio, radioRow, search, slider, spellChecked, text, thumb, username, values_)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, labelHidden, labelRight, labelLeft, labelBelow, labelAbove, optionWith, option, radioRow, radio, defaultThumb, thumb, slider, spellChecked, search, email, currentPassword, newPassword, username, placeholder, multiline, text, defaultCheckbox, checkbox, button, focusedOnLoad, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Element", "Input" ]


{-| Attach this attribute to any `Input` that you would like to be automatically focused when the page loads.

You should only have a maximum of one per page.

focusedOnLoad: Element.Attribute msg
-}
focusedOnLoad : Elm.Expression
focusedOnLoad =
    Elm.value
        { importFrom = [ "Element", "Input" ]
        , name = "focusedOnLoad"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| A standard button.

The `onPress` handler will be fired either `onClick` or when the element is focused and the `Enter` key has been pressed.

    import Element exposing (rgb255, text)
    import Element.Background as Background
    import Element.Input as Input

    blue =
        Element.rgb255 238 238 238

    myButton =
        Input.button
            [ Background.color blue
            , Element.focused
                [ Background.color purple ]
            ]
            { onPress = Just ClickMsg
            , label = text "My Button"
            }

**Note** If you have an icon button but want it to be accessible, consider adding a [`Region.description`](Element-Region#description), which will describe the button to screen readers.

button: 
    List (Element.Attribute msg)
    -> { onPress : Maybe msg, label : Element.Element msg }
    -> Element.Element msg
-}
button :
    List Elm.Expression
    -> { onPress : Elm.Expression, label : Elm.Expression }
    -> Elm.Expression
button buttonArg buttonArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onPress", Type.maybe (Type.var "msg") )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list buttonArg
        , Elm.record
            [ Tuple.pair "onPress" buttonArg0.onPress
            , Tuple.pair "label" buttonArg0.label
            ]
        ]


{-| - **onChange** - The `Msg` to send.
  - **icon** - The checkbox icon to show. This can be whatever you'd like, but `Input.defaultCheckbox` is included to get you started.
  - **checked** - The current checked state.
  - **label** - The [`Label`](#Label) for this checkbox

checkbox: 
    List (Element.Attribute msg)
    -> { onChange : Bool -> msg
    , icon : Bool -> Element.Element msg
    , checked : Bool
    , label : Element.Input.Label msg
    }
    -> Element.Element msg
-}
checkbox :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , icon : Elm.Expression -> Elm.Expression
    , checked : Bool
    , label : Elm.Expression
    }
    -> Elm.Expression
checkbox checkboxArg checkboxArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "checkbox"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.bool ] (Type.var "msg")
                              )
                            , ( "icon"
                              , Type.function
                                    [ Type.bool ]
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "checked", Type.bool )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list checkboxArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced "checkboxUnpack" checkboxArg0.onChange)
            , Tuple.pair
                "icon"
                (Elm.functionReduced "checkboxUnpack" checkboxArg0.icon)
            , Tuple.pair "checked" (Elm.bool checkboxArg0.checked)
            , Tuple.pair "label" checkboxArg0.label
            ]
        ]


{-| The blue default checked box icon.

You'll likely want to make your own checkbox at some point that fits your design.

defaultCheckbox: Bool -> Element.Element msg
-}
defaultCheckbox : Bool -> Elm.Expression
defaultCheckbox defaultCheckboxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "defaultCheckbox"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.bool defaultCheckboxArg ]


{-| text: 
    List (Element.Attribute msg)
    -> { onChange : String -> msg
    , text : String
    , placeholder : Maybe (Element.Input.Placeholder msg)
    , label : Element.Input.Label msg
    }
    -> Element.Element msg
-}
text :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , text : String
    , placeholder : Elm.Expression
    , label : Elm.Expression
    }
    -> Elm.Expression
text textArg textArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list textArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced "textUnpack" textArg0.onChange)
            , Tuple.pair "text" (Elm.string textArg0.text)
            , Tuple.pair "placeholder" textArg0.placeholder
            , Tuple.pair "label" textArg0.label
            ]
        ]


{-| A multiline text input.

By default it will have a minimum height of one line and resize based on it's contents.

multiline: 
    List (Element.Attribute msg)
    -> { onChange : String -> msg
    , text : String
    , placeholder : Maybe (Element.Input.Placeholder msg)
    , label : Element.Input.Label msg
    , spellcheck : Bool
    }
    -> Element.Element msg
-}
multiline :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , text : String
    , placeholder : Elm.Expression
    , label : Elm.Expression
    , spellcheck : Bool
    }
    -> Elm.Expression
multiline multilineArg multilineArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "multiline"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            , ( "spellcheck", Type.bool )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list multilineArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced "multilineUnpack" multilineArg0.onChange)
            , Tuple.pair "text" (Elm.string multilineArg0.text)
            , Tuple.pair "placeholder" multilineArg0.placeholder
            , Tuple.pair "label" multilineArg0.label
            , Tuple.pair "spellcheck" (Elm.bool multilineArg0.spellcheck)
            ]
        ]


{-| placeholder: 
    List (Element.Attribute msg)
    -> Element.Element msg
    -> Element.Input.Placeholder msg
-}
placeholder : List Elm.Expression -> Elm.Expression -> Elm.Expression
placeholder placeholderArg placeholderArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "placeholder"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Placeholder"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list placeholderArg, placeholderArg0 ]


{-| username: 
    List (Element.Attribute msg)
    -> { onChange : String -> msg
    , text : String
    , placeholder : Maybe (Element.Input.Placeholder msg)
    , label : Element.Input.Label msg
    }
    -> Element.Element msg
-}
username :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , text : String
    , placeholder : Elm.Expression
    , label : Elm.Expression
    }
    -> Elm.Expression
username usernameArg usernameArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "username"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list usernameArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced "usernameUnpack" usernameArg0.onChange)
            , Tuple.pair "text" (Elm.string usernameArg0.text)
            , Tuple.pair "placeholder" usernameArg0.placeholder
            , Tuple.pair "label" usernameArg0.label
            ]
        ]


{-| A password input that allows the browser to autofill.

It's `newPassword` instead of just `password` because it gives the browser a hint on what type of password input it is.

A password takes all the arguments a normal `Input.text` would, and also **show**, which will remove the password mask (e.g. `****` vs `pass1234`)

newPassword: 
    List (Element.Attribute msg)
    -> { onChange : String -> msg
    , text : String
    , placeholder : Maybe (Element.Input.Placeholder msg)
    , label : Element.Input.Label msg
    , show : Bool
    }
    -> Element.Element msg
-}
newPassword :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , text : String
    , placeholder : Elm.Expression
    , label : Elm.Expression
    , show : Bool
    }
    -> Elm.Expression
newPassword newPasswordArg newPasswordArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "newPassword"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            , ( "show", Type.bool )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list newPasswordArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced
                    "newPasswordUnpack"
                    newPasswordArg0.onChange
                )
            , Tuple.pair "text" (Elm.string newPasswordArg0.text)
            , Tuple.pair "placeholder" newPasswordArg0.placeholder
            , Tuple.pair "label" newPasswordArg0.label
            , Tuple.pair "show" (Elm.bool newPasswordArg0.show)
            ]
        ]


{-| currentPassword: 
    List (Element.Attribute msg)
    -> { onChange : String -> msg
    , text : String
    , placeholder : Maybe (Element.Input.Placeholder msg)
    , label : Element.Input.Label msg
    , show : Bool
    }
    -> Element.Element msg
-}
currentPassword :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , text : String
    , placeholder : Elm.Expression
    , label : Elm.Expression
    , show : Bool
    }
    -> Elm.Expression
currentPassword currentPasswordArg currentPasswordArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "currentPassword"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            , ( "show", Type.bool )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list currentPasswordArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced
                    "currentPasswordUnpack"
                    currentPasswordArg0.onChange
                )
            , Tuple.pair "text" (Elm.string currentPasswordArg0.text)
            , Tuple.pair "placeholder" currentPasswordArg0.placeholder
            , Tuple.pair "label" currentPasswordArg0.label
            , Tuple.pair "show" (Elm.bool currentPasswordArg0.show)
            ]
        ]


{-| email: 
    List (Element.Attribute msg)
    -> { onChange : String -> msg
    , text : String
    , placeholder : Maybe (Element.Input.Placeholder msg)
    , label : Element.Input.Label msg
    }
    -> Element.Element msg
-}
email :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , text : String
    , placeholder : Elm.Expression
    , label : Elm.Expression
    }
    -> Elm.Expression
email emailArg emailArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "email"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list emailArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced "emailUnpack" emailArg0.onChange)
            , Tuple.pair "text" (Elm.string emailArg0.text)
            , Tuple.pair "placeholder" emailArg0.placeholder
            , Tuple.pair "label" emailArg0.label
            ]
        ]


{-| search: 
    List (Element.Attribute msg)
    -> { onChange : String -> msg
    , text : String
    , placeholder : Maybe (Element.Input.Placeholder msg)
    , label : Element.Input.Label msg
    }
    -> Element.Element msg
-}
search :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , text : String
    , placeholder : Elm.Expression
    , label : Elm.Expression
    }
    -> Elm.Expression
search searchArg searchArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "search"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list searchArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced "searchUnpack" searchArg0.onChange)
            , Tuple.pair "text" (Elm.string searchArg0.text)
            , Tuple.pair "placeholder" searchArg0.placeholder
            , Tuple.pair "label" searchArg0.label
            ]
        ]


{-| If spell checking is available, this input will be spellchecked.

spellChecked: 
    List (Element.Attribute msg)
    -> { onChange : String -> msg
    , text : String
    , placeholder : Maybe (Element.Input.Placeholder msg)
    , label : Element.Input.Label msg
    }
    -> Element.Element msg
-}
spellChecked :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , text : String
    , placeholder : Elm.Expression
    , label : Elm.Expression
    }
    -> Elm.Expression
spellChecked spellCheckedArg spellCheckedArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "spellChecked"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list spellCheckedArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced
                    "spellCheckedUnpack"
                    spellCheckedArg0.onChange
                )
            , Tuple.pair "text" (Elm.string spellCheckedArg0.text)
            , Tuple.pair "placeholder" spellCheckedArg0.placeholder
            , Tuple.pair "label" spellCheckedArg0.label
            ]
        ]


{-| A slider input, good for capturing float values.

    Input.slider
        [ Element.height (Element.px 30)

        -- Here is where we're creating/styling the "track"
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color grey
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = AdjustValue
        , label =
            Input.labelAbove []
                (text "My Slider Value")
        , min = 0
        , max = 75
        , step = Nothing
        , value = model.sliderValue
        , thumb =
            Input.defaultThumb
        }

`Element.behindContent` is used to render the track of the slider. Without it, no track would be rendered. The `thumb` is the icon that you can move around.

The slider can be vertical or horizontal depending on the width/height of the slider.

  - `height fill` and `width (px someWidth)` will cause the slider to be vertical.
  - `height (px someHeight)` and `width (px someWidth)` where `someHeight` > `someWidth` will also do it.
  - otherwise, the slider will be horizontal.

**Note** If you want a slider for an `Int` value:

  - set `step` to be `Just 1`, or some other whole value
  - `value = toFloat model.myInt`
  - And finally, round the value before making a message `onChange = round >> AdjustValue`

slider: 
    List (Element.Attribute msg)
    -> { onChange : Float -> msg
    , label : Element.Input.Label msg
    , min : Float
    , max : Float
    , value : Float
    , thumb : Element.Input.Thumb
    , step : Maybe Float
    }
    -> Element.Element msg
-}
slider :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , label : Elm.Expression
    , min : Float
    , max : Float
    , value : Float
    , thumb : Elm.Expression
    , step : Elm.Expression
    }
    -> Elm.Expression
slider sliderArg sliderArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "slider"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.float ] (Type.var "msg")
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            , ( "min", Type.float )
                            , ( "max", Type.float )
                            , ( "value", Type.float )
                            , ( "thumb"
                              , Type.namedWith [ "Element", "Input" ] "Thumb" []
                              )
                            , ( "step", Type.maybe Type.float )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list sliderArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced "sliderUnpack" sliderArg0.onChange)
            , Tuple.pair "label" sliderArg0.label
            , Tuple.pair "min" (Elm.float sliderArg0.min)
            , Tuple.pair "max" (Elm.float sliderArg0.max)
            , Tuple.pair "value" (Elm.float sliderArg0.value)
            , Tuple.pair "thumb" sliderArg0.thumb
            , Tuple.pair "step" sliderArg0.step
            ]
        ]


{-| thumb: List (Element.Attribute Basics.Never) -> Element.Input.Thumb -}
thumb : List Elm.Expression -> Elm.Expression
thumb thumbArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "thumb"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith [ "Element", "Input" ] "Thumb" [])
                    )
            }
        )
        [ Elm.list thumbArg ]


{-| defaultThumb: Element.Input.Thumb -}
defaultThumb : Elm.Expression
defaultThumb =
    Elm.value
        { importFrom = [ "Element", "Input" ]
        , name = "defaultThumb"
        , annotation = Just (Type.namedWith [ "Element", "Input" ] "Thumb" [])
        }


{-| radio: 
    List (Element.Attribute msg)
    -> { onChange : option -> msg
    , options : List (Element.Input.Option option msg)
    , selected : Maybe option
    , label : Element.Input.Label msg
    }
    -> Element.Element msg
-}
radio :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , options : List Elm.Expression
    , selected : Elm.Expression
    , label : Elm.Expression
    }
    -> Elm.Expression
radio radioArg radioArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "radio"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function
                                    [ Type.var "option" ]
                                    (Type.var "msg")
                              )
                            , ( "options"
                              , Type.list
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Option"
                                        [ Type.var "option", Type.var "msg" ]
                                    )
                              )
                            , ( "selected", Type.maybe (Type.var "option") )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list radioArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced "radioUnpack" radioArg0.onChange)
            , Tuple.pair "options" (Elm.list radioArg0.options)
            , Tuple.pair "selected" radioArg0.selected
            , Tuple.pair "label" radioArg0.label
            ]
        ]


{-| Same as radio, but displayed as a row

radioRow: 
    List (Element.Attribute msg)
    -> { onChange : option -> msg
    , options : List (Element.Input.Option option msg)
    , selected : Maybe option
    , label : Element.Input.Label msg
    }
    -> Element.Element msg
-}
radioRow :
    List Elm.Expression
    -> { onChange : Elm.Expression -> Elm.Expression
    , options : List Elm.Expression
    , selected : Elm.Expression
    , label : Elm.Expression
    }
    -> Elm.Expression
radioRow radioRowArg radioRowArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "radioRow"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function
                                    [ Type.var "option" ]
                                    (Type.var "msg")
                              )
                            , ( "options"
                              , Type.list
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Option"
                                        [ Type.var "option", Type.var "msg" ]
                                    )
                              )
                            , ( "selected", Type.maybe (Type.var "option") )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list radioRowArg
        , Elm.record
            [ Tuple.pair
                "onChange"
                (Elm.functionReduced "radioRowUnpack" radioRowArg0.onChange)
            , Tuple.pair "options" (Elm.list radioRowArg0.options)
            , Tuple.pair "selected" radioRowArg0.selected
            , Tuple.pair "label" radioRowArg0.label
            ]
        ]


{-| Add a choice to your radio element. This will be rendered with the default radio icon.

option: value -> Element.Element msg -> Element.Input.Option value msg
-}
option : Elm.Expression -> Elm.Expression -> Elm.Expression
option optionArg optionArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "option"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "value"
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Option"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ optionArg, optionArg0 ]


{-| Customize exactly what your radio option should look like in different states.

optionWith: 
    value
    -> (Element.Input.OptionState -> Element.Element msg)
    -> Element.Input.Option value msg
-}
optionWith :
    Elm.Expression -> (Elm.Expression -> Elm.Expression) -> Elm.Expression
optionWith optionWithArg optionWithArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "optionWith"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "value"
                        , Type.function
                            [ Type.namedWith
                                [ "Element", "Input" ]
                                "OptionState"
                                []
                            ]
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Option"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
        )
        [ optionWithArg, Elm.functionReduced "optionWithUnpack" optionWithArg0 ]


{-| labelAbove: List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Label msg -}
labelAbove : List Elm.Expression -> Elm.Expression -> Elm.Expression
labelAbove labelAboveArg labelAboveArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "labelAbove"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Label"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list labelAboveArg, labelAboveArg0 ]


{-| labelBelow: List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Label msg -}
labelBelow : List Elm.Expression -> Elm.Expression -> Elm.Expression
labelBelow labelBelowArg labelBelowArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "labelBelow"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Label"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list labelBelowArg, labelBelowArg0 ]


{-| labelLeft: List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Label msg -}
labelLeft : List Elm.Expression -> Elm.Expression -> Elm.Expression
labelLeft labelLeftArg labelLeftArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "labelLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Label"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list labelLeftArg, labelLeftArg0 ]


{-| labelRight: List (Element.Attribute msg) -> Element.Element msg -> Element.Input.Label msg -}
labelRight : List Elm.Expression -> Elm.Expression -> Elm.Expression
labelRight labelRightArg labelRightArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "labelRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Label"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list labelRightArg, labelRightArg0 ]


{-| Sometimes you may need to have a label which is not visible, but is still accessible to screen readers.

Seriously consider a visible label before using this.

The situations where a hidden label makes sense:

  - A searchbar with a `search` button right next to it.
  - A `table` of inputs where the header gives the label.

Basically, a hidden label works when there are other contextual clues that sighted people can pick up on.

labelHidden: String -> Element.Input.Label msg
-}
labelHidden : String -> Elm.Expression
labelHidden labelHiddenArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "labelHidden"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Label"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string labelHiddenArg ]


annotation_ :
    { placeholder : Type.Annotation -> Type.Annotation
    , thumb : Type.Annotation
    , option : Type.Annotation -> Type.Annotation -> Type.Annotation
    , optionState : Type.Annotation
    , label : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { placeholder =
        \placeholderArg0 ->
            Type.namedWith
                [ "Element", "Input" ]
                "Placeholder"
                [ placeholderArg0 ]
    , thumb = Type.namedWith [ "Element", "Input" ] "Thumb" []
    , option =
        \optionArg0 optionArg1 ->
            Type.namedWith
                [ "Element", "Input" ]
                "Option"
                [ optionArg0, optionArg1 ]
    , optionState = Type.namedWith [ "Element", "Input" ] "OptionState" []
    , label =
        \labelArg0 ->
            Type.namedWith [ "Element", "Input" ] "Label" [ labelArg0 ]
    }


make_ :
    { idle : Elm.Expression
    , focused : Elm.Expression
    , selected : Elm.Expression
    }
make_ =
    { idle =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "Idle"
            , annotation = Just (Type.namedWith [] "OptionState" [])
            }
    , focused =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "Focused"
            , annotation = Just (Type.namedWith [] "OptionState" [])
            }
    , selected =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "Selected"
            , annotation = Just (Type.namedWith [] "OptionState" [])
            }
    }


caseOf_ :
    { optionState :
        Elm.Expression
        -> { optionStateTags_0_0
            | idle : Elm.Expression
            , focused : Elm.Expression
            , selected : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { optionState =
        \optionStateExpression optionStateTags ->
            Elm.Case.custom
                optionStateExpression
                (Type.namedWith [ "Element", "Input" ] "OptionState" [])
                [ Elm.Case.branch0 "Idle" optionStateTags.idle
                , Elm.Case.branch0 "Focused" optionStateTags.focused
                , Elm.Case.branch0 "Selected" optionStateTags.selected
                ]
    }


call_ :
    { button : Elm.Expression -> Elm.Expression -> Elm.Expression
    , checkbox : Elm.Expression -> Elm.Expression -> Elm.Expression
    , defaultCheckbox : Elm.Expression -> Elm.Expression
    , text : Elm.Expression -> Elm.Expression -> Elm.Expression
    , multiline : Elm.Expression -> Elm.Expression -> Elm.Expression
    , placeholder : Elm.Expression -> Elm.Expression -> Elm.Expression
    , username : Elm.Expression -> Elm.Expression -> Elm.Expression
    , newPassword : Elm.Expression -> Elm.Expression -> Elm.Expression
    , currentPassword : Elm.Expression -> Elm.Expression -> Elm.Expression
    , email : Elm.Expression -> Elm.Expression -> Elm.Expression
    , search : Elm.Expression -> Elm.Expression -> Elm.Expression
    , spellChecked : Elm.Expression -> Elm.Expression -> Elm.Expression
    , slider : Elm.Expression -> Elm.Expression -> Elm.Expression
    , thumb : Elm.Expression -> Elm.Expression
    , radio : Elm.Expression -> Elm.Expression -> Elm.Expression
    , radioRow : Elm.Expression -> Elm.Expression -> Elm.Expression
    , option : Elm.Expression -> Elm.Expression -> Elm.Expression
    , optionWith : Elm.Expression -> Elm.Expression -> Elm.Expression
    , labelAbove : Elm.Expression -> Elm.Expression -> Elm.Expression
    , labelBelow : Elm.Expression -> Elm.Expression -> Elm.Expression
    , labelLeft : Elm.Expression -> Elm.Expression -> Elm.Expression
    , labelRight : Elm.Expression -> Elm.Expression -> Elm.Expression
    , labelHidden : Elm.Expression -> Elm.Expression
    }
call_ =
    { button =
        \buttonArg buttonArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "button"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onPress", Type.maybe (Type.var "msg") )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element" ]
                                            "Element"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ buttonArg, buttonArg0 ]
    , checkbox =
        \checkboxArg checkboxArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "checkbox"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.bool ]
                                            (Type.var "msg")
                                      )
                                    , ( "icon"
                                      , Type.function
                                            [ Type.bool ]
                                            (Type.namedWith
                                                [ "Element" ]
                                                "Element"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "checked", Type.bool )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ checkboxArg, checkboxArg0 ]
    , defaultCheckbox =
        \defaultCheckboxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "defaultCheckbox"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ defaultCheckboxArg ]
    , text =
        \textArg textArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "text"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
                                    , ( "text", Type.string )
                                    , ( "placeholder"
                                      , Type.maybe
                                            (Type.namedWith
                                                [ "Element", "Input" ]
                                                "Placeholder"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ textArg, textArg0 ]
    , multiline =
        \multilineArg multilineArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "multiline"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
                                    , ( "text", Type.string )
                                    , ( "placeholder"
                                      , Type.maybe
                                            (Type.namedWith
                                                [ "Element", "Input" ]
                                                "Placeholder"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    , ( "spellcheck", Type.bool )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ multilineArg, multilineArg0 ]
    , placeholder =
        \placeholderArg placeholderArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "placeholder"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element", "Input" ]
                                    "Placeholder"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ placeholderArg, placeholderArg0 ]
    , username =
        \usernameArg usernameArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "username"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
                                    , ( "text", Type.string )
                                    , ( "placeholder"
                                      , Type.maybe
                                            (Type.namedWith
                                                [ "Element", "Input" ]
                                                "Placeholder"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ usernameArg, usernameArg0 ]
    , newPassword =
        \newPasswordArg newPasswordArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "newPassword"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
                                    , ( "text", Type.string )
                                    , ( "placeholder"
                                      , Type.maybe
                                            (Type.namedWith
                                                [ "Element", "Input" ]
                                                "Placeholder"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    , ( "show", Type.bool )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ newPasswordArg, newPasswordArg0 ]
    , currentPassword =
        \currentPasswordArg currentPasswordArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "currentPassword"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
                                    , ( "text", Type.string )
                                    , ( "placeholder"
                                      , Type.maybe
                                            (Type.namedWith
                                                [ "Element", "Input" ]
                                                "Placeholder"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    , ( "show", Type.bool )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ currentPasswordArg, currentPasswordArg0 ]
    , email =
        \emailArg emailArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "email"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
                                    , ( "text", Type.string )
                                    , ( "placeholder"
                                      , Type.maybe
                                            (Type.namedWith
                                                [ "Element", "Input" ]
                                                "Placeholder"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ emailArg, emailArg0 ]
    , search =
        \searchArg searchArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "search"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
                                    , ( "text", Type.string )
                                    , ( "placeholder"
                                      , Type.maybe
                                            (Type.namedWith
                                                [ "Element", "Input" ]
                                                "Placeholder"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ searchArg, searchArg0 ]
    , spellChecked =
        \spellCheckedArg spellCheckedArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "spellChecked"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.string ]
                                            (Type.var "msg")
                                      )
                                    , ( "text", Type.string )
                                    , ( "placeholder"
                                      , Type.maybe
                                            (Type.namedWith
                                                [ "Element", "Input" ]
                                                "Placeholder"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ spellCheckedArg, spellCheckedArg0 ]
    , slider =
        \sliderArg sliderArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "slider"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.float ]
                                            (Type.var "msg")
                                      )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    , ( "min", Type.float )
                                    , ( "max", Type.float )
                                    , ( "value", Type.float )
                                    , ( "thumb"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Thumb"
                                            []
                                      )
                                    , ( "step", Type.maybe Type.float )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ sliderArg, sliderArg0 ]
    , thumb =
        \thumbArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "thumb"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.namedWith [ "Basics" ] "Never" []
                                        ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element", "Input" ]
                                    "Thumb"
                                    []
                                )
                            )
                    }
                )
                [ thumbArg ]
    , radio =
        \radioArg radioArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "radio"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.var "option" ]
                                            (Type.var "msg")
                                      )
                                    , ( "options"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Element", "Input" ]
                                                "Option"
                                                [ Type.var "option"
                                                , Type.var "msg"
                                                ]
                                            )
                                      )
                                    , ( "selected"
                                      , Type.maybe (Type.var "option")
                                      )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ radioArg, radioArg0 ]
    , radioRow =
        \radioRowArg radioRowArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "radioRow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.record
                                    [ ( "onChange"
                                      , Type.function
                                            [ Type.var "option" ]
                                            (Type.var "msg")
                                      )
                                    , ( "options"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Element", "Input" ]
                                                "Option"
                                                [ Type.var "option"
                                                , Type.var "msg"
                                                ]
                                            )
                                      )
                                    , ( "selected"
                                      , Type.maybe (Type.var "option")
                                      )
                                    , ( "label"
                                      , Type.namedWith
                                            [ "Element", "Input" ]
                                            "Label"
                                            [ Type.var "msg" ]
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ radioRowArg, radioRowArg0 ]
    , option =
        \optionArg optionArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "option"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "value"
                                , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element", "Input" ]
                                    "Option"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ optionArg, optionArg0 ]
    , optionWith =
        \optionWithArg optionWithArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "optionWith"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "value"
                                , Type.function
                                    [ Type.namedWith
                                        [ "Element", "Input" ]
                                        "OptionState"
                                        []
                                    ]
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element", "Input" ]
                                    "Option"
                                    [ Type.var "value", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ optionWithArg, optionWithArg0 ]
    , labelAbove =
        \labelAboveArg labelAboveArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "labelAbove"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelAboveArg, labelAboveArg0 ]
    , labelBelow =
        \labelBelowArg labelBelowArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "labelBelow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelBelowArg, labelBelowArg0 ]
    , labelLeft =
        \labelLeftArg labelLeftArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "labelLeft"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelLeftArg, labelLeftArg0 ]
    , labelRight =
        \labelRightArg labelRightArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "labelRight"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelRightArg, labelRightArg0 ]
    , labelHidden =
        \labelHiddenArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Input" ]
                    , name = "labelHidden"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ labelHiddenArg ]
    }


values_ :
    { focusedOnLoad : Elm.Expression
    , button : Elm.Expression
    , checkbox : Elm.Expression
    , defaultCheckbox : Elm.Expression
    , text : Elm.Expression
    , multiline : Elm.Expression
    , placeholder : Elm.Expression
    , username : Elm.Expression
    , newPassword : Elm.Expression
    , currentPassword : Elm.Expression
    , email : Elm.Expression
    , search : Elm.Expression
    , spellChecked : Elm.Expression
    , slider : Elm.Expression
    , thumb : Elm.Expression
    , defaultThumb : Elm.Expression
    , radio : Elm.Expression
    , radioRow : Elm.Expression
    , option : Elm.Expression
    , optionWith : Elm.Expression
    , labelAbove : Elm.Expression
    , labelBelow : Elm.Expression
    , labelLeft : Elm.Expression
    , labelRight : Elm.Expression
    , labelHidden : Elm.Expression
    }
values_ =
    { focusedOnLoad =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "focusedOnLoad"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , button =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "button"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onPress", Type.maybe (Type.var "msg") )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , checkbox =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "checkbox"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.bool ] (Type.var "msg")
                              )
                            , ( "icon"
                              , Type.function
                                    [ Type.bool ]
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "checked", Type.bool )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , defaultCheckbox =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "defaultCheckbox"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , text =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , multiline =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "multiline"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            , ( "spellcheck", Type.bool )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , placeholder =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "placeholder"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Placeholder"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , username =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "username"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , newPassword =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "newPassword"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            , ( "show", Type.bool )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , currentPassword =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "currentPassword"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            , ( "show", Type.bool )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , email =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "email"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , search =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "search"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , spellChecked =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "spellChecked"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.string ] (Type.var "msg")
                              )
                            , ( "text", Type.string )
                            , ( "placeholder"
                              , Type.maybe
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Placeholder"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , slider =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "slider"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function [ Type.float ] (Type.var "msg")
                              )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            , ( "min", Type.float )
                            , ( "max", Type.float )
                            , ( "value", Type.float )
                            , ( "thumb"
                              , Type.namedWith [ "Element", "Input" ] "Thumb" []
                              )
                            , ( "step", Type.maybe Type.float )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , thumb =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "thumb"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.namedWith [ "Basics" ] "Never" [] ]
                            )
                        ]
                        (Type.namedWith [ "Element", "Input" ] "Thumb" [])
                    )
            }
    , defaultThumb =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "defaultThumb"
            , annotation =
                Just (Type.namedWith [ "Element", "Input" ] "Thumb" [])
            }
    , radio =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "radio"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function
                                    [ Type.var "option" ]
                                    (Type.var "msg")
                              )
                            , ( "options"
                              , Type.list
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Option"
                                        [ Type.var "option", Type.var "msg" ]
                                    )
                              )
                            , ( "selected", Type.maybe (Type.var "option") )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , radioRow =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "radioRow"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.record
                            [ ( "onChange"
                              , Type.function
                                    [ Type.var "option" ]
                                    (Type.var "msg")
                              )
                            , ( "options"
                              , Type.list
                                    (Type.namedWith
                                        [ "Element", "Input" ]
                                        "Option"
                                        [ Type.var "option", Type.var "msg" ]
                                    )
                              )
                            , ( "selected", Type.maybe (Type.var "option") )
                            , ( "label"
                              , Type.namedWith
                                    [ "Element", "Input" ]
                                    "Label"
                                    [ Type.var "msg" ]
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , option =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "option"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "value"
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Option"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , optionWith =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "optionWith"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "value"
                        , Type.function
                            [ Type.namedWith
                                [ "Element", "Input" ]
                                "OptionState"
                                []
                            ]
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Option"
                            [ Type.var "value", Type.var "msg" ]
                        )
                    )
            }
    , labelAbove =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "labelAbove"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Label"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , labelBelow =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "labelBelow"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Label"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , labelLeft =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "labelLeft"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Label"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , labelRight =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "labelRight"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Label"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , labelHidden =
        Elm.value
            { importFrom = [ "Element", "Input" ]
            , name = "labelHidden"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Element", "Input" ]
                            "Label"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


