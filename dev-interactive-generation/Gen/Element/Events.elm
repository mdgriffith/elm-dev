module Gen.Element.Events exposing (call_, moduleName_, onClick, onDoubleClick, onFocus, onLoseFocus, onMouseDown, onMouseEnter, onMouseLeave, onMouseMove, onMouseUp, values_)

{-| 
@docs values_, call_, onLoseFocus, onFocus, onMouseMove, onMouseLeave, onMouseEnter, onMouseUp, onMouseDown, onDoubleClick, onClick, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Element", "Events" ]


{-| onClick: msg -> Element.Attribute msg -}
onClick : Elm.Expression -> Elm.Expression
onClick onClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onClickArg ]


{-| onDoubleClick: msg -> Element.Attribute msg -}
onDoubleClick : Elm.Expression -> Elm.Expression
onDoubleClick onDoubleClickArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onDoubleClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onDoubleClickArg ]


{-| onMouseDown: msg -> Element.Attribute msg -}
onMouseDown : Elm.Expression -> Elm.Expression
onMouseDown onMouseDownArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onMouseDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseDownArg ]


{-| onMouseUp: msg -> Element.Attribute msg -}
onMouseUp : Elm.Expression -> Elm.Expression
onMouseUp onMouseUpArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onMouseUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseUpArg ]


{-| onMouseEnter: msg -> Element.Attribute msg -}
onMouseEnter : Elm.Expression -> Elm.Expression
onMouseEnter onMouseEnterArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onMouseEnter"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseEnterArg ]


{-| onMouseLeave: msg -> Element.Attribute msg -}
onMouseLeave : Elm.Expression -> Elm.Expression
onMouseLeave onMouseLeaveArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onMouseLeave"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseLeaveArg ]


{-| onMouseMove: msg -> Element.Attribute msg -}
onMouseMove : Elm.Expression -> Elm.Expression
onMouseMove onMouseMoveArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onMouseMove"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onMouseMoveArg ]


{-| onFocus: msg -> Element.Attribute msg -}
onFocus : Elm.Expression -> Elm.Expression
onFocus onFocusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onFocus"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onFocusArg ]


{-| onLoseFocus: msg -> Element.Attribute msg -}
onLoseFocus : Elm.Expression -> Elm.Expression
onLoseFocus onLoseFocusArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onLoseFocus"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ onLoseFocusArg ]


call_ :
    { onClick : Elm.Expression -> Elm.Expression
    , onDoubleClick : Elm.Expression -> Elm.Expression
    , onMouseDown : Elm.Expression -> Elm.Expression
    , onMouseUp : Elm.Expression -> Elm.Expression
    , onMouseEnter : Elm.Expression -> Elm.Expression
    , onMouseLeave : Elm.Expression -> Elm.Expression
    , onMouseMove : Elm.Expression -> Elm.Expression
    , onFocus : Elm.Expression -> Elm.Expression
    , onLoseFocus : Elm.Expression -> Elm.Expression
    }
call_ =
    { onClick =
        \onClickArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Events" ]
                    , name = "onClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onClickArg ]
    , onDoubleClick =
        \onDoubleClickArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Events" ]
                    , name = "onDoubleClick"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onDoubleClickArg ]
    , onMouseDown =
        \onMouseDownArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Events" ]
                    , name = "onMouseDown"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseDownArg ]
    , onMouseUp =
        \onMouseUpArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Events" ]
                    , name = "onMouseUp"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseUpArg ]
    , onMouseEnter =
        \onMouseEnterArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Events" ]
                    , name = "onMouseEnter"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseEnterArg ]
    , onMouseLeave =
        \onMouseLeaveArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Events" ]
                    , name = "onMouseLeave"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseLeaveArg ]
    , onMouseMove =
        \onMouseMoveArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Events" ]
                    , name = "onMouseMove"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onMouseMoveArg ]
    , onFocus =
        \onFocusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Events" ]
                    , name = "onFocus"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onFocusArg ]
    , onLoseFocus =
        \onLoseFocusArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Events" ]
                    , name = "onLoseFocus"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "msg" ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ onLoseFocusArg ]
    }


values_ :
    { onClick : Elm.Expression
    , onDoubleClick : Elm.Expression
    , onMouseDown : Elm.Expression
    , onMouseUp : Elm.Expression
    , onMouseEnter : Elm.Expression
    , onMouseLeave : Elm.Expression
    , onMouseMove : Elm.Expression
    , onFocus : Elm.Expression
    , onLoseFocus : Elm.Expression
    }
values_ =
    { onClick =
        Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onDoubleClick =
        Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onDoubleClick"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseDown =
        Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onMouseDown"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseUp =
        Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onMouseUp"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseEnter =
        Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onMouseEnter"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseLeave =
        Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onMouseLeave"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onMouseMove =
        Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onMouseMove"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onFocus =
        Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onFocus"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , onLoseFocus =
        Elm.value
            { importFrom = [ "Element", "Events" ]
            , name = "onLoseFocus"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "msg" ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


