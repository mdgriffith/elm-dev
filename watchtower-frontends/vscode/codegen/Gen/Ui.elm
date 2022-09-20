module Gen.Ui exposing (call_, card, code, divider, lightCode, moduleName_, pointer, values_)

{-| 
@docs values_, call_, divider, pointer, code, lightCode, card, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Ui" ]


{-| card: List (Html msg) -> Html msg -}
card : List Elm.Expression -> Elm.Expression
card cardArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ui" ]
            , name = "card"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Html" [ Type.var "msg" ])
                        ]
                        (Type.namedWith [] "Html" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.list cardArg ]


{-| lightCode: String -> Element.Element msg -}
lightCode : String -> Elm.Expression
lightCode lightCodeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ui" ]
            , name = "lightCode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string lightCodeArg ]


{-| code: String -> String -> Element.Element msg -}
code : String -> String -> Elm.Expression
code codeArg codeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ui" ]
            , name = "code"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string codeArg, Elm.string codeArg0 ]


{-| pointer: Element.Attribute msg -}
pointer : Elm.Expression
pointer =
    Elm.value
        { importFrom = [ "Ui" ]
        , name = "pointer"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| divider: Element.Element msg -}
divider : Elm.Expression
divider =
    Elm.value
        { importFrom = [ "Ui" ]
        , name = "divider"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Element" [ Type.var "msg" ])
        }


call_ :
    { card : Elm.Expression -> Elm.Expression
    , lightCode : Elm.Expression -> Elm.Expression
    , code : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { card =
        \cardArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ui" ]
                    , name = "card"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith [] "Html" [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith [] "Html" [ Type.var "msg" ])
                            )
                    }
                )
                [ cardArg ]
    , lightCode =
        \lightCodeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ui" ]
                    , name = "lightCode"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ lightCodeArg ]
    , code =
        \codeArg codeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ui" ]
                    , name = "code"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.string ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ codeArg, codeArg0 ]
    }


values_ :
    { card : Elm.Expression
    , lightCode : Elm.Expression
    , code : Elm.Expression
    , pointer : Elm.Expression
    , divider : Elm.Expression
    }
values_ =
    { card =
        Elm.value
            { importFrom = [ "Ui" ]
            , name = "card"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [] "Html" [ Type.var "msg" ])
                        ]
                        (Type.namedWith [] "Html" [ Type.var "msg" ])
                    )
            }
    , lightCode =
        Elm.value
            { importFrom = [ "Ui" ]
            , name = "lightCode"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , code =
        Elm.value
            { importFrom = [ "Ui" ]
            , name = "code"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , pointer =
        Elm.value
            { importFrom = [ "Ui" ]
            , name = "pointer"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , divider =
        Elm.value
            { importFrom = [ "Ui" ]
            , name = "divider"
            , annotation =
                Just (Type.namedWith [ "Element" ] "Element" [ Type.var "msg" ])
            }
    }


