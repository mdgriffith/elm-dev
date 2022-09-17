module Gen.Ui exposing (call_, card, code, moduleName_, values_)

{-| 
@docs values_, call_, code, card, moduleName_
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


call_ :
    { card : Elm.Expression -> Elm.Expression
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


values_ : { card : Elm.Expression, code : Elm.Expression }
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
    }


