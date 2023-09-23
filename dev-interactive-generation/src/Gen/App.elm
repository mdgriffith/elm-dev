module Gen.App exposing (annotation_, call_, element, moduleName_, values_)

{-| 
@docs values_, call_, annotation_, element, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "App" ]


{-| element: 
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Prog flags model msg
-}
element :
    { init : Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression
    , update : Elm.Expression -> Elm.Expression -> Elm.Expression
    , subscriptions : Elm.Expression -> Elm.Expression
    }
    -> Elm.Expression
element elementArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "App" ]
            , name = "element"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "init"
                              , Type.function
                                    [ Type.var "flags" ]
                                    (Type.tuple
                                        (Type.var "model")
                                        (Type.namedWith
                                            []
                                            "Cmd"
                                            [ Type.var "msg" ]
                                        )
                                    )
                              )
                            , ( "view"
                              , Type.function
                                    [ Type.var "model" ]
                                    (Type.namedWith [] "Html" [ Type.var "msg" ]
                                    )
                              )
                            , ( "update"
                              , Type.function
                                    [ Type.var "msg", Type.var "model" ]
                                    (Type.tuple
                                        (Type.var "model")
                                        (Type.namedWith
                                            []
                                            "Cmd"
                                            [ Type.var "msg" ]
                                        )
                                    )
                              )
                            , ( "subscriptions"
                              , Type.function
                                    [ Type.var "model" ]
                                    (Type.namedWith [] "Sub" [ Type.var "msg" ])
                              )
                            ]
                        ]
                        (Type.namedWith
                            []
                            "Prog"
                            [ Type.var "flags"
                            , Type.var "model"
                            , Type.var "msg"
                            ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair
                "init"
                (Elm.functionReduced "elementUnpack" elementArg.init)
            , Tuple.pair
                "view"
                (Elm.functionReduced "elementUnpack" elementArg.view)
            , Tuple.pair
                "update"
                (Elm.functionReduced
                    "elementUnpack"
                    (\functionReducedUnpack ->
                        Elm.functionReduced
                            "unpack"
                            (elementArg.update functionReducedUnpack)
                    )
                )
            , Tuple.pair
                "subscriptions"
                (Elm.functionReduced "elementUnpack" elementArg.subscriptions)
            ]
        ]


annotation_ :
    { prog :
        Type.Annotation -> Type.Annotation -> Type.Annotation -> Type.Annotation
    }
annotation_ =
    { prog =
        \progArg0 progArg1 progArg2 ->
            Type.alias
                moduleName_
                "Prog"
                [ progArg0, progArg1, progArg2 ]
                (Type.namedWith
                    []
                    "Program"
                    [ Type.var "flags"
                    , Type.namedWith [] "Model" [ Type.var "model" ]
                    , Type.namedWith [] "Msg" [ Type.var "msg" ]
                    ]
                )
    }


call_ : { element : Elm.Expression -> Elm.Expression }
call_ =
    { element =
        \elementArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "App" ]
                    , name = "element"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "init"
                                      , Type.function
                                            [ Type.var "flags" ]
                                            (Type.tuple
                                                (Type.var "model")
                                                (Type.namedWith
                                                    []
                                                    "Cmd"
                                                    [ Type.var "msg" ]
                                                )
                                            )
                                      )
                                    , ( "view"
                                      , Type.function
                                            [ Type.var "model" ]
                                            (Type.namedWith
                                                []
                                                "Html"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "update"
                                      , Type.function
                                            [ Type.var "msg", Type.var "model" ]
                                            (Type.tuple
                                                (Type.var "model")
                                                (Type.namedWith
                                                    []
                                                    "Cmd"
                                                    [ Type.var "msg" ]
                                                )
                                            )
                                      )
                                    , ( "subscriptions"
                                      , Type.function
                                            [ Type.var "model" ]
                                            (Type.namedWith
                                                []
                                                "Sub"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    []
                                    "Prog"
                                    [ Type.var "flags"
                                    , Type.var "model"
                                    , Type.var "msg"
                                    ]
                                )
                            )
                    }
                )
                [ elementArg ]
    }


values_ : { element : Elm.Expression }
values_ =
    { element =
        Elm.value
            { importFrom = [ "App" ]
            , name = "element"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "init"
                              , Type.function
                                    [ Type.var "flags" ]
                                    (Type.tuple
                                        (Type.var "model")
                                        (Type.namedWith
                                            []
                                            "Cmd"
                                            [ Type.var "msg" ]
                                        )
                                    )
                              )
                            , ( "view"
                              , Type.function
                                    [ Type.var "model" ]
                                    (Type.namedWith [] "Html" [ Type.var "msg" ]
                                    )
                              )
                            , ( "update"
                              , Type.function
                                    [ Type.var "msg", Type.var "model" ]
                                    (Type.tuple
                                        (Type.var "model")
                                        (Type.namedWith
                                            []
                                            "Cmd"
                                            [ Type.var "msg" ]
                                        )
                                    )
                              )
                            , ( "subscriptions"
                              , Type.function
                                    [ Type.var "model" ]
                                    (Type.namedWith [] "Sub" [ Type.var "msg" ])
                              )
                            ]
                        ]
                        (Type.namedWith
                            []
                            "Prog"
                            [ Type.var "flags"
                            , Type.var "model"
                            , Type.var "msg"
                            ]
                        )
                    )
            }
    }


