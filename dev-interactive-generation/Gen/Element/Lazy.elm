module Gen.Element.Lazy exposing (call_, lazy, lazy2, lazy3, lazy4, lazy5, moduleName_, values_)

{-| 
@docs values_, call_, lazy5, lazy4, lazy3, lazy2, lazy, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Element", "Lazy" ]


{-| lazy: (a -> Internal.Model.Element msg) -> a -> Internal.Model.Element msg -}
lazy : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
lazy lazyArg lazyArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Lazy" ]
            , name = "lazy"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith
                                [ "Internal", "Model" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        , Type.var "a"
                        ]
                        (Type.namedWith
                            [ "Internal", "Model" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "lazyUnpack" lazyArg, lazyArg0 ]


{-| lazy2: (a -> b -> Internal.Model.Element msg) -> a -> b -> Internal.Model.Element msg -}
lazy2 :
    (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
lazy2 lazy2Arg lazy2Arg0 lazy2Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Lazy" ]
            , name = "lazy2"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b" ]
                            (Type.namedWith
                                [ "Internal", "Model" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        , Type.var "a"
                        , Type.var "b"
                        ]
                        (Type.namedWith
                            [ "Internal", "Model" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced
            "lazy2Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced "unpack" (lazy2Arg functionReducedUnpack)
            )
        , lazy2Arg0
        , lazy2Arg1
        ]


{-| lazy3: 
    (a -> b -> c -> Internal.Model.Element msg)
    -> a
    -> b
    -> c
    -> Internal.Model.Element msg
-}
lazy3 :
    (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
lazy3 lazy3Arg lazy3Arg0 lazy3Arg1 lazy3Arg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Lazy" ]
            , name = "lazy3"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b", Type.var "c" ]
                            (Type.namedWith
                                [ "Internal", "Model" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        , Type.var "a"
                        , Type.var "b"
                        , Type.var "c"
                        ]
                        (Type.namedWith
                            [ "Internal", "Model" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced
            "lazy3Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (lazy3Arg functionReducedUnpack
                                functionReducedUnpack0
                            )
                    )
            )
        , lazy3Arg0
        , lazy3Arg1
        , lazy3Arg2
        ]


{-| lazy4: 
    (a -> b -> c -> d -> Internal.Model.Element msg)
    -> a
    -> b
    -> c
    -> d
    -> Internal.Model.Element msg
-}
lazy4 :
    (Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
lazy4 lazy4Arg lazy4Arg0 lazy4Arg1 lazy4Arg2 lazy4Arg3 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Lazy" ]
            , name = "lazy4"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            ]
                            (Type.namedWith
                                [ "Internal", "Model" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        , Type.var "a"
                        , Type.var "b"
                        , Type.var "c"
                        , Type.var "d"
                        ]
                        (Type.namedWith
                            [ "Internal", "Model" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced
            "lazy4Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_0_2_0_0 ->
                                Elm.functionReduced
                                    "unpack"
                                    (lazy4Arg functionReducedUnpack
                                         functionReducedUnpack0
                                        functionReducedUnpack_2_1_2_0_2_0_2_0_0
                                    )
                            )
                    )
            )
        , lazy4Arg0
        , lazy4Arg1
        , lazy4Arg2
        , lazy4Arg3
        ]


{-| lazy5: 
    (a -> b -> c -> d -> e -> Internal.Model.Element msg)
    -> a
    -> b
    -> c
    -> d
    -> e
    -> Internal.Model.Element msg
-}
lazy5 :
    (Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
lazy5 lazy5Arg lazy5Arg0 lazy5Arg1 lazy5Arg2 lazy5Arg3 lazy5Arg4 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Lazy" ]
            , name = "lazy5"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            , Type.var "e"
                            ]
                            (Type.namedWith
                                [ "Internal", "Model" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        , Type.var "a"
                        , Type.var "b"
                        , Type.var "c"
                        , Type.var "d"
                        , Type.var "e"
                        ]
                        (Type.namedWith
                            [ "Internal", "Model" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced
            "lazy5Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_0_2_0_0 ->
                                Elm.functionReduced
                                    "unpack"
                                    (\functionReducedUnpack_2_1_2_1_2_0_2_0_2_0_0 ->
                                        Elm.functionReduced
                                            "unpack"
                                            (lazy5Arg functionReducedUnpack
                                                 functionReducedUnpack0
                                                 functionReducedUnpack_2_1_2_0_2_0_2_0_0
                                                functionReducedUnpack_2_1_2_1_2_0_2_0_2_0_0
                                            )
                                    )
                            )
                    )
            )
        , lazy5Arg0
        , lazy5Arg1
        , lazy5Arg2
        , lazy5Arg3
        , lazy5Arg4
        ]


call_ :
    { lazy : Elm.Expression -> Elm.Expression -> Elm.Expression
    , lazy2 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , lazy3 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , lazy4 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , lazy5 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    }
call_ =
    { lazy =
        \lazyArg lazyArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Lazy" ]
                    , name = "lazy"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a" ]
                                    (Type.namedWith
                                        [ "Internal", "Model" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                , Type.var "a"
                                ]
                                (Type.namedWith
                                    [ "Internal", "Model" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ lazyArg, lazyArg0 ]
    , lazy2 =
        \lazy2Arg lazy2Arg0 lazy2Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Lazy" ]
                    , name = "lazy2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a", Type.var "b" ]
                                    (Type.namedWith
                                        [ "Internal", "Model" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                , Type.var "a"
                                , Type.var "b"
                                ]
                                (Type.namedWith
                                    [ "Internal", "Model" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ lazy2Arg, lazy2Arg0, lazy2Arg1 ]
    , lazy3 =
        \lazy3Arg lazy3Arg0 lazy3Arg1 lazy3Arg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Lazy" ]
                    , name = "lazy3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a", Type.var "b", Type.var "c" ]
                                    (Type.namedWith
                                        [ "Internal", "Model" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                , Type.var "a"
                                , Type.var "b"
                                , Type.var "c"
                                ]
                                (Type.namedWith
                                    [ "Internal", "Model" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ lazy3Arg, lazy3Arg0, lazy3Arg1, lazy3Arg2 ]
    , lazy4 =
        \lazy4Arg lazy4Arg0 lazy4Arg1 lazy4Arg2 lazy4Arg3 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Lazy" ]
                    , name = "lazy4"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a"
                                    , Type.var "b"
                                    , Type.var "c"
                                    , Type.var "d"
                                    ]
                                    (Type.namedWith
                                        [ "Internal", "Model" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                , Type.var "a"
                                , Type.var "b"
                                , Type.var "c"
                                , Type.var "d"
                                ]
                                (Type.namedWith
                                    [ "Internal", "Model" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ lazy4Arg, lazy4Arg0, lazy4Arg1, lazy4Arg2, lazy4Arg3 ]
    , lazy5 =
        \lazy5Arg lazy5Arg0 lazy5Arg1 lazy5Arg2 lazy5Arg3 lazy5Arg4 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Lazy" ]
                    , name = "lazy5"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a"
                                    , Type.var "b"
                                    , Type.var "c"
                                    , Type.var "d"
                                    , Type.var "e"
                                    ]
                                    (Type.namedWith
                                        [ "Internal", "Model" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                , Type.var "a"
                                , Type.var "b"
                                , Type.var "c"
                                , Type.var "d"
                                , Type.var "e"
                                ]
                                (Type.namedWith
                                    [ "Internal", "Model" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ lazy5Arg
                , lazy5Arg0
                , lazy5Arg1
                , lazy5Arg2
                , lazy5Arg3
                , lazy5Arg4
                ]
    }


values_ :
    { lazy : Elm.Expression
    , lazy2 : Elm.Expression
    , lazy3 : Elm.Expression
    , lazy4 : Elm.Expression
    , lazy5 : Elm.Expression
    }
values_ =
    { lazy =
        Elm.value
            { importFrom = [ "Element", "Lazy" ]
            , name = "lazy"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a" ]
                            (Type.namedWith
                                [ "Internal", "Model" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        , Type.var "a"
                        ]
                        (Type.namedWith
                            [ "Internal", "Model" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , lazy2 =
        Elm.value
            { importFrom = [ "Element", "Lazy" ]
            , name = "lazy2"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b" ]
                            (Type.namedWith
                                [ "Internal", "Model" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        , Type.var "a"
                        , Type.var "b"
                        ]
                        (Type.namedWith
                            [ "Internal", "Model" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , lazy3 =
        Elm.value
            { importFrom = [ "Element", "Lazy" ]
            , name = "lazy3"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b", Type.var "c" ]
                            (Type.namedWith
                                [ "Internal", "Model" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        , Type.var "a"
                        , Type.var "b"
                        , Type.var "c"
                        ]
                        (Type.namedWith
                            [ "Internal", "Model" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , lazy4 =
        Elm.value
            { importFrom = [ "Element", "Lazy" ]
            , name = "lazy4"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            ]
                            (Type.namedWith
                                [ "Internal", "Model" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        , Type.var "a"
                        , Type.var "b"
                        , Type.var "c"
                        , Type.var "d"
                        ]
                        (Type.namedWith
                            [ "Internal", "Model" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , lazy5 =
        Elm.value
            { importFrom = [ "Element", "Lazy" ]
            , name = "lazy5"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            , Type.var "e"
                            ]
                            (Type.namedWith
                                [ "Internal", "Model" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        , Type.var "a"
                        , Type.var "b"
                        , Type.var "c"
                        , Type.var "d"
                        , Type.var "e"
                        ]
                        (Type.namedWith
                            [ "Internal", "Model" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


