module Gen.Element.Keyed exposing (call_, column, el, moduleName_, row, values_)

{-| 
@docs values_, call_, row, column, el, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Element", "Keyed" ]


{-| el: 
    List (Element.Attribute msg)
    -> ( String, Element.Element msg )
    -> Element.Element msg
-}
el : List Elm.Expression -> Elm.Expression -> Elm.Expression
el elArg elArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Keyed" ]
            , name = "el"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.tuple
                            Type.string
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list elArg, elArg0 ]


{-| column: 
    List (Element.Attribute msg)
    -> List ( String, Element.Element msg )
    -> Element.Element msg
-}
column : List Elm.Expression -> List Elm.Expression -> Elm.Expression
column columnArg columnArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Keyed" ]
            , name = "column"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list columnArg, Elm.list columnArg0 ]


{-| row: 
    List (Element.Attribute msg)
    -> List ( String, Element.Element msg )
    -> Element.Element msg
-}
row : List Elm.Expression -> List Elm.Expression -> Elm.Expression
row rowArg rowArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Keyed" ]
            , name = "row"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list rowArg, Elm.list rowArg0 ]


call_ :
    { el : Elm.Expression -> Elm.Expression -> Elm.Expression
    , column : Elm.Expression -> Elm.Expression -> Elm.Expression
    , row : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { el =
        \elArg elArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Keyed" ]
                    , name = "el"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.tuple
                                    Type.string
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Element"
                                        [ Type.var "msg" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ elArg, elArg0 ]
    , column =
        \columnArg columnArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Keyed" ]
                    , name = "column"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.tuple
                                        Type.string
                                        (Type.namedWith
                                            [ "Element" ]
                                            "Element"
                                            [ Type.var "msg" ]
                                        )
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ columnArg, columnArg0 ]
    , row =
        \rowArg rowArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Keyed" ]
                    , name = "row"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element" ]
                                        "Attribute"
                                        [ Type.var "msg" ]
                                    )
                                , Type.list
                                    (Type.tuple
                                        Type.string
                                        (Type.namedWith
                                            [ "Element" ]
                                            "Element"
                                            [ Type.var "msg" ]
                                        )
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ rowArg, rowArg0 ]
    }


values_ : { el : Elm.Expression, column : Elm.Expression, row : Elm.Expression }
values_ =
    { el =
        Elm.value
            { importFrom = [ "Element", "Keyed" ]
            , name = "el"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.tuple
                            Type.string
                            (Type.namedWith
                                [ "Element" ]
                                "Element"
                                [ Type.var "msg" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , column =
        Elm.value
            { importFrom = [ "Element", "Keyed" ]
            , name = "column"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , row =
        Elm.value
            { importFrom = [ "Element", "Keyed" ]
            , name = "row"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Element" ]
                                "Attribute"
                                [ Type.var "msg" ]
                            )
                        , Type.list
                            (Type.tuple
                                Type.string
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


