module Gen.Ui.Input exposing (bool, call_, int, maybeBool, maybeString, moduleName_, string, values_)

{-| 
@docs values_, call_, maybeString, maybeBool, string, bool, int, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Ui", "Input" ]


{-| {-| -}

int: String -> (Int -> msg) -> Int -> Element.Element msg
-}
int : String -> (Elm.Expression -> Elm.Expression) -> Int -> Elm.Expression
int intArg intArg0 intArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ui", "Input" ]
            , name = "int"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function [ Type.int ] (Type.var "msg")
                        , Type.int
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string intArg
        , Elm.functionReduced "intUnpack" intArg0
        , Elm.int intArg1
        ]


{-| {-| -}

bool: String -> (Bool -> msg) -> Bool -> Element.Element msg
-}
bool : String -> (Elm.Expression -> Elm.Expression) -> Bool -> Elm.Expression
bool boolArg boolArg0 boolArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ui", "Input" ]
            , name = "bool"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function [ Type.bool ] (Type.var "msg")
                        , Type.bool
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string boolArg
        , Elm.functionReduced "boolUnpack" boolArg0
        , Elm.bool boolArg1
        ]


{-| {-| -}

string: String -> (String -> msg) -> String -> Element.Element msg
-}
string :
    String -> (Elm.Expression -> Elm.Expression) -> String -> Elm.Expression
string stringArg stringArg0 stringArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ui", "Input" ]
            , name = "string"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function [ Type.string ] (Type.var "msg")
                        , Type.string
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string stringArg
        , Elm.functionReduced "stringUnpack" stringArg0
        , Elm.string stringArg1
        ]


{-| {-| -}

maybeBool: String -> (Maybe Bool -> msg) -> Maybe Bool -> Element.Element msg
-}
maybeBool :
    String
    -> (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
maybeBool maybeBoolArg maybeBoolArg0 maybeBoolArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ui", "Input" ]
            , name = "maybeBool"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function
                            [ Type.namedWith [] "Maybe" [ Type.bool ] ]
                            (Type.var "msg")
                        , Type.namedWith [] "Maybe" [ Type.bool ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string maybeBoolArg
        , Elm.functionReduced "maybeBoolUnpack" maybeBoolArg0
        , maybeBoolArg1
        ]


{-| {-| -}

maybeString: String -> (Maybe String -> msg) -> Maybe String -> Element.Element msg
-}
maybeString :
    String
    -> (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
maybeString maybeStringArg maybeStringArg0 maybeStringArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ui", "Input" ]
            , name = "maybeString"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function
                            [ Type.namedWith [] "Maybe" [ Type.string ] ]
                            (Type.var "msg")
                        , Type.namedWith [] "Maybe" [ Type.string ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string maybeStringArg
        , Elm.functionReduced "maybeStringUnpack" maybeStringArg0
        , maybeStringArg1
        ]


call_ :
    { int : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , bool :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , string :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , maybeBool :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , maybeString :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { int =
        \intArg intArg0 intArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ui", "Input" ]
                    , name = "int"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.function [ Type.int ] (Type.var "msg")
                                , Type.int
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ intArg, intArg0, intArg1 ]
    , bool =
        \boolArg boolArg0 boolArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ui", "Input" ]
                    , name = "bool"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.function [ Type.bool ] (Type.var "msg")
                                , Type.bool
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ boolArg, boolArg0, boolArg1 ]
    , string =
        \stringArg stringArg0 stringArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ui", "Input" ]
                    , name = "string"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.function [ Type.string ] (Type.var "msg")
                                , Type.string
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ stringArg, stringArg0, stringArg1 ]
    , maybeBool =
        \maybeBoolArg maybeBoolArg0 maybeBoolArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ui", "Input" ]
                    , name = "maybeBool"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.function
                                    [ Type.namedWith [] "Maybe" [ Type.bool ] ]
                                    (Type.var "msg")
                                , Type.namedWith [] "Maybe" [ Type.bool ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ maybeBoolArg, maybeBoolArg0, maybeBoolArg1 ]
    , maybeString =
        \maybeStringArg maybeStringArg0 maybeStringArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Ui", "Input" ]
                    , name = "maybeString"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.function
                                    [ Type.namedWith [] "Maybe" [ Type.string ]
                                    ]
                                    (Type.var "msg")
                                , Type.namedWith [] "Maybe" [ Type.string ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Element"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ maybeStringArg, maybeStringArg0, maybeStringArg1 ]
    }


values_ :
    { int : Elm.Expression
    , bool : Elm.Expression
    , string : Elm.Expression
    , maybeBool : Elm.Expression
    , maybeString : Elm.Expression
    }
values_ =
    { int =
        Elm.value
            { importFrom = [ "Ui", "Input" ]
            , name = "int"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function [ Type.int ] (Type.var "msg")
                        , Type.int
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , bool =
        Elm.value
            { importFrom = [ "Ui", "Input" ]
            , name = "bool"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function [ Type.bool ] (Type.var "msg")
                        , Type.bool
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , string =
        Elm.value
            { importFrom = [ "Ui", "Input" ]
            , name = "string"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function [ Type.string ] (Type.var "msg")
                        , Type.string
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , maybeBool =
        Elm.value
            { importFrom = [ "Ui", "Input" ]
            , name = "maybeBool"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function
                            [ Type.namedWith [] "Maybe" [ Type.bool ] ]
                            (Type.var "msg")
                        , Type.namedWith [] "Maybe" [ Type.bool ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , maybeString =
        Elm.value
            { importFrom = [ "Ui", "Input" ]
            , name = "maybeString"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.function
                            [ Type.namedWith [] "Maybe" [ Type.string ] ]
                            (Type.var "msg")
                        , Type.namedWith [] "Maybe" [ Type.string ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Element"
                            [ Type.var "msg" ]
                        )
                    )
            }
    }


