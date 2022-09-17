module Gen.Ui.Input exposing (bool, call_, int, moduleName_, string, values_)

{-| 
@docs values_, call_, string, bool, int, moduleName_
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


call_ :
    { int : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , bool :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , string :
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
    }


values_ :
    { int : Elm.Expression, bool : Elm.Expression, string : Elm.Expression }
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
    }


