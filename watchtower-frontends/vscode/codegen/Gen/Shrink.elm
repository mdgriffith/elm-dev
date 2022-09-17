module Gen.Shrink exposing (andMap, annotation_, array, atLeastChar, atLeastFloat, atLeastInt, bool, call_, char, character, convert, dropIf, float, int, keepIf, lazylist, list, map, maybe, merge, moduleName_, noShrink, order, result, shrink, string, tuple, tuple3, unit, values_)

{-| 
@docs values_, call_, annotation_, andMap, map, merge, dropIf, keepIf, convert, tuple3, tuple, array, list, lazylist, result, maybe, string, character, atLeastChar, char, atLeastFloat, float, atLeastInt, int, order, bool, unit, noShrink, shrink, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Shrink" ]


{-| Perform shrinking. Takes a predicate that returns `True` if you want
shrinking to continue (most likely the failing test for which we are attempting
to shrink the value). Also takes the shrinker and the value to shrink.

It returns the shrunken value, or the input value if no shrunken values that
satisfy the predicate are found.

shrink: (a -> Bool) -> Shrink.Shrinker a -> a -> a
-}
shrink :
    (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
shrink shrinkArg shrinkArg0 shrinkArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "shrink"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.bool
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.var "a"
                        ]
                        (Type.var "a")
                    )
            }
        )
        [ Elm.functionReduced "shrinkUnpack" shrinkArg, shrinkArg0, shrinkArg1 ]


{-| Perform no shrinking. Equivalent to the empty lazy list.

noShrink: a -> Lazy.List.LazyList a
-}
noShrink : Elm.Expression -> Elm.Expression
noShrink noShrinkArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "noShrink"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        )
                    )
            }
        )
        [ noShrinkArg ]


{-| Shrink the empty tuple. Equivalent to `noShrink`.

unit: Shrink.Shrinker ()
-}
unit : Elm.Expression
unit =
    Elm.value
        { importFrom = [ "Shrink" ]
        , name = "unit"
        , annotation =
            Just (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.unit ])
        }


{-| Shrinker of bools.

bool: Bool -> Lazy.List.LazyList Bool
-}
bool : Bool -> Elm.Expression
bool boolArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "bool"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.bool ]
                        )
                    )
            }
        )
        [ Elm.bool boolArg ]


{-| Shrinker of `Order` values.

order: Basics.Order -> Lazy.List.LazyList Basics.Order
-}
order : Elm.Expression -> Elm.Expression
order orderArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "order"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Basics" ] "Order" [] ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.namedWith [ "Basics" ] "Order" [] ]
                        )
                    )
            }
        )
        [ orderArg ]


{-| Shrinker of integers.

int: Int -> Lazy.List.LazyList Int
-}
int : Int -> Elm.Expression
int intArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "int"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.int ]
                        )
                    )
            }
        )
        [ Elm.int intArg ]


{-| Construct a shrinker of ints which considers the given int to
be most minimal.

atLeastInt: Int -> Int -> Lazy.List.LazyList Int
-}
atLeastInt : Int -> Int -> Elm.Expression
atLeastInt atLeastIntArg atLeastIntArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "atLeastInt"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.int ]
                        )
                    )
            }
        )
        [ Elm.int atLeastIntArg, Elm.int atLeastIntArg0 ]


{-| Shrinker of floats.

float: Float -> Lazy.List.LazyList Float
-}
float : Float -> Elm.Expression
float floatArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "float"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.float ]
                        )
                    )
            }
        )
        [ Elm.float floatArg ]


{-| Construct a shrinker of floats which considers the given float to
be most minimal.

atLeastFloat: Float -> Float -> Lazy.List.LazyList Float
-}
atLeastFloat : Float -> Float -> Elm.Expression
atLeastFloat atLeastFloatArg atLeastFloatArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "atLeastFloat"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.float ]
                        )
                    )
            }
        )
        [ Elm.float atLeastFloatArg, Elm.float atLeastFloatArg0 ]


{-| Shrinker of chars.

char: Shrink.Shrinker Char.Char
-}
char : Elm.Expression
char =
    Elm.value
        { importFrom = [ "Shrink" ]
        , name = "char"
        , annotation =
            Just (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.char ])
        }


{-| Construct a shrinker of chars which considers the given char to
be most minimal.

atLeastChar: Char.Char -> Shrink.Shrinker Char.Char
-}
atLeastChar : Char.Char -> Elm.Expression
atLeastChar atLeastCharArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "atLeastChar"
            , annotation =
                Just
                    (Type.function
                        [ Type.char ]
                        (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.char ])
                    )
            }
        )
        [ Elm.char atLeastCharArg ]


{-| Shrinker of chars which considers the empty space as the most
minimal char and omits the control key codes.

Equivalent to:

    atLeastChar (Char.fromCode 32)

character: Shrink.Shrinker Char.Char
-}
character : Elm.Expression
character =
    Elm.value
        { importFrom = [ "Shrink" ]
        , name = "character"
        , annotation =
            Just (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.char ])
        }


{-| Shrinker of strings. Considers the empty string to be the most
minimal string and the space to be the most minimal char.

Equivalent to:

    convert String.fromList String.toList (list character)

string: Shrink.Shrinker String
-}
string : Elm.Expression
string =
    Elm.value
        { importFrom = [ "Shrink" ]
        , name = "string"
        , annotation =
            Just (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.string ])
        }


{-| Maybe shrinker constructor.
Takes a shrinker of values and returns a shrinker of Maybes.

maybe: Shrink.Shrinker a -> Maybe a -> Lazy.List.LazyList (Maybe a)
-}
maybe : Elm.Expression -> Elm.Expression -> Elm.Expression
maybe maybeArg maybeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "maybe"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.maybe (Type.var "a")
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.maybe (Type.var "a") ]
                        )
                    )
            }
        )
        [ maybeArg, maybeArg0 ]


{-| Result shrinker constructor. Takes a shrinker of errors and a shrinker of
values and returns a shrinker of Results.

result: 
    Shrink.Shrinker error
    -> Shrink.Shrinker value
    -> Result.Result error value
    -> Lazy.List.LazyList (Result.Result error value)
-}
result : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
result resultArg resultArg0 resultArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "result"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "error" ]
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "value" ]
                        , Type.namedWith
                            [ "Result" ]
                            "Result"
                            [ Type.var "error", Type.var "value" ]
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.namedWith
                                [ "Result" ]
                                "Result"
                                [ Type.var "error", Type.var "value" ]
                            ]
                        )
                    )
            }
        )
        [ resultArg, resultArg0, resultArg1 ]


{-| Lazy List shrinker constructor. Takes a shrinker of values and returns a
shrinker of Lazy Lists. The lazy list being shrunk must be finite. (I mean
really, how do you shrink infinity?)

lazylist: 
    Shrink.Shrinker a
    -> Lazy.List.LazyList a
    -> Lazy.List.LazyList (Lazy.List.LazyList a)
-}
lazylist : Elm.Expression -> Elm.Expression -> Elm.Expression
lazylist lazylistArg lazylistArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "lazylist"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.namedWith
                                [ "Lazy", "List" ]
                                "LazyList"
                                [ Type.var "a" ]
                            ]
                        )
                    )
            }
        )
        [ lazylistArg, lazylistArg0 ]


{-| List shrinker constructor.
Takes a shrinker of values and returns a shrinker of Lists.

list: Shrink.Shrinker a -> Shrink.Shrinker (List a)
-}
list : Elm.Expression -> Elm.Expression
list listArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "list"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
        )
        [ listArg ]


{-| Array shrinker constructor.
Takes a shrinker of values and returns a shrinker of Arrays.

array: Shrink.Shrinker a -> Shrink.Shrinker (Array.Array a)
-}
array : Elm.Expression -> Elm.Expression
array arrayArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "array"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.namedWith
                                [ "Array" ]
                                "Array"
                                [ Type.var "a" ]
                            ]
                        )
                    )
            }
        )
        [ arrayArg ]


{-| 2-Tuple shrinker constructor.
Takes a tuple of shrinkers and returns a shrinker of tuples.

tuple: 
    ( Shrink.Shrinker a, Shrink.Shrinker b )
    -> ( a, b )
    -> Lazy.List.LazyList ( a, b )
-}
tuple : Elm.Expression -> Elm.Expression -> Elm.Expression
tuple tupleArg tupleArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "tuple"
            , annotation =
                Just
                    (Type.function
                        [ Type.tuple
                            (Type.namedWith
                                [ "Shrink" ]
                                "Shrinker"
                                [ Type.var "a" ]
                            )
                            (Type.namedWith
                                [ "Shrink" ]
                                "Shrinker"
                                [ Type.var "b" ]
                            )
                        , Type.tuple (Type.var "a") (Type.var "b")
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.tuple (Type.var "a") (Type.var "b") ]
                        )
                    )
            }
        )
        [ tupleArg, tupleArg0 ]


{-| 3-Tuple shrinker constructor.
Takes a tuple of shrinkers and returns a shrinker of tuples.

tuple3: 
    ( Shrink.Shrinker a, Shrink.Shrinker b, Shrink.Shrinker c )
    -> ( a, b, c )
    -> Lazy.List.LazyList ( a, b, c )
-}
tuple3 : Elm.Expression -> Elm.Expression -> Elm.Expression
tuple3 tuple3Arg tuple3Arg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "tuple3"
            , annotation =
                Just
                    (Type.function
                        [ Type.triple
                            (Type.namedWith
                                [ "Shrink" ]
                                "Shrinker"
                                [ Type.var "a" ]
                            )
                            (Type.namedWith
                                [ "Shrink" ]
                                "Shrinker"
                                [ Type.var "b" ]
                            )
                            (Type.namedWith
                                [ "Shrink" ]
                                "Shrinker"
                                [ Type.var "c" ]
                            )
                        , Type.triple
                            (Type.var "a")
                            (Type.var "b")
                            (Type.var "c")
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.triple
                                (Type.var "a")
                                (Type.var "b")
                                (Type.var "c")
                            ]
                        )
                    )
            }
        )
        [ tuple3Arg, tuple3Arg0 ]


{-| Convert a Shrinker of a's into a Shrinker of b's using two inverse functions.
)
If you use this function as follows:

    shrinkerB =
        convert f g shrinkerA

Make sure that:

    `f(g(x)) == x` for all x
    -- (putting something into g then feeding the output into f must give back
    -- just that original something, whatever it is)

Or else this process will generate garbage.

convert: (a -> b) -> (b -> a) -> Shrink.Shrinker a -> b -> Lazy.List.LazyList b
-}
convert :
    (Elm.Expression -> Elm.Expression)
    -> (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
convert convertArg convertArg0 convertArg1 convertArg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "convert"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.function [ Type.var "b" ] (Type.var "a")
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.var "b"
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "b" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "convertUnpack" convertArg
        , Elm.functionReduced "convertUnpack" convertArg0
        , convertArg1
        , convertArg2
        ]


{-| Filter out the results of a shrinker. The resulting shrinker
will only produce shrinks which satisfy the given predicate.

keepIf: (a -> Bool) -> Shrink.Shrinker a -> a -> Lazy.List.LazyList a
-}
keepIf :
    (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
keepIf keepIfArg keepIfArg0 keepIfArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "keepIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.bool
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.var "a"
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "keepIfUnpack" keepIfArg, keepIfArg0, keepIfArg1 ]


{-| Filter out the results of a shrinker. The resulting shrinker
will only throw away shrinks which satisfy the given predicate.

dropIf: (a -> Bool) -> Shrink.Shrinker a -> Shrink.Shrinker a
-}
dropIf : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
dropIf dropIfArg dropIfArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "dropIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.bool
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.var "a" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "dropIfUnpack" dropIfArg, dropIfArg0 ]


{-| Merge two shrinkers. Generates all the values in the first
shrinker, and then all the non-duplicated values in the second
shrinker.

merge: Shrink.Shrinker a -> Shrink.Shrinker a -> a -> Lazy.List.LazyList a
-}
merge : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
merge mergeArg mergeArg0 mergeArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "merge"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.var "a"
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        )
                    )
            }
        )
        [ mergeArg, mergeArg0, mergeArg1 ]


{-| Re-export of `Lazy.List.map`
This is useful in order to compose shrinkers, especially when used in
conjunction with `andMap`. For example:

    type alias Vector =
        { x : Float
        , y : Float
        , z : Float
        }

    vector : Shrinker Vector
    vector { x, y, z } =
        Vector
            |> map (float x)
            |> andMap (float y)
            |> andMap (float z)

map: (a -> b) -> Lazy.List.LazyList a -> Lazy.List.LazyList b
-}
map : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
map mapArg mapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "b" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced "mapUnpack" mapArg, mapArg0 ]


{-| Apply a lazy list of functions on a lazy list of values.

The argument order is so that it is easy to use in `|>` chains.

andMap: Lazy.List.LazyList a -> Lazy.List.LazyList (a -> b) -> Lazy.List.LazyList b
-}
andMap : Elm.Expression -> Elm.Expression -> Elm.Expression
andMap andMapArg andMapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Shrink" ]
            , name = "andMap"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        , Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.function [ Type.var "a" ] (Type.var "b") ]
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "b" ]
                        )
                    )
            }
        )
        [ andMapArg, andMapArg0 ]


annotation_ : { shrinker : Type.Annotation -> Type.Annotation }
annotation_ =
    { shrinker =
        \shrinkerArg0 ->
            Type.alias
                moduleName_
                "Shrinker"
                [ shrinkerArg0 ]
                (Type.function
                    [ Type.var "a" ]
                    (Type.namedWith
                        [ "Lazy", "List" ]
                        "LazyList"
                        [ Type.var "a" ]
                    )
                )
    }


call_ :
    { shrink :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , noShrink : Elm.Expression -> Elm.Expression
    , bool : Elm.Expression -> Elm.Expression
    , order : Elm.Expression -> Elm.Expression
    , int : Elm.Expression -> Elm.Expression
    , atLeastInt : Elm.Expression -> Elm.Expression -> Elm.Expression
    , float : Elm.Expression -> Elm.Expression
    , atLeastFloat : Elm.Expression -> Elm.Expression -> Elm.Expression
    , atLeastChar : Elm.Expression -> Elm.Expression
    , maybe : Elm.Expression -> Elm.Expression -> Elm.Expression
    , result :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , lazylist : Elm.Expression -> Elm.Expression -> Elm.Expression
    , list : Elm.Expression -> Elm.Expression
    , array : Elm.Expression -> Elm.Expression
    , tuple : Elm.Expression -> Elm.Expression -> Elm.Expression
    , tuple3 : Elm.Expression -> Elm.Expression -> Elm.Expression
    , convert :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , keepIf :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , dropIf : Elm.Expression -> Elm.Expression -> Elm.Expression
    , merge :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , map : Elm.Expression -> Elm.Expression -> Elm.Expression
    , andMap : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { shrink =
        \shrinkArg shrinkArg0 shrinkArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "shrink"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] Type.bool
                                , Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                , Type.var "a"
                                ]
                                (Type.var "a")
                            )
                    }
                )
                [ shrinkArg, shrinkArg0, shrinkArg1 ]
    , noShrink =
        \noShrinkArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "noShrink"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "a" ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ noShrinkArg ]
    , bool =
        \boolArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "bool"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.bool ]
                                )
                            )
                    }
                )
                [ boolArg ]
    , order =
        \orderArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "order"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Basics" ] "Order" [] ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.namedWith [ "Basics" ] "Order" [] ]
                                )
                            )
                    }
                )
                [ orderArg ]
    , int =
        \intArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "int"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.int ]
                                )
                            )
                    }
                )
                [ intArg ]
    , atLeastInt =
        \atLeastIntArg atLeastIntArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "atLeastInt"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.int ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.int ]
                                )
                            )
                    }
                )
                [ atLeastIntArg, atLeastIntArg0 ]
    , float =
        \floatArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "float"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.float ]
                                )
                            )
                    }
                )
                [ floatArg ]
    , atLeastFloat =
        \atLeastFloatArg atLeastFloatArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "atLeastFloat"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.float ]
                                )
                            )
                    }
                )
                [ atLeastFloatArg, atLeastFloatArg0 ]
    , atLeastChar =
        \atLeastCharArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "atLeastChar"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.char ]
                                (Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.char ]
                                )
                            )
                    }
                )
                [ atLeastCharArg ]
    , maybe =
        \maybeArg maybeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "maybe"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                , Type.maybe (Type.var "a")
                                ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.maybe (Type.var "a") ]
                                )
                            )
                    }
                )
                [ maybeArg, maybeArg0 ]
    , result =
        \resultArg resultArg0 resultArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "result"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "error" ]
                                , Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "value" ]
                                , Type.namedWith
                                    [ "Result" ]
                                    "Result"
                                    [ Type.var "error", Type.var "value" ]
                                ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.namedWith
                                        [ "Result" ]
                                        "Result"
                                        [ Type.var "error", Type.var "value" ]
                                    ]
                                )
                            )
                    }
                )
                [ resultArg, resultArg0, resultArg1 ]
    , lazylist =
        \lazylistArg lazylistArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "lazylist"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.namedWith
                                        [ "Lazy", "List" ]
                                        "LazyList"
                                        [ Type.var "a" ]
                                    ]
                                )
                            )
                    }
                )
                [ lazylistArg, lazylistArg0 ]
    , list =
        \listArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "list"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.list (Type.var "a") ]
                                )
                            )
                    }
                )
                [ listArg ]
    , array =
        \arrayArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "array"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.namedWith
                                        [ "Array" ]
                                        "Array"
                                        [ Type.var "a" ]
                                    ]
                                )
                            )
                    }
                )
                [ arrayArg ]
    , tuple =
        \tupleArg tupleArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "tuple"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.tuple
                                    (Type.namedWith
                                        [ "Shrink" ]
                                        "Shrinker"
                                        [ Type.var "a" ]
                                    )
                                    (Type.namedWith
                                        [ "Shrink" ]
                                        "Shrinker"
                                        [ Type.var "b" ]
                                    )
                                , Type.tuple (Type.var "a") (Type.var "b")
                                ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.tuple (Type.var "a") (Type.var "b") ]
                                )
                            )
                    }
                )
                [ tupleArg, tupleArg0 ]
    , tuple3 =
        \tuple3Arg tuple3Arg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "tuple3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.triple
                                    (Type.namedWith
                                        [ "Shrink" ]
                                        "Shrinker"
                                        [ Type.var "a" ]
                                    )
                                    (Type.namedWith
                                        [ "Shrink" ]
                                        "Shrinker"
                                        [ Type.var "b" ]
                                    )
                                    (Type.namedWith
                                        [ "Shrink" ]
                                        "Shrinker"
                                        [ Type.var "c" ]
                                    )
                                , Type.triple
                                    (Type.var "a")
                                    (Type.var "b")
                                    (Type.var "c")
                                ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.triple
                                        (Type.var "a")
                                        (Type.var "b")
                                        (Type.var "c")
                                    ]
                                )
                            )
                    }
                )
                [ tuple3Arg, tuple3Arg0 ]
    , convert =
        \convertArg convertArg0 convertArg1 convertArg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "convert"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] (Type.var "b")
                                , Type.function [ Type.var "b" ] (Type.var "a")
                                , Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                , Type.var "b"
                                ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.var "b" ]
                                )
                            )
                    }
                )
                [ convertArg, convertArg0, convertArg1, convertArg2 ]
    , keepIf =
        \keepIfArg keepIfArg0 keepIfArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "keepIf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] Type.bool
                                , Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                , Type.var "a"
                                ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ keepIfArg, keepIfArg0, keepIfArg1 ]
    , dropIf =
        \dropIfArg dropIfArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "dropIf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] Type.bool
                                , Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ dropIfArg, dropIfArg0 ]
    , merge =
        \mergeArg mergeArg0 mergeArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "merge"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                , Type.var "a"
                                ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ mergeArg, mergeArg0, mergeArg1 ]
    , map =
        \mapArg mapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "map"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] (Type.var "b")
                                , Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.var "b" ]
                                )
                            )
                    }
                )
                [ mapArg, mapArg0 ]
    , andMap =
        \andMapArg andMapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Shrink" ]
                    , name = "andMap"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.function
                                        [ Type.var "a" ]
                                        (Type.var "b")
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Lazy", "List" ]
                                    "LazyList"
                                    [ Type.var "b" ]
                                )
                            )
                    }
                )
                [ andMapArg, andMapArg0 ]
    }


values_ :
    { shrink : Elm.Expression
    , noShrink : Elm.Expression
    , unit : Elm.Expression
    , bool : Elm.Expression
    , order : Elm.Expression
    , int : Elm.Expression
    , atLeastInt : Elm.Expression
    , float : Elm.Expression
    , atLeastFloat : Elm.Expression
    , char : Elm.Expression
    , atLeastChar : Elm.Expression
    , character : Elm.Expression
    , string : Elm.Expression
    , maybe : Elm.Expression
    , result : Elm.Expression
    , lazylist : Elm.Expression
    , list : Elm.Expression
    , array : Elm.Expression
    , tuple : Elm.Expression
    , tuple3 : Elm.Expression
    , convert : Elm.Expression
    , keepIf : Elm.Expression
    , dropIf : Elm.Expression
    , merge : Elm.Expression
    , map : Elm.Expression
    , andMap : Elm.Expression
    }
values_ =
    { shrink =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "shrink"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.bool
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.var "a"
                        ]
                        (Type.var "a")
                    )
            }
    , noShrink =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "noShrink"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        )
                    )
            }
    , unit =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "unit"
            , annotation =
                Just (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.unit ])
            }
    , bool =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "bool"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.bool ]
                        )
                    )
            }
    , order =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "order"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Basics" ] "Order" [] ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.namedWith [ "Basics" ] "Order" [] ]
                        )
                    )
            }
    , int =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "int"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.int ]
                        )
                    )
            }
    , atLeastInt =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "atLeastInt"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.int ]
                        )
                    )
            }
    , float =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "float"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.float ]
                        )
                    )
            }
    , atLeastFloat =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "atLeastFloat"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.float ]
                        )
                    )
            }
    , char =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "char"
            , annotation =
                Just (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.char ])
            }
    , atLeastChar =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "atLeastChar"
            , annotation =
                Just
                    (Type.function
                        [ Type.char ]
                        (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.char ])
                    )
            }
    , character =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "character"
            , annotation =
                Just (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.char ])
            }
    , string =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "string"
            , annotation =
                Just (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.string ])
            }
    , maybe =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "maybe"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.maybe (Type.var "a")
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.maybe (Type.var "a") ]
                        )
                    )
            }
    , result =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "result"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "error" ]
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "value" ]
                        , Type.namedWith
                            [ "Result" ]
                            "Result"
                            [ Type.var "error", Type.var "value" ]
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.namedWith
                                [ "Result" ]
                                "Result"
                                [ Type.var "error", Type.var "value" ]
                            ]
                        )
                    )
            }
    , lazylist =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "lazylist"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.namedWith
                                [ "Lazy", "List" ]
                                "LazyList"
                                [ Type.var "a" ]
                            ]
                        )
                    )
            }
    , list =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "list"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
    , array =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "array"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.namedWith
                                [ "Array" ]
                                "Array"
                                [ Type.var "a" ]
                            ]
                        )
                    )
            }
    , tuple =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "tuple"
            , annotation =
                Just
                    (Type.function
                        [ Type.tuple
                            (Type.namedWith
                                [ "Shrink" ]
                                "Shrinker"
                                [ Type.var "a" ]
                            )
                            (Type.namedWith
                                [ "Shrink" ]
                                "Shrinker"
                                [ Type.var "b" ]
                            )
                        , Type.tuple (Type.var "a") (Type.var "b")
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.tuple (Type.var "a") (Type.var "b") ]
                        )
                    )
            }
    , tuple3 =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "tuple3"
            , annotation =
                Just
                    (Type.function
                        [ Type.triple
                            (Type.namedWith
                                [ "Shrink" ]
                                "Shrinker"
                                [ Type.var "a" ]
                            )
                            (Type.namedWith
                                [ "Shrink" ]
                                "Shrinker"
                                [ Type.var "b" ]
                            )
                            (Type.namedWith
                                [ "Shrink" ]
                                "Shrinker"
                                [ Type.var "c" ]
                            )
                        , Type.triple
                            (Type.var "a")
                            (Type.var "b")
                            (Type.var "c")
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.triple
                                (Type.var "a")
                                (Type.var "b")
                                (Type.var "c")
                            ]
                        )
                    )
            }
    , convert =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "convert"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.function [ Type.var "b" ] (Type.var "a")
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.var "b"
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "b" ]
                        )
                    )
            }
    , keepIf =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "keepIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.bool
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.var "a"
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        )
                    )
            }
    , dropIf =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "dropIf"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] Type.bool
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Shrink" ] "Shrinker" [ Type.var "a" ]
                        )
                    )
            }
    , merge =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "merge"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        , Type.var "a"
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        )
                    )
            }
    , map =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "b" ]
                        )
                    )
            }
    , andMap =
        Elm.value
            { importFrom = [ "Shrink" ]
            , name = "andMap"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "a" ]
                        , Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.function [ Type.var "a" ] (Type.var "b") ]
                        ]
                        (Type.namedWith
                            [ "Lazy", "List" ]
                            "LazyList"
                            [ Type.var "b" ]
                        )
                    )
            }
    }


