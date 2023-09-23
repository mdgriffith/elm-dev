module Gen.Fuzz exposing (andMap, annotation_, array, bool, call_, char, constant, custom, float, floatRange, frequency, int, intRange, invalid, list, map, map2, map3, map4, map5, maybe, moduleName_, oneOf, order, percentage, result, string, tuple, tuple3, unit, values_)

{-| 
@docs values_, call_, annotation_, invalid, order, unit, char, custom, tuple3, tuple, frequency, andMap, map5, map4, map3, map2, map, constant, oneOf, array, list, result, maybe, bool, string, percentage, floatRange, float, intRange, int, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Fuzz" ]


{-| A fuzzer for int values. It will never produce `NaN`, `Infinity`, or `-Infinity`.

It's possible for this fuzzer to generate any 32-bit integer, signed or unsigned, but it favors
numbers between -50 and 50 and especially zero.

int: Fuzz.Fuzzer Int
-}
int : Elm.Expression
int =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "int"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
        }


{-| A fuzzer for int values between a given minimum and maximum value,
inclusive. Shrunken values will also be within the range.

Remember that [Random.maxInt](http://package.elm-lang.org/packages/elm-lang/core/latest/Random#maxInt)
is the maximum possible int value, so you can do `intRange x Random.maxInt` to get all
the ints x or bigger.

intRange: Int -> Int -> Fuzz.Fuzzer Int
-}
intRange : Int -> Int -> Elm.Expression
intRange intRangeArg intRangeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "intRange"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
                    )
            }
        )
        [ Elm.int intRangeArg, Elm.int intRangeArg0 ]


{-| A fuzzer for float values. It will never produce `NaN`, `Infinity`, or `-Infinity`.

It's possible for this fuzzer to generate any other floating-point value, but it
favors numbers between -50 and 50, numbers between -1 and 1, and especially zero.

float: Fuzz.Fuzzer Float
-}
float : Elm.Expression
float =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "float"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
        }


{-| A fuzzer for float values within between a given minimum and maximum
value, inclusive. Shrunken values will also be within the range.

floatRange: Float -> Float -> Fuzz.Fuzzer Float
-}
floatRange : Float -> Float -> Elm.Expression
floatRange floatRangeArg floatRangeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "floatRange"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
                    )
            }
        )
        [ Elm.float floatRangeArg, Elm.float floatRangeArg0 ]


{-| A fuzzer for percentage values. Generates random floats between `0.0` and
`1.0`. It will test zero and one about 10% of the time each.

percentage: Fuzz.Fuzzer Float
-}
percentage : Elm.Expression
percentage =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "percentage"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
        }


{-| Generates random printable ASCII strings of up to 1000 characters.

Shorter strings are more common, especially the empty string.

string: Fuzz.Fuzzer String
-}
string : Elm.Expression
string =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "string"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
        }


{-| A fuzzer for boolean values. It's useful when building up fuzzers of complex
types that contain a boolean somewhere.

We recommend against writing tests fuzzing over booleans. Write a unit test for
the true and false cases explicitly.

bool: Fuzz.Fuzzer Bool
-}
bool : Elm.Expression
bool =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "bool"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.bool ])
        }


{-| Given a fuzzer of a type, create a fuzzer of a maybe for that type.

maybe: Fuzz.Fuzzer a -> Fuzz.Fuzzer (Maybe a)
-}
maybe : Elm.Expression -> Elm.Expression
maybe maybeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "maybe"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.maybe (Type.var "a") ]
                        )
                    )
            }
        )
        [ maybeArg ]


{-| Given fuzzers for an error type and a success type, create a fuzzer for
a result.

result: 
    Fuzz.Fuzzer error
    -> Fuzz.Fuzzer value
    -> Fuzz.Fuzzer (Result.Result error value)
-}
result : Elm.Expression -> Elm.Expression -> Elm.Expression
result resultArg resultArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "result"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.var "error" ]
                        , Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.var "value" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.namedWith
                                [ "Result" ]
                                "Result"
                                [ Type.var "error", Type.var "value" ]
                            ]
                        )
                    )
            }
        )
        [ resultArg, resultArg0 ]


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of varying length, favoring shorter lists.

list: Fuzz.Fuzzer a -> Fuzz.Fuzzer (List a)
-}
list : Elm.Expression -> Elm.Expression
list listArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "list"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
        )
        [ listArg ]


{-| Given a fuzzer of a type, create a fuzzer of an array of that type.
Generates random arrays of varying length, favoring shorter arrays.

array: Fuzz.Fuzzer a -> Fuzz.Fuzzer (Array.Array a)
-}
array : Elm.Expression -> Elm.Expression
array arrayArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "array"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
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


{-| Choose one of the given fuzzers at random. Each fuzzer has an equal chance
of being chosen; to customize the probabilities, use [`frequency`](#frequency).

    Fuzz.oneOf
        [ Fuzz.intRange 0 3
        , Fuzz.intRange 7 9
        ]

oneOf: List (Fuzz.Fuzzer a) -> Fuzz.Fuzzer a
-}
oneOf : List Elm.Expression -> Elm.Expression
oneOf oneOfArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "oneOf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.list oneOfArg ]


{-| Create a fuzzer that only and always returns the value provided, and performs no shrinking. This is hardly random,
and so this function is best used as a helper when creating more complicated fuzzers.

constant: a -> Fuzz.Fuzzer a
-}
constant : Elm.Expression -> Elm.Expression
constant constantArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "constant"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ constantArg ]


{-| Map a function over a fuzzer. This applies to both the generated and the shrunken values.

map: (a -> b) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b
-}
map : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
map mapArg mapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ])
                    )
            }
        )
        [ Elm.functionReduced "mapUnpack" mapArg, mapArg0 ]


{-| Map over two fuzzers.

map2: (a -> b -> c) -> Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c
-}
map2 :
    (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
map2 map2Arg map2Arg0 map2Arg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map2"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b" ]
                            (Type.var "c")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ])
                    )
            }
        )
        [ Elm.functionReduced
            "map2Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced "unpack" (map2Arg functionReducedUnpack)
            )
        , map2Arg0
        , map2Arg1
        ]


{-| Map over three fuzzers.

map3: 
    (a -> b -> c -> d)
    -> Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> Fuzz.Fuzzer c
    -> Fuzz.Fuzzer d
-}
map3 :
    (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
map3 map3Arg map3Arg0 map3Arg1 map3Arg2 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map3"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b", Type.var "c" ]
                            (Type.var "d")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ])
                    )
            }
        )
        [ Elm.functionReduced
            "map3Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (map3Arg functionReducedUnpack
                                functionReducedUnpack0
                            )
                    )
            )
        , map3Arg0
        , map3Arg1
        , map3Arg2
        ]


{-| Map over four fuzzers.

map4: 
    (a -> b -> c -> d -> e)
    -> Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> Fuzz.Fuzzer c
    -> Fuzz.Fuzzer d
    -> Fuzz.Fuzzer e
-}
map4 :
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
map4 map4Arg map4Arg0 map4Arg1 map4Arg2 map4Arg3 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map4"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            ]
                            (Type.var "e")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ])
                    )
            }
        )
        [ Elm.functionReduced
            "map4Unpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_0_2_0_0 ->
                                Elm.functionReduced
                                    "unpack"
                                    (map4Arg functionReducedUnpack
                                         functionReducedUnpack0
                                        functionReducedUnpack_2_1_2_0_2_0_2_0_0
                                    )
                            )
                    )
            )
        , map4Arg0
        , map4Arg1
        , map4Arg2
        , map4Arg3
        ]


{-| Map over five fuzzers.

map5: 
    (a -> b -> c -> d -> e -> f)
    -> Fuzz.Fuzzer a
    -> Fuzz.Fuzzer b
    -> Fuzz.Fuzzer c
    -> Fuzz.Fuzzer d
    -> Fuzz.Fuzzer e
    -> Fuzz.Fuzzer f
-}
map5 :
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
map5 map5Arg map5Arg0 map5Arg1 map5Arg2 map5Arg3 map5Arg4 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map5"
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
                            (Type.var "f")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "f" ])
                    )
            }
        )
        [ Elm.functionReduced
            "map5Unpack"
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
                                            (map5Arg functionReducedUnpack
                                                 functionReducedUnpack0
                                                 functionReducedUnpack_2_1_2_0_2_0_2_0_0
                                                functionReducedUnpack_2_1_2_1_2_0_2_0_2_0_0
                                            )
                                    )
                            )
                    )
            )
        , map5Arg0
        , map5Arg1
        , map5Arg2
        , map5Arg3
        , map5Arg4
        ]


{-| Map over many fuzzers. This can act as `mapN` for `N > 5`.
The argument order is meant to accommodate chaining:

    map f aFuzzer
        |> andMap anotherFuzzer
        |> andMap aThirdFuzzer

Note that shrinking may be better using `mapN`.

andMap: Fuzz.Fuzzer a -> Fuzz.Fuzzer (a -> b) -> Fuzz.Fuzzer b
-}
andMap : Elm.Expression -> Elm.Expression -> Elm.Expression
andMap andMapArg andMapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "andMap"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.function [ Type.var "a" ] (Type.var "b") ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ])
                    )
            }
        )
        [ andMapArg, andMapArg0 ]


{-| Create a new `Fuzzer` by providing a list of probabilistic weights to use
with other fuzzers.
For example, to create a `Fuzzer` that has a 1/4 chance of generating an int
between -1 and -100, and a 3/4 chance of generating one between 1 and 100,
you could do this:

    Fuzz.frequency
        [ ( 1, Fuzz.intRange -100 -1 )
        , ( 3, Fuzz.intRange 1 100 )
        ]

There are a few circumstances in which this function will return an invalid
fuzzer, which causes it to fail any test that uses it:

  - If you provide an empty list of frequencies
  - If any of the weights are less than 0
  - If the weights sum to 0

Be careful recursively using this fuzzer in its arguments. Often using `map`
is a better way to do what you want. If you are fuzzing a tree-like data
structure, you should include a depth limit so to avoid infinite recursion, like
so:

    type Tree
        = Leaf
        | Branch Tree Tree

    tree : Int -> Fuzzer Tree
    tree i =
        if i <= 0 then
            Fuzz.constant Leaf

        else
            Fuzz.frequency
                [ ( 1, Fuzz.constant Leaf )
                , ( 2, Fuzz.map2 Branch (tree (i - 1)) (tree (i - 1)) )
                ]

frequency: List ( Float, Fuzz.Fuzzer a ) -> Fuzz.Fuzzer a
-}
frequency : List Elm.Expression -> Elm.Expression
frequency frequencyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "frequency"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.float
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.list frequencyArg ]


{-| Turn a tuple of fuzzers into a fuzzer of tuples.

tuple: ( Fuzz.Fuzzer a, Fuzz.Fuzzer b ) -> Fuzz.Fuzzer ( a, b )
-}
tuple : Elm.Expression -> Elm.Expression
tuple tupleArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "tuple"
            , annotation =
                Just
                    (Type.function
                        [ Type.tuple
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.tuple (Type.var "a") (Type.var "b") ]
                        )
                    )
            }
        )
        [ tupleArg ]


{-| Turn a 3-tuple of fuzzers into a fuzzer of 3-tuples.

tuple3: ( Fuzz.Fuzzer a, Fuzz.Fuzzer b, Fuzz.Fuzzer c ) -> Fuzz.Fuzzer ( a, b, c )
-}
tuple3 : Elm.Expression -> Elm.Expression
tuple3 tuple3Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "tuple3"
            , annotation =
                Just
                    (Type.function
                        [ Type.triple
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                            )
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.triple
                                (Type.var "a")
                                (Type.var "b")
                                (Type.var "c")
                            ]
                        )
                    )
            }
        )
        [ tuple3Arg ]


{-| Build a custom `Fuzzer a` by providing a `Generator a` and a `Shrinker a`. Generators are defined in
[`elm/random`](http://package.elm-lang.org/packages/elm/random/latest). Shrinkers are defined in the [`Shrink`
module](https://package.elm-lang.org/packages/elm-explorations/test/latest/Shrink). It is not possible to extract the
generator and shrinker from an existing fuzzer.

This function should be considered for advanced uses. It's often easier to use `map` and other functions in this
module to create a fuzzer.

Here is an example for a record:

    import Random
    import Shrink

    type alias Position =
        { x : Int, y : Int }

    position : Fuzzer Position
    position =
        Fuzz.custom
            (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
            (\{ x, y } -> Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))

Here is an example for a custom union type, assuming there is already a `genName : Generator String` defined:

    type Question
        = Name String
        | Age Int

    question =
        let
            generator =
                Random.bool
                    |> Random.andThen
                        (\b ->
                            if b then
                                Random.map Name genName

                            else
                                Random.map Age (Random.int 0 120)
                        )

            shrinker question =
                case question of
                    Name n ->
                        Shrink.string n |> Shrink.map Name

                    Age i ->
                        Shrink.int i |> Shrink.map Age
        in
        Fuzz.custom generator shrinker

custom: Random.Generator a -> Shrink.Shrinker a -> Fuzz.Fuzzer a
-}
custom : Elm.Expression -> Elm.Expression -> Elm.Expression
custom customArg customArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Random" ]
                            "Generator"
                            [ Type.var "a" ]
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ customArg, customArg0 ]


{-| A fuzzer for char values. Generates random ascii chars disregarding the control
characters and the extended character set.

char: Fuzz.Fuzzer Char.Char
-}
char : Elm.Expression
char =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "char"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.char ])
        }


{-| A fuzzer for the unit value. Unit is a type with only one value, commonly
used as a placeholder.

unit: Fuzz.Fuzzer ()
-}
unit : Elm.Expression
unit =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "unit"
        , annotation = Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.unit ])
        }


{-| A fuzzer for order values.

order: Fuzz.Fuzzer Basics.Order
-}
order : Elm.Expression
order =
    Elm.value
        { importFrom = [ "Fuzz" ]
        , name = "order"
        , annotation =
            Just
                (Type.namedWith
                    [ "Fuzz" ]
                    "Fuzzer"
                    [ Type.namedWith [ "Basics" ] "Order" [] ]
                )
        }


{-| A fuzzer that is invalid for the provided reason. Any fuzzers built with it
are also invalid. Any tests using an invalid fuzzer fail.

invalid: String -> Fuzz.Fuzzer a
-}
invalid : String -> Elm.Expression
invalid invalidArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "invalid"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
        )
        [ Elm.string invalidArg ]


annotation_ : { fuzzer : Type.Annotation -> Type.Annotation }
annotation_ =
    { fuzzer =
        \fuzzerArg0 ->
            Type.alias
                moduleName_
                "Fuzzer"
                [ fuzzerArg0 ]
                (Type.namedWith [ "Fuzz", "Internal" ] "Fuzzer" [ Type.var "a" ]
                )
    }


call_ :
    { intRange : Elm.Expression -> Elm.Expression -> Elm.Expression
    , floatRange : Elm.Expression -> Elm.Expression -> Elm.Expression
    , maybe : Elm.Expression -> Elm.Expression
    , result : Elm.Expression -> Elm.Expression -> Elm.Expression
    , list : Elm.Expression -> Elm.Expression
    , array : Elm.Expression -> Elm.Expression
    , oneOf : Elm.Expression -> Elm.Expression
    , constant : Elm.Expression -> Elm.Expression
    , map : Elm.Expression -> Elm.Expression -> Elm.Expression
    , map2 :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , map3 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , map4 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , map5 :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , andMap : Elm.Expression -> Elm.Expression -> Elm.Expression
    , frequency : Elm.Expression -> Elm.Expression
    , tuple : Elm.Expression -> Elm.Expression
    , tuple3 : Elm.Expression -> Elm.Expression
    , custom : Elm.Expression -> Elm.Expression -> Elm.Expression
    , invalid : Elm.Expression -> Elm.Expression
    }
call_ =
    { intRange =
        \intRangeArg intRangeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "intRange"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.int ]
                                (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ]
                                )
                            )
                    }
                )
                [ intRangeArg, intRangeArg0 ]
    , floatRange =
        \floatRangeArg floatRangeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "floatRange"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float, Type.float ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.float ]
                                )
                            )
                    }
                )
                [ floatRangeArg, floatRangeArg0 ]
    , maybe =
        \maybeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "maybe"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.maybe (Type.var "a") ]
                                )
                            )
                    }
                )
                [ maybeArg ]
    , result =
        \resultArg resultArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "result"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "error" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "value" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.namedWith
                                        [ "Result" ]
                                        "Result"
                                        [ Type.var "error", Type.var "value" ]
                                    ]
                                )
                            )
                    }
                )
                [ resultArg, resultArg0 ]
    , list =
        \listArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "list"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
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
                    { importFrom = [ "Fuzz" ]
                    , name = "array"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
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
    , oneOf =
        \oneOfArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "oneOf"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Fuzz" ]
                                        "Fuzzer"
                                        [ Type.var "a" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ oneOfArg ]
    , constant =
        \constantArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "constant"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "a" ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ constantArg ]
    , map =
        \mapArg mapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function [ Type.var "a" ] (Type.var "b")
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                )
                            )
                    }
                )
                [ mapArg, mapArg0 ]
    , map2 =
        \map2Arg map2Arg0 map2Arg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map2"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a", Type.var "b" ]
                                    (Type.var "c")
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "c" ]
                                )
                            )
                    }
                )
                [ map2Arg, map2Arg0, map2Arg1 ]
    , map3 =
        \map3Arg map3Arg0 map3Arg1 map3Arg2 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a", Type.var "b", Type.var "c" ]
                                    (Type.var "d")
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "c" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "d" ]
                                )
                            )
                    }
                )
                [ map3Arg, map3Arg0, map3Arg1, map3Arg2 ]
    , map4 =
        \map4Arg map4Arg0 map4Arg1 map4Arg2 map4Arg3 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map4"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "a"
                                    , Type.var "b"
                                    , Type.var "c"
                                    , Type.var "d"
                                    ]
                                    (Type.var "e")
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "c" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "d" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "e" ]
                                )
                            )
                    }
                )
                [ map4Arg, map4Arg0, map4Arg1, map4Arg2, map4Arg3 ]
    , map5 =
        \map5Arg map5Arg0 map5Arg1 map5Arg2 map5Arg3 map5Arg4 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "map5"
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
                                    (Type.var "f")
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "c" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "d" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "e" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "f" ]
                                )
                            )
                    }
                )
                [ map5Arg, map5Arg0, map5Arg1, map5Arg2, map5Arg3, map5Arg4 ]
    , andMap =
        \andMapArg andMapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "andMap"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.function
                                        [ Type.var "a" ]
                                        (Type.var "b")
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "b" ]
                                )
                            )
                    }
                )
                [ andMapArg, andMapArg0 ]
    , frequency =
        \frequencyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "frequency"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.tuple
                                        Type.float
                                        (Type.namedWith
                                            [ "Fuzz" ]
                                            "Fuzzer"
                                            [ Type.var "a" ]
                                        )
                                    )
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ frequencyArg ]
    , tuple =
        \tupleArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "tuple"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.tuple
                                    (Type.namedWith
                                        [ "Fuzz" ]
                                        "Fuzzer"
                                        [ Type.var "a" ]
                                    )
                                    (Type.namedWith
                                        [ "Fuzz" ]
                                        "Fuzzer"
                                        [ Type.var "b" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.tuple (Type.var "a") (Type.var "b") ]
                                )
                            )
                    }
                )
                [ tupleArg ]
    , tuple3 =
        \tuple3Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "tuple3"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.triple
                                    (Type.namedWith
                                        [ "Fuzz" ]
                                        "Fuzzer"
                                        [ Type.var "a" ]
                                    )
                                    (Type.namedWith
                                        [ "Fuzz" ]
                                        "Fuzzer"
                                        [ Type.var "b" ]
                                    )
                                    (Type.namedWith
                                        [ "Fuzz" ]
                                        "Fuzzer"
                                        [ Type.var "c" ]
                                    )
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.triple
                                        (Type.var "a")
                                        (Type.var "b")
                                        (Type.var "c")
                                    ]
                                )
                            )
                    }
                )
                [ tuple3Arg ]
    , custom =
        \customArg customArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "custom"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Random" ]
                                    "Generator"
                                    [ Type.var "a" ]
                                , Type.namedWith
                                    [ "Shrink" ]
                                    "Shrinker"
                                    [ Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ customArg, customArg0 ]
    , invalid =
        \invalidArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Fuzz" ]
                    , name = "invalid"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                    }
                )
                [ invalidArg ]
    }


values_ :
    { int : Elm.Expression
    , intRange : Elm.Expression
    , float : Elm.Expression
    , floatRange : Elm.Expression
    , percentage : Elm.Expression
    , string : Elm.Expression
    , bool : Elm.Expression
    , maybe : Elm.Expression
    , result : Elm.Expression
    , list : Elm.Expression
    , array : Elm.Expression
    , oneOf : Elm.Expression
    , constant : Elm.Expression
    , map : Elm.Expression
    , map2 : Elm.Expression
    , map3 : Elm.Expression
    , map4 : Elm.Expression
    , map5 : Elm.Expression
    , andMap : Elm.Expression
    , frequency : Elm.Expression
    , tuple : Elm.Expression
    , tuple3 : Elm.Expression
    , custom : Elm.Expression
    , char : Elm.Expression
    , unit : Elm.Expression
    , order : Elm.Expression
    , invalid : Elm.Expression
    }
values_ =
    { int =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "int"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
            }
    , intRange =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "intRange"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.int ])
                    )
            }
    , float =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "float"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
            }
    , floatRange =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "floatRange"
            , annotation =
                Just
                    (Type.function
                        [ Type.float, Type.float ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
                    )
            }
    , percentage =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "percentage"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.float ])
            }
    , string =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "string"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.string ])
            }
    , bool =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "bool"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.bool ])
            }
    , maybe =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "maybe"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.maybe (Type.var "a") ]
                        )
                    )
            }
    , result =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "result"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.var "error" ]
                        , Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.var "value" ]
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.namedWith
                                [ "Result" ]
                                "Result"
                                [ Type.var "error", Type.var "value" ]
                            ]
                        )
                    )
            }
    , list =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "list"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.list (Type.var "a") ]
                        )
                    )
            }
    , array =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "array"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ] ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.namedWith
                                [ "Array" ]
                                "Array"
                                [ Type.var "a" ]
                            ]
                        )
                    )
            }
    , oneOf =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "oneOf"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , constant =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "constant"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a" ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , map =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function [ Type.var "a" ] (Type.var "b")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ])
                    )
            }
    , map2 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map2"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b" ]
                            (Type.var "c")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ])
                    )
            }
    , map3 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map3"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a", Type.var "b", Type.var "c" ]
                            (Type.var "d")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ])
                    )
            }
    , map4 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map4"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "a"
                            , Type.var "b"
                            , Type.var "c"
                            , Type.var "d"
                            ]
                            (Type.var "e")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ])
                    )
            }
    , map5 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "map5"
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
                            (Type.var "f")
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "d" ]
                        , Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "e" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "f" ])
                    )
            }
    , andMap =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "andMap"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                        , Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.function [ Type.var "a" ] (Type.var "b") ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ])
                    )
            }
    , frequency =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "frequency"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple
                                Type.float
                                (Type.namedWith
                                    [ "Fuzz" ]
                                    "Fuzzer"
                                    [ Type.var "a" ]
                                )
                            )
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , tuple =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "tuple"
            , annotation =
                Just
                    (Type.function
                        [ Type.tuple
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.tuple (Type.var "a") (Type.var "b") ]
                        )
                    )
            }
    , tuple3 =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "tuple3"
            , annotation =
                Just
                    (Type.function
                        [ Type.triple
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ]
                            )
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "b" ]
                            )
                            (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "c" ]
                            )
                        ]
                        (Type.namedWith
                            [ "Fuzz" ]
                            "Fuzzer"
                            [ Type.triple
                                (Type.var "a")
                                (Type.var "b")
                                (Type.var "c")
                            ]
                        )
                    )
            }
    , custom =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "custom"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Random" ]
                            "Generator"
                            [ Type.var "a" ]
                        , Type.namedWith
                            [ "Shrink" ]
                            "Shrinker"
                            [ Type.var "a" ]
                        ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    , char =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "char"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.char ])
            }
    , unit =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "unit"
            , annotation =
                Just (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.unit ])
            }
    , order =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "order"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Fuzz" ]
                        "Fuzzer"
                        [ Type.namedWith [ "Basics" ] "Order" [] ]
                    )
            }
    , invalid =
        Elm.value
            { importFrom = [ "Fuzz" ]
            , name = "invalid"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Fuzz" ] "Fuzzer" [ Type.var "a" ])
                    )
            }
    }


