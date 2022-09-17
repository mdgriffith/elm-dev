module Gen.Expect exposing (all, annotation_, atLeast, atMost, call_, caseOf_, equal, equalDicts, equalLists, equalSets, err, fail, false, greaterThan, lessThan, make_, moduleName_, notEqual, notWithin, ok, onFail, pass, true, values_, within)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, onFail, fail, pass, equalSets, equalDicts, equalLists, err, ok, false, true, notWithin, within, atLeast, greaterThan, atMost, lessThan, all, notEqual, equal, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Expect" ]


{-| Passes if the arguments are equal.

    Expect.equal 0 (List.length [])

    -- Passes because (0 == 0) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because the expected value didn't split the space in "Betty Botter"
    String.split " " "Betty Botter bought some butter"
        |> Expect.equal [ "Betty Botter", "bought", "some", "butter" ]

    {-

    [ "Betty", "Botter", "bought", "some", "butter" ]
    ╷
    │ Expect.equal
    ╵
    [ "Betty Botter", "bought", "some", "butter" ]

    -}

Do not equate `Float` values; use [`within`](#within) instead.

equal: a -> a -> Expect.Expectation
-}
equal : Elm.Expression -> Elm.Expression -> Elm.Expression
equal equalArg equalArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "equal"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a", Type.var "a" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ equalArg, equalArg0 ]


{-| Passes if the arguments are not equal.

    -- Passes because (11 /= 100) is True
    90 + 10
        |> Expect.notEqual 11


    -- Fails because (100 /= 100) is False
    90 + 10
        |> Expect.notEqual 100

    {-

    100
    ╷
    │ Expect.notEqual
    ╵
    100

    -}

notEqual: a -> a -> Expect.Expectation
-}
notEqual : Elm.Expression -> Elm.Expression -> Elm.Expression
notEqual notEqualArg notEqualArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "notEqual"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a", Type.var "a" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ notEqualArg, notEqualArg0 ]


{-| Passes if each of the given functions passes when applied to the subject.

Passing an empty list is assumed to be a mistake, so `Expect.all []`
will always return a failed expectation no matter what else it is passed.

    Expect.all
        [ Expect.greaterThan -2
        , Expect.lessThan 5
        ]
        (List.length [])
    -- Passes because (0 > -2) is True and (0 < 5) is also True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 < -10) is False
    List.length []
        |> Expect.all
            [ Expect.greaterThan -2
            , Expect.lessThan -10
            , Expect.equal 0
            ]
    {-
    0
    ╷
    │ Expect.lessThan
    ╵
    -10
    -}

all: List (subject -> Expect.Expectation) -> subject -> Expect.Expectation
-}
all : List Elm.Expression -> Elm.Expression -> Elm.Expression
all allArg allArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "all"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.function
                                [ Type.var "subject" ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                        , Type.var "subject"
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ Elm.list allArg, allArg0 ]


{-| Passes if the second argument is less than the first.

    Expect.lessThan 1 (List.length [])

    -- Passes because (0 < 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 < -1) is False
    List.length []
        |> Expect.lessThan -1


    {-

    0
    ╷
    │ Expect.lessThan
    ╵
    -1

    -}

Do not equate `Float` values; use [`notWithin`](#notWithin) instead.

lessThan: comparable -> comparable -> Expect.Expectation
-}
lessThan : Elm.Expression -> Elm.Expression -> Elm.Expression
lessThan lessThanArg lessThanArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "lessThan"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable", Type.var "comparable" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ lessThanArg, lessThanArg0 ]


{-| Passes if the second argument is less than or equal to the first.

    Expect.atMost 1 (List.length [])

    -- Passes because (0 <= 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 <= -3) is False
    List.length []
        |> Expect.atMost -3

    {-

    0
    ╷
    │ Expect.atMost
    ╵
    -3

    -}

atMost: comparable -> comparable -> Expect.Expectation
-}
atMost : Elm.Expression -> Elm.Expression -> Elm.Expression
atMost atMostArg atMostArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "atMost"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable", Type.var "comparable" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ atMostArg, atMostArg0 ]


{-| Passes if the second argument is greater than the first.

    Expect.greaterThan -2 List.length []

    -- Passes because (0 > -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 > 1) is False
    List.length []
        |> Expect.greaterThan 1

    {-

    0
    ╷
    │ Expect.greaterThan
    ╵
    1

    -}

greaterThan: comparable -> comparable -> Expect.Expectation
-}
greaterThan : Elm.Expression -> Elm.Expression -> Elm.Expression
greaterThan greaterThanArg greaterThanArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "greaterThan"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable", Type.var "comparable" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ greaterThanArg, greaterThanArg0 ]


{-| Passes if the second argument is greater than or equal to the first.

    Expect.atLeast -2 (List.length [])

    -- Passes because (0 >= -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 >= 3) is False
    List.length []
        |> Expect.atLeast 3

    {-

    0
    ╷
    │ Expect.atLeast
    ╵
    3

    -}

atLeast: comparable -> comparable -> Expect.Expectation
-}
atLeast : Elm.Expression -> Elm.Expression -> Elm.Expression
atLeast atLeastArg atLeastArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "atLeast"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable", Type.var "comparable" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ atLeastArg, atLeastArg0 ]


{-| Passes if the second and third arguments are equal within a tolerance
specified by the first argument. This is intended to avoid failing because of
minor inaccuracies introduced by floating point arithmetic.

    -- Fails because 0.1 + 0.2 == 0.30000000000000004 (0.1 is non-terminating in base 2)
    0.1 + 0.2 |> Expect.equal 0.3

    -- So instead write this test, which passes
    0.1 + 0.2 |> Expect.within (Absolute 0.000000001) 0.3

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because 3.14 is not close enough to pi
    3.14 |> Expect.within (Absolute 0.0001) pi

    {-

    3.14
    ╷
    │ Expect.within Absolute 0.0001
    ╵
    3.141592653589793

    -}

within: Expect.FloatingPointTolerance -> Float -> Float -> Expect.Expectation
-}
within : Elm.Expression -> Float -> Float -> Elm.Expression
within withinArg withinArg0 withinArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "within"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Expect" ]
                            "FloatingPointTolerance"
                            []
                        , Type.float
                        , Type.float
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ withinArg, Elm.float withinArg0, Elm.float withinArg1 ]


{-| Passes if (and only if) a call to `within` with the same arguments would have failed.

notWithin: Expect.FloatingPointTolerance -> Float -> Float -> Expect.Expectation
-}
notWithin : Elm.Expression -> Float -> Float -> Elm.Expression
notWithin notWithinArg notWithinArg0 notWithinArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "notWithin"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Expect" ]
                            "FloatingPointTolerance"
                            []
                        , Type.float
                        , Type.float
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ notWithinArg, Elm.float notWithinArg0, Elm.float notWithinArg1 ]


{-| Passes if the argument is 'True', and otherwise fails with the given message.

    Expect.true "Expected the list to be empty." (List.isEmpty [])

    -- Passes because (List.isEmpty []) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because List.isEmpty returns False, but we expect True.
    List.isEmpty [ 42 ]
        |> Expect.true "Expected the list to be empty."

    {-

    Expected the list to be empty.

    -}

true: String -> Bool -> Expect.Expectation
-}
true : String -> Bool -> Elm.Expression
true trueArg trueArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "true"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.bool ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ Elm.string trueArg, Elm.bool trueArg0 ]


{-| Passes if the argument is 'False', and otherwise fails with the given message.

    Expect.false "Expected the list not to be empty." (List.isEmpty [ 42 ])

    -- Passes because (List.isEmpty [ 42 ]) is False

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (List.isEmpty []) is True
    List.isEmpty []
        |> Expect.false "Expected the list not to be empty."

    {-

    Expected the list not to be empty.

    -}

false: String -> Bool -> Expect.Expectation
-}
false : String -> Bool -> Elm.Expression
false falseArg falseArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "false"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.bool ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ Elm.string falseArg, Elm.bool falseArg0 ]


{-| Passes if the
[`Result`](https://package.elm-lang.org/packages/lang/core/latest/Result) is
an `Ok` rather than `Err`. This is useful for tests where you expect not to see
an error, but you don't care what the actual result is.

_(Tip: If your function returns a `Maybe` instead, consider `Expect.notEqual Nothing`.)_

    -- Passes
    String.toInt "not an int"
        |> Expect.err

Test failures will be printed with the unexpected `Ok` value contrasting with
any `Err`.

    -- Fails
    String.toInt "20"
        |> Expect.err

    {-

    Ok 20
    ╷
    │ Expect.err
    ╵
    Err _

    -}

ok: Result.Result a b -> Expect.Expectation
-}
ok : Elm.Expression -> Elm.Expression
ok okArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "ok"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Result" ]
                            "Result"
                            [ Type.var "a", Type.var "b" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ okArg ]


{-| Passes if the
[`Result`](http://package.elm-lang.org/packages/elm-lang/core/latest/Result) is
an `Err` rather than `Ok`. This is useful for tests where you expect to get an
error but you don't care what the actual error is.

_(Tip: If your function returns a `Maybe` instead, consider `Expect.equal Nothing`.)_

    -- Passes
    String.toInt "not an int"
        |> Expect.err

Test failures will be printed with the unexpected `Ok` value contrasting with
any `Err`.

    -- Fails
    String.toInt "20"
        |> Expect.err

    {-

    Ok 20
    ╷
    │ Expect.err
    ╵
    Err _

    -}

err: Result.Result a b -> Expect.Expectation
-}
err : Elm.Expression -> Elm.Expression
err errArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "err"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Result" ]
                            "Result"
                            [ Type.var "a", Type.var "b" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ errArg ]


{-| Passes if the arguments are equal lists.

    -- Passes
    [ 1, 2, 3 ]
        |> Expect.equalLists [ 1, 2, 3 ]

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which index the lists first
differed at or which list was longer:

    -- Fails
    [ 1, 2, 4, 6 ]
        |> Expect.equalLists [ 1, 2, 5 ]

    {-

    [1,2,4,6]
    first diff at index index 2: +`4`, -`5`
    ╷
    │ Expect.equalLists
    ╵
    first diff at index index 2: +`5`, -`4`
    [1,2,5]

    -}

equalLists: List a -> List a -> Expect.Expectation
-}
equalLists : List Elm.Expression -> List Elm.Expression -> Elm.Expression
equalLists equalListsArg equalListsArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "equalLists"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.var "a"), Type.list (Type.var "a") ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ Elm.list equalListsArg, Elm.list equalListsArg0 ]


{-| Passes if the arguments are equal dicts.

    -- Passes
    Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ]
        |> Expect.equalDicts (Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ])

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which keys were missing from
or added to each dict:

    -- Fails
    (Dict.fromList [ ( 1, "one" ), ( 2, "too" ) ])
        |> Expect.equalDicts (Dict.fromList [ ( 1, "one" ), ( 2, "two" ), ( 3, "three" ) ])

    {-

    Dict.fromList [(1,"one"),(2,"too")]
    diff: -[ (2,"two"), (3,"three") ] +[ (2,"too") ]
    ╷
    │ Expect.equalDicts
    ╵
    diff: +[ (2,"two"), (3,"three") ] -[ (2,"too") ]
    Dict.fromList [(1,"one"),(2,"two"),(3,"three")]

    -}

equalDicts: Dict.Dict comparable a -> Dict.Dict comparable a -> Expect.Expectation
-}
equalDicts : Elm.Expression -> Elm.Expression -> Elm.Expression
equalDicts equalDictsArg equalDictsArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "equalDicts"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Dict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "a" ]
                        , Type.namedWith
                            [ "Dict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "a" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ equalDictsArg, equalDictsArg0 ]


{-| Passes if the arguments are equal sets.

    -- Passes
    Set.fromList [ 1, 2 ]
        |> Expect.equalSets (Set.fromList [ 1, 2 ])

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which keys were missing from
or added to each set:

    -- Fails
    (Set.fromList [ 1, 2, 4, 6 ])
        |> Expect.equalSets (Set.fromList [ 1, 2, 5 ])

    {-

    Set.fromList [1,2,4,6]
    diff: -[ 5 ] +[ 4, 6 ]
    ╷
    │ Expect.equalSets
    ╵
    diff: +[ 5 ] -[ 4, 6 ]
    Set.fromList [1,2,5]

    -}

equalSets: Set.Set comparable -> Set.Set comparable -> Expect.Expectation
-}
equalSets : Elm.Expression -> Elm.Expression -> Elm.Expression
equalSets equalSetsArg equalSetsArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "equalSets"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Set" ]
                            "Set"
                            [ Type.var "comparable" ]
                        , Type.namedWith
                            [ "Set" ]
                            "Set"
                            [ Type.var "comparable" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ equalSetsArg, equalSetsArg0 ]


{-| Always passes.

    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)
    import Expect


    test "Json.Decode.int can decode the number 42." <|
        \_ ->
            case decodeString int "42" of
                Ok _ ->
                    Expect.pass

                Err err ->
                    Expect.fail err

pass: Expect.Expectation
-}
pass : Elm.Expression
pass =
    Elm.value
        { importFrom = [ "Expect" ]
        , name = "pass"
        , annotation = Just (Type.namedWith [ "Expect" ] "Expectation" [])
        }


{-| Fails with the given message.

    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)
    import Expect


    test "Json.Decode.int can decode the number 42." <|
        \_ ->
            case decodeString int "42" of
                Ok _ ->
                    Expect.pass

                Err err ->
                    Expect.fail err

fail: String -> Expect.Expectation
-}
fail : String -> Elm.Expression
fail failArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "fail"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ Elm.string failArg ]


{-| If the given expectation fails, replace its failure message with a custom one.

    "something"
        |> Expect.equal "something else"
        |> Expect.onFail "thought those two strings would be the same"

onFail: String -> Expect.Expectation -> Expect.Expectation
-}
onFail : String -> Elm.Expression -> Elm.Expression
onFail onFailArg onFailArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Expect" ]
            , name = "onFail"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Expect" ] "Expectation" []
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ Elm.string onFailArg, onFailArg0 ]


annotation_ :
    { expectation : Type.Annotation, floatingPointTolerance : Type.Annotation }
annotation_ =
    { expectation =
        Type.alias
            moduleName_
            "Expectation"
            []
            (Type.namedWith [ "Test", "Expectation" ] "Expectation" [])
    , floatingPointTolerance =
        Type.namedWith [ "Expect" ] "FloatingPointTolerance" []
    }


make_ :
    { absolute : Elm.Expression -> Elm.Expression
    , relative : Elm.Expression -> Elm.Expression
    , absoluteOrRelative : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
make_ =
    { absolute =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "Absolute"
                    , annotation =
                        Just (Type.namedWith [] "FloatingPointTolerance" [])
                    }
                )
                [ ar0 ]
    , relative =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "Relative"
                    , annotation =
                        Just (Type.namedWith [] "FloatingPointTolerance" [])
                    }
                )
                [ ar0 ]
    , absoluteOrRelative =
        \ar0 ar1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "AbsoluteOrRelative"
                    , annotation =
                        Just (Type.namedWith [] "FloatingPointTolerance" [])
                    }
                )
                [ ar0, ar1 ]
    }


caseOf_ :
    { floatingPointTolerance :
        Elm.Expression
        -> { floatingPointToleranceTags_0_0
            | absolute : Elm.Expression -> Elm.Expression
            , relative : Elm.Expression -> Elm.Expression
            , absoluteOrRelative :
                Elm.Expression -> Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { floatingPointTolerance =
        \floatingPointToleranceExpression floatingPointToleranceTags ->
            Elm.Case.custom
                floatingPointToleranceExpression
                (Type.namedWith [ "Expect" ] "FloatingPointTolerance" [])
                [ Elm.Case.branch1
                    "Absolute"
                    ( "basics.Float", Type.float )
                    floatingPointToleranceTags.absolute
                , Elm.Case.branch1
                    "Relative"
                    ( "basics.Float", Type.float )
                    floatingPointToleranceTags.relative
                , Elm.Case.branch2
                    "AbsoluteOrRelative"
                    ( "basics.Float", Type.float )
                    ( "basics.Float", Type.float )
                    floatingPointToleranceTags.absoluteOrRelative
                ]
    }


call_ :
    { equal : Elm.Expression -> Elm.Expression -> Elm.Expression
    , notEqual : Elm.Expression -> Elm.Expression -> Elm.Expression
    , all : Elm.Expression -> Elm.Expression -> Elm.Expression
    , lessThan : Elm.Expression -> Elm.Expression -> Elm.Expression
    , atMost : Elm.Expression -> Elm.Expression -> Elm.Expression
    , greaterThan : Elm.Expression -> Elm.Expression -> Elm.Expression
    , atLeast : Elm.Expression -> Elm.Expression -> Elm.Expression
    , within :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , notWithin :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , true : Elm.Expression -> Elm.Expression -> Elm.Expression
    , false : Elm.Expression -> Elm.Expression -> Elm.Expression
    , ok : Elm.Expression -> Elm.Expression
    , err : Elm.Expression -> Elm.Expression
    , equalLists : Elm.Expression -> Elm.Expression -> Elm.Expression
    , equalDicts : Elm.Expression -> Elm.Expression -> Elm.Expression
    , equalSets : Elm.Expression -> Elm.Expression -> Elm.Expression
    , fail : Elm.Expression -> Elm.Expression
    , onFail : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { equal =
        \equalArg equalArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "equal"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "a", Type.var "a" ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ equalArg, equalArg0 ]
    , notEqual =
        \notEqualArg notEqualArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "notEqual"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "a", Type.var "a" ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ notEqualArg, notEqualArg0 ]
    , all =
        \allArg allArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "all"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.function
                                        [ Type.var "subject" ]
                                        (Type.namedWith
                                            [ "Expect" ]
                                            "Expectation"
                                            []
                                        )
                                    )
                                , Type.var "subject"
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ allArg, allArg0 ]
    , lessThan =
        \lessThanArg lessThanArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "lessThan"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "comparable", Type.var "comparable" ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ lessThanArg, lessThanArg0 ]
    , atMost =
        \atMostArg atMostArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "atMost"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "comparable", Type.var "comparable" ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ atMostArg, atMostArg0 ]
    , greaterThan =
        \greaterThanArg greaterThanArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "greaterThan"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "comparable", Type.var "comparable" ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ greaterThanArg, greaterThanArg0 ]
    , atLeast =
        \atLeastArg atLeastArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "atLeast"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "comparable", Type.var "comparable" ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ atLeastArg, atLeastArg0 ]
    , within =
        \withinArg withinArg0 withinArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "within"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Expect" ]
                                    "FloatingPointTolerance"
                                    []
                                , Type.float
                                , Type.float
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ withinArg, withinArg0, withinArg1 ]
    , notWithin =
        \notWithinArg notWithinArg0 notWithinArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "notWithin"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Expect" ]
                                    "FloatingPointTolerance"
                                    []
                                , Type.float
                                , Type.float
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ notWithinArg, notWithinArg0, notWithinArg1 ]
    , true =
        \trueArg trueArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "true"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.bool ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ trueArg, trueArg0 ]
    , false =
        \falseArg falseArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "false"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.bool ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ falseArg, falseArg0 ]
    , ok =
        \okArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "ok"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Result" ]
                                    "Result"
                                    [ Type.var "a", Type.var "b" ]
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ okArg ]
    , err =
        \errArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "err"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Result" ]
                                    "Result"
                                    [ Type.var "a", Type.var "b" ]
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ errArg ]
    , equalLists =
        \equalListsArg equalListsArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "equalLists"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list (Type.var "a")
                                , Type.list (Type.var "a")
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ equalListsArg, equalListsArg0 ]
    , equalDicts =
        \equalDictsArg equalDictsArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "equalDicts"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Dict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "a" ]
                                , Type.namedWith
                                    [ "Dict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "a" ]
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ equalDictsArg, equalDictsArg0 ]
    , equalSets =
        \equalSetsArg equalSetsArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "equalSets"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Set" ]
                                    "Set"
                                    [ Type.var "comparable" ]
                                , Type.namedWith
                                    [ "Set" ]
                                    "Set"
                                    [ Type.var "comparable" ]
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ equalSetsArg, equalSetsArg0 ]
    , fail =
        \failArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "fail"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ failArg ]
    , onFail =
        \onFailArg onFailArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Expect" ]
                    , name = "onFail"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string
                                , Type.namedWith [ "Expect" ] "Expectation" []
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ onFailArg, onFailArg0 ]
    }


values_ :
    { equal : Elm.Expression
    , notEqual : Elm.Expression
    , all : Elm.Expression
    , lessThan : Elm.Expression
    , atMost : Elm.Expression
    , greaterThan : Elm.Expression
    , atLeast : Elm.Expression
    , within : Elm.Expression
    , notWithin : Elm.Expression
    , true : Elm.Expression
    , false : Elm.Expression
    , ok : Elm.Expression
    , err : Elm.Expression
    , equalLists : Elm.Expression
    , equalDicts : Elm.Expression
    , equalSets : Elm.Expression
    , pass : Elm.Expression
    , fail : Elm.Expression
    , onFail : Elm.Expression
    }
values_ =
    { equal =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "equal"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a", Type.var "a" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , notEqual =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "notEqual"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "a", Type.var "a" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , all =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "all"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.function
                                [ Type.var "subject" ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                        , Type.var "subject"
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , lessThan =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "lessThan"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable", Type.var "comparable" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , atMost =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "atMost"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable", Type.var "comparable" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , greaterThan =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "greaterThan"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable", Type.var "comparable" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , atLeast =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "atLeast"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable", Type.var "comparable" ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , within =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "within"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Expect" ]
                            "FloatingPointTolerance"
                            []
                        , Type.float
                        , Type.float
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , notWithin =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "notWithin"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Expect" ]
                            "FloatingPointTolerance"
                            []
                        , Type.float
                        , Type.float
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , true =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "true"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.bool ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , false =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "false"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.bool ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , ok =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "ok"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Result" ]
                            "Result"
                            [ Type.var "a", Type.var "b" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , err =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "err"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Result" ]
                            "Result"
                            [ Type.var "a", Type.var "b" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , equalLists =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "equalLists"
            , annotation =
                Just
                    (Type.function
                        [ Type.list (Type.var "a"), Type.list (Type.var "a") ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , equalDicts =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "equalDicts"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Dict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "a" ]
                        , Type.namedWith
                            [ "Dict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "a" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , equalSets =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "equalSets"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Set" ]
                            "Set"
                            [ Type.var "comparable" ]
                        , Type.namedWith
                            [ "Set" ]
                            "Set"
                            [ Type.var "comparable" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , pass =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "pass"
            , annotation = Just (Type.namedWith [ "Expect" ] "Expectation" [])
            }
    , fail =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "fail"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , onFail =
        Elm.value
            { importFrom = [ "Expect" ]
            , name = "onFail"
            , annotation =
                Just
                    (Type.function
                        [ Type.string
                        , Type.namedWith [ "Expect" ] "Expectation" []
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    }


