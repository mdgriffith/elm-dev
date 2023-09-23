module Gen.Test.Html.Query exposing (annotation_, call_, children, contains, count, each, find, findAll, first, fromHtml, has, hasNot, index, keep, moduleName_, values_)

{-| 
@docs values_, call_, annotation_, each, hasNot, has, contains, count, keep, index, first, children, findAll, find, fromHtml, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Test", "Html", "Query" ]


{-| Translate a `Html` value into a `Single` query. This is how queries
typically begin.

    import Html
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (text)


    test "Button has the expected text" <|
        \() ->
            Html.button [] [ Html.text "I'm a button!" ]
                |> Query.fromHtml
                |> Query.has [ text "I'm a button!" ]

fromHtml: Html.Html msg -> Test.Html.Query.Single msg
-}
fromHtml : Elm.Expression -> Elm.Expression
fromHtml fromHtmlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "fromHtml"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ] ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ fromHtmlArg ]


{-| Find exactly one descendant element which matches all the given selectors.
If no descendants match, or if more than one matches, the test will fail.

    import Html exposing (div, ul, li)
    import Html.Attributes exposing (class)
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag, classes)


    test "The list has both the classes 'items' and 'active'" <|
        \() ->
            div []
                [ ul [ class "items active" ]
                    [ li [] [ text "first item" ]
                    , li [] [ text "second item" ]
                    , li [] [ text "third item" ]
                    ]
                ]
                |> Query.fromHtml
                |> Query.find [ tag "ul" ]
                |> Query.has [ classes [ "items", "active" ] ]

find: 
    List Test.Html.Selector.Internal.Selector
    -> Test.Html.Query.Single msg
    -> Test.Html.Query.Single msg
-}
find : List Elm.Expression -> Elm.Expression -> Elm.Expression
find findArg findArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "find"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector", "Internal" ]
                                "Selector"
                                []
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list findArg, findArg0 ]


{-| Find the descendant elements which match all the given selectors.

    import Html exposing (div, ul, li)
    import Html.Attributes exposing (class)
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag)
    import Expect


    test "The list has three items" <|
        \() ->
            div []
                [ ul [ class "items active" ]
                    [ li [] [ text "first item" ]
                    , li [] [ text "second item" ]
                    , li [] [ text "third item" ]
                    ]
                ]
                |> Query.fromHtml
                |> Query.findAll [ tag "li" ]
                |> Query.count (Expect.equal 3)

findAll: 
    List Test.Html.Selector.Internal.Selector
    -> Test.Html.Query.Single msg
    -> Test.Html.Query.Multiple msg
-}
findAll : List Elm.Expression -> Elm.Expression -> Elm.Expression
findAll findAllArg findAllArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "findAll"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector", "Internal" ]
                                "Selector"
                                []
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list findAllArg, findAllArg0 ]


{-| Return the matched element's immediate child elements.

    import Html exposing (div, ul, li)
    import Html.Attributes exposing (class)
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag, classes)


    test "The <ul> only has <li> children" <|
        \() ->
            div []
                [ ul [ class "items active" ]
                    [ li [ class "item"] [ text "first item" ]
                    , li [ class "item selected"] [ text "second item" ]
                    , li [ class "item"] [ text "third item" ]
                    ]
                ]
                |> Query.fromHtml
                |> Query.find [ class "items" ]
                |> Query.children [ class "selected" ]
                |> Query.count (Expect.equal 1)

children: 
    List Test.Html.Selector.Internal.Selector
    -> Test.Html.Query.Single msg
    -> Test.Html.Query.Multiple msg
-}
children : List Elm.Expression -> Elm.Expression -> Elm.Expression
children childrenArg childrenArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "children"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector", "Internal" ]
                                "Selector"
                                []
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list childrenArg, childrenArg0 ]


{-| Return the first element in a match. If there were no matches, the test
will fail.

`Query.first` is a shorthand for `Query.index 0` - they do the same thing.

    import Html exposing (div, ul, li)
    import Html.Attributes exposing (class)
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag, classes)


    test "The first <li> is called 'first item'" <|
        \() ->
            div []
                [ ul [ class "items active" ]
                    [ li [] [ text "first item" ]
                    , li [] [ text "second item" ]
                    , li [] [ text "third item" ]
                    ]
                ]
                |> Query.fromHtml
                |> Query.findAll [ tag "li" ]
                |> Query.first
                |> Query.has [ text "first item" ]

first: Test.Html.Query.Multiple msg -> Test.Html.Query.Single msg
-}
first : Elm.Expression -> Elm.Expression
first firstArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "first"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ firstArg ]


{-| Return the element in a match at the given index. For example,
`Query.index 0` would match the first element, and `Query.index 1` would match
the second element.

You can pass negative numbers to get elements from the end - for example, `Query.index -1`
will match the last element, and `Query.index -2` will match the second-to-last.

If the index falls outside the bounds of the match, the test will fail.

    import Html exposing (div, ul, li)
    import Html.Attributes exposing (class)
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag, classes)


    test "The second <li> is called 'second item'" <|
        \() ->
            div []
                [ ul [ class "items active" ]
                    [ li [] [ text "first item" ]
                    , li [] [ text "second item" ]
                    , li [] [ text "third item" ]
                    ]
                ]
                |> Query.fromHtml
                |> Query.findAll [ tag "li" ]
                |> Query.index 1
                |> Query.has [ text "second item" ]

index: Int -> Test.Html.Query.Multiple msg -> Test.Html.Query.Single msg
-}
index : Int -> Elm.Expression -> Elm.Expression
index indexArg indexArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "index"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int indexArg, indexArg0 ]


{-| Find the descendant elements of the result of `findAll` which match all the given selectors.

    import Html exposing (div, ul, li)
    import Html.Attributes exposing (class)
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag)
    import Expect


    test "The list has three items" <|
        \() ->
            div []
                [ ul [ class "items active" ]
                    [ li [] [ a [] [ text "first item" ]]
                    , li [] [ a [] [ text "second item" ]]
                    , li [] [ a [] [ text "third item" ]]
                    , li [] [ button [] [ text "button" ]]
                    ]
                ]
                |> Query.fromHtml
                |> Query.findAll [ tag "li" ]
                |> Query.keep ( tag "a" )
                |> Expect.all
                    [ Query.each (Query.has [ tag "a" ])
                    , Query.first >> Query.has [ text "first item" ]
                    ]

keep: 
    Test.Html.Selector.Internal.Selector
    -> Test.Html.Query.Multiple msg
    -> Test.Html.Query.Multiple msg
-}
keep : Elm.Expression -> Elm.Expression -> Elm.Expression
keep keepArg keepArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "keep"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Test", "Html", "Selector", "Internal" ]
                            "Selector"
                            []
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ keepArg, keepArg0 ]


{-| Expect the number of elements matching the query fits the given expectation.

    import Html exposing (div, ul, li)
    import Html.Attributes exposing (class)
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag)
    import Expect


    test "The list has three items" <|
        \() ->
            div []
                [ ul [ class "items active" ]
                    [ li [] [ text "first item" ]
                    , li [] [ text "second item" ]
                    , li [] [ text "third item" ]
                    ]
                ]
                |> Query.fromHtml
                |> Query.findAll [ tag "li" ]
                |> Query.count (Expect.equal 3)

count: 
    (Int -> Expect.Expectation)
    -> Test.Html.Query.Multiple msg
    -> Expect.Expectation
-}
count : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
count countArg countArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "count"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.int ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ Elm.functionReduced "countUnpack" countArg, countArg0 ]


{-| Expect the element to have at least one descendant matching

    import Html exposing (div, ul, li)
    import Html.Attributes exposing (class)
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag, classes)


    test "The list has two li: one with the text \"third item\" and \
        another one with \"first item\"" <|
        \() ->
            div []
                [ ul [ class "items active" ]
                    [ li [] [ text "first item" ]
                    , li [] [ text "second item" ]
                    , li [] [ text "third item" ]
                    ]
                ]
                |> Query.fromHtml
                |> Query.contains
                    [ li [] [ text "third item" ]
                    , li [] [ text "first item" ]
                    ]

contains: List (Html.Html msg) -> Test.Html.Query.Single msg -> Expect.Expectation
-}
contains : List Elm.Expression -> Elm.Expression -> Elm.Expression
contains containsArg containsArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "contains"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ Elm.list containsArg, containsArg0 ]


{-| Expect the element to match all of the given selectors.

    import Html exposing (div, ul, li)
    import Html.Attributes exposing (class)
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag, classes)


    test "The list has both the classes 'items' and 'active'" <|
        \() ->
            div []
                [ ul [ class "items active" ]
                    [ li [] [ text "first item" ]
                    , li [] [ text "second item" ]
                    , li [] [ text "third item" ]
                    ]
                ]
                |> Query.fromHtml
                |> Query.find [ tag "ul" ]
                |> Query.has [ tag "ul", classes [ "items", "active" ] ]

has: 
    List Test.Html.Selector.Internal.Selector
    -> Test.Html.Query.Single msg
    -> Expect.Expectation
-}
has : List Elm.Expression -> Elm.Expression -> Elm.Expression
has hasArg hasArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "has"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector", "Internal" ]
                                "Selector"
                                []
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ Elm.list hasArg, hasArg0 ]


{-| Expect the element to **not** match all of the given selectors.

    import Html exposing (div)
    import Html.Attributes as Attributes
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag, class)


    test "The div element has no progress-bar class" <|
        \() ->
            div [ Attributes.class "button" ] []
                |> Query.fromHtml
                |> Query.find [ tag "div" ]
                |> Query.hasNot [ tag "div", class "progress-bar" ]

hasNot: 
    List Test.Html.Selector.Internal.Selector
    -> Test.Html.Query.Single msg
    -> Expect.Expectation
-}
hasNot : List Elm.Expression -> Elm.Expression -> Elm.Expression
hasNot hasNotArg hasNotArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "hasNot"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector", "Internal" ]
                                "Selector"
                                []
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ Elm.list hasNotArg, hasNotArg0 ]


{-| Expect that a [`Single`](#Single) expectation will hold true for each of the
[`Multiple`](#Multiple) matched elements.

    import Html exposing (div, ul, li)
    import Html.Attributes exposing (class)
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag, classes)


    test "The list has both the classes 'items' and 'active'" <|
        \() ->
            div []
                [ ul [ class "items active" ]
                    [ li [] [ text "first item" ]
                    , li [] [ text "second item" ]
                    , li [] [ text "third item" ]
                    ]
                ]
                |> Query.fromHtml
                |> Query.findAll [ tag "ul" ]
                |> Query.each
                    (Expect.all
                        [ Query.has [ tag "ul" ]
                        , Query.has [ classes [ "items", "active" ] ]
                        ]
                    )

each: 
    (Test.Html.Query.Single msg -> Expect.Expectation)
    -> Test.Html.Query.Multiple msg
    -> Expect.Expectation
-}
each : (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
each eachArg eachArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "each"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith
                                [ "Test", "Html", "Query" ]
                                "Single"
                                [ Type.var "msg" ]
                            ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
        )
        [ Elm.functionReduced "eachUnpack" eachArg, eachArg0 ]


annotation_ :
    { single : Type.Annotation -> Type.Annotation
    , multiple : Type.Annotation -> Type.Annotation
    }
annotation_ =
    { single =
        \singleArg0 ->
            Type.alias
                moduleName_
                "Single"
                [ singleArg0 ]
                (Type.namedWith
                    [ "Test", "Html", "Query", "Internal" ]
                    "Single"
                    [ Type.var "msg" ]
                )
    , multiple =
        \multipleArg0 ->
            Type.alias
                moduleName_
                "Multiple"
                [ multipleArg0 ]
                (Type.namedWith
                    [ "Test", "Html", "Query", "Internal" ]
                    "Multiple"
                    [ Type.var "msg" ]
                )
    }


call_ :
    { fromHtml : Elm.Expression -> Elm.Expression
    , find : Elm.Expression -> Elm.Expression -> Elm.Expression
    , findAll : Elm.Expression -> Elm.Expression -> Elm.Expression
    , children : Elm.Expression -> Elm.Expression -> Elm.Expression
    , first : Elm.Expression -> Elm.Expression
    , index : Elm.Expression -> Elm.Expression -> Elm.Expression
    , keep : Elm.Expression -> Elm.Expression -> Elm.Expression
    , count : Elm.Expression -> Elm.Expression -> Elm.Expression
    , contains : Elm.Expression -> Elm.Expression -> Elm.Expression
    , has : Elm.Expression -> Elm.Expression -> Elm.Expression
    , hasNot : Elm.Expression -> Elm.Expression -> Elm.Expression
    , each : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
call_ =
    { fromHtml =
        \fromHtmlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "fromHtml"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Html" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Single"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ fromHtmlArg ]
    , find =
        \findArg findArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "find"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Test"
                                        , "Html"
                                        , "Selector"
                                        , "Internal"
                                        ]
                                        "Selector"
                                        []
                                    )
                                , Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Single"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Single"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ findArg, findArg0 ]
    , findAll =
        \findAllArg findAllArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "findAll"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Test"
                                        , "Html"
                                        , "Selector"
                                        , "Internal"
                                        ]
                                        "Selector"
                                        []
                                    )
                                , Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Single"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Multiple"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ findAllArg, findAllArg0 ]
    , children =
        \childrenArg childrenArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "children"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Test"
                                        , "Html"
                                        , "Selector"
                                        , "Internal"
                                        ]
                                        "Selector"
                                        []
                                    )
                                , Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Single"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Multiple"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ childrenArg, childrenArg0 ]
    , first =
        \firstArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "first"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Multiple"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Single"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ firstArg ]
    , index =
        \indexArg indexArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "index"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int
                                , Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Multiple"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Single"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ indexArg, indexArg0 ]
    , keep =
        \keepArg keepArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "keep"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Test", "Html", "Selector", "Internal" ]
                                    "Selector"
                                    []
                                , Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Multiple"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Multiple"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ keepArg, keepArg0 ]
    , count =
        \countArg countArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "count"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.int ]
                                    (Type.namedWith
                                        [ "Expect" ]
                                        "Expectation"
                                        []
                                    )
                                , Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Multiple"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ countArg, countArg0 ]
    , contains =
        \containsArg containsArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "contains"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Html" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                                , Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Single"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ containsArg, containsArg0 ]
    , has =
        \hasArg hasArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "has"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Test"
                                        , "Html"
                                        , "Selector"
                                        , "Internal"
                                        ]
                                        "Selector"
                                        []
                                    )
                                , Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Single"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ hasArg, hasArg0 ]
    , hasNot =
        \hasNotArg hasNotArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "hasNot"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Test"
                                        , "Html"
                                        , "Selector"
                                        , "Internal"
                                        ]
                                        "Selector"
                                        []
                                    )
                                , Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Single"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ hasNotArg, hasNotArg0 ]
    , each =
        \eachArg eachArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Query" ]
                    , name = "each"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.namedWith
                                        [ "Test", "Html", "Query" ]
                                        "Single"
                                        [ Type.var "msg" ]
                                    ]
                                    (Type.namedWith
                                        [ "Expect" ]
                                        "Expectation"
                                        []
                                    )
                                , Type.namedWith
                                    [ "Test", "Html", "Query" ]
                                    "Multiple"
                                    [ Type.var "msg" ]
                                ]
                                (Type.namedWith [ "Expect" ] "Expectation" [])
                            )
                    }
                )
                [ eachArg, eachArg0 ]
    }


values_ :
    { fromHtml : Elm.Expression
    , find : Elm.Expression
    , findAll : Elm.Expression
    , children : Elm.Expression
    , first : Elm.Expression
    , index : Elm.Expression
    , keep : Elm.Expression
    , count : Elm.Expression
    , contains : Elm.Expression
    , has : Elm.Expression
    , hasNot : Elm.Expression
    , each : Elm.Expression
    }
values_ =
    { fromHtml =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "fromHtml"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ] ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , find =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "find"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector", "Internal" ]
                                "Selector"
                                []
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , findAll =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "findAll"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector", "Internal" ]
                                "Selector"
                                []
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , children =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "children"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector", "Internal" ]
                                "Selector"
                                []
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , first =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "first"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , index =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "index"
            , annotation =
                Just
                    (Type.function
                        [ Type.int
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , keep =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "keep"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Test", "Html", "Selector", "Internal" ]
                            "Selector"
                            []
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , count =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "count"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.int ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , contains =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "contains"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ]
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , has =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "has"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector", "Internal" ]
                                "Selector"
                                []
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , hasNot =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "hasNot"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector", "Internal" ]
                                "Selector"
                                []
                            )
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Single"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    , each =
        Elm.value
            { importFrom = [ "Test", "Html", "Query" ]
            , name = "each"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.namedWith
                                [ "Test", "Html", "Query" ]
                                "Single"
                                [ Type.var "msg" ]
                            ]
                            (Type.namedWith [ "Expect" ] "Expectation" [])
                        , Type.namedWith
                            [ "Test", "Html", "Query" ]
                            "Multiple"
                            [ Type.var "msg" ]
                        ]
                        (Type.namedWith [ "Expect" ] "Expectation" [])
                    )
            }
    }


