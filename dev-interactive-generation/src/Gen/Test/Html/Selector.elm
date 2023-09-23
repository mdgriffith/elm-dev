module Gen.Test.Html.Selector exposing (all, annotation_, attribute, call_, checked, class, classes, containing, disabled, exactClassName, id, moduleName_, selected, style, tag, text, values_)

{-| 
@docs values_, call_, annotation_, disabled, selected, checked, style, exactClassName, classes, class, id, all, attribute, containing, text, tag, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Test", "Html", "Selector" ]


{-| Matches elements that have the given tag.

    import Html
    import Html.Attributes as Attr
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (tag, text)


    test "the welcome <h1> says hello!" <|
        \() ->
            Html.div []
                [ Html.h1 [ Attr.id "welcome" ] [ Html.text "Hello!" ] ]
                |> Query.fromHtml
                |> Query.find [ tag "h1" ]
                |> Query.has [ text "Hello!" ]

tag: String -> Test.Html.Selector.Selector
-}
tag : String -> Elm.Expression
tag tagArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "tag"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.string tagArg ]


{-| Matches elements that have a
[`text`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Attributes#text)
attribute with the given value.

text: String -> Test.Html.Selector.Selector
-}
text : String -> Elm.Expression
text textArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.string textArg ]


{-| Matches elements whose descendants match the given selectors.

(You will get the element and **not** the descendant.)

This is especially useful to find elements which contain specific
text somewhere in their descendants.

    import Html
    import Html.Events exposing (onClick)
    import Test exposing (test)
    import Test.Html.Event as Event
    import Test.Html.Query as Query
    import Test.Html.Selector exposing (containing, tag)

    test : Test
    test =
        test "..." <|
            Html.div []
                [ Html.button [ onClick NopeMsg ] [ Html.text "not me" ]
                , Html.button [ onClick ClickedMsg ] [ Html.text "click me" ]
                ]
                |> Query.find
                    [ tag "button"
                    , containing [ text "click me" ]
                    ]
                |> Event.simulate Event.click
                |> Event.expect ClickedMsg

containing: List Test.Html.Selector.Selector -> Test.Html.Selector.Selector
-}
containing : List Elm.Expression -> Elm.Expression
containing containingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "containing"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector" ]
                                "Selector"
                                []
                            )
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.list containingArg ]


{-| Matches elements that have the given attribute in a way that makes sense
given their semantics in `Html`.

attribute: Html.Attribute Basics.Never -> Test.Html.Selector.Selector
-}
attribute : Elm.Expression -> Elm.Expression
attribute attributeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "attribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Html" ]
                            "Attribute"
                            [ Type.namedWith [ "Basics" ] "Never" [] ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ attributeArg ]


{-| Combine the given selectors into one which requires all of them to match.

    import Html
    import Html.Attributes as Attr
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (class, text, all, Selector)


    replyBtnSelector : Selector
    replyBtnSelector =
        all [ class "btn", text "Reply" ]


    test "Button has the class 'btn' and the text 'Reply'" <|
        \() ->
            Html.button [ Attr.class "btn btn-large" ] [ Html.text "Reply" ]
                |> Query.fromHtml
                |> Query.has [ replyBtnSelector ]

all: List Test.Html.Selector.Selector -> Test.Html.Selector.Selector
-}
all : List Elm.Expression -> Elm.Expression
all allArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "all"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector" ]
                                "Selector"
                                []
                            )
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.list allArg ]


{-| Matches elements that have the given `id` attribute.

    import Html
    import Html.Attributes as Attr
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (id, text)


    test "the welcome <h1> says hello!" <|
        \() ->
            Html.div []
                [ Html.h1 [ Attr.id "welcome" ] [ Html.text "Hello!" ] ]
                |> Query.fromHtml
                |> Query.find [ id "welcome" ]
                |> Query.has [ text "Hello!" ]

id: String -> Test.Html.Selector.Selector
-}
id : String -> Elm.Expression
id idArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.string idArg ]


{-| Matches elements that have the given class (and possibly others as well).

To match multiple classes at once, use [`classes`](#classes) instead.

To match the element's exact class attribute string, use [`exactClassName`](#exactClassName).

    import Html
    import Html.Attributes as Attr
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (class)


    test "Button has the class btn-large" <|
        \() ->
            Html.button [ Attr.class "btn btn-large" ] [ Html.text "Reply" ]
                |> Query.fromHtml
                |> Query.has [ class "btn-large" ]

class: String -> Test.Html.Selector.Selector
-}
class : String -> Elm.Expression
class classArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "class"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.string classArg ]


{-| Matches elements that have all the given classes (and possibly others as well).

When you only care about one class instead of several, you can use
[`class`](#class) instead of passing this function a list with one value in it.

To match the element's exact class attribute string, use [`exactClassName`](#exactClassName).

    import Html
    import Html.Attributes as Attr
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (classes)


    test "Button has the classes btn and btn-large" <|
        \() ->
            Html.button [ Attr.class "btn btn-large" ] [ Html.text "Reply" ]
                |> Query.fromHtml
                |> Query.has [ classes [ "btn", "btn-large" ] ]

classes: List String -> Test.Html.Selector.Selector
-}
classes : List String -> Elm.Expression
classes classesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "classes"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.list (List.map Elm.string classesArg) ]


{-| Matches the element's exact class attribute string.

This is used less often than [`class`](#class), [`classes`](#classes) or
[`attribute`](#attribute), which check for the _presence_ of a class as opposed
to matching the entire class attribute exactly.

    import Html
    import Html.Attributes as Attr
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (exactClassName)


    test "Button has the exact class 'btn btn-large'" <|
        \() ->
            Html.button [ Attr.class "btn btn-large" ] [ Html.text "Reply" ]
                |> Query.fromHtml
                |> Query.has [ exactClassName "btn btn-large" ]

exactClassName: String -> Test.Html.Selector.Selector
-}
exactClassName : String -> Elm.Expression
exactClassName exactClassNameArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "exactClassName"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.string exactClassNameArg ]


{-| Matches elements that have the given style properties (and possibly others as well).

    import Html
    import Html.Attributes as Attr
    import Test.Html.Query as Query
    import Test exposing (test)
    import Test.Html.Selector exposing (classes)


    test "the Reply button has red text" <|
        \() ->
            Html.div []
                [ Html.button
                    [ Attr.style "color" "red" ]
                    [ Html.text "Reply" ]
                ]
                |> Query.has [ style "color" "red" ]

style: String -> String -> Test.Html.Selector.Selector
-}
style : String -> String -> Elm.Expression
style styleArg styleArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "style"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.string styleArg, Elm.string styleArg0 ]


{-| Matches elements that have a
[`checked`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Attributes#checked)
attribute with the given value.

checked: Bool -> Test.Html.Selector.Selector
-}
checked : Bool -> Elm.Expression
checked checkedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "checked"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.bool checkedArg ]


{-| Matches elements that have a
[`selected`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Attributes#selected)
attribute with the given value.

selected: Bool -> Test.Html.Selector.Selector
-}
selected : Bool -> Elm.Expression
selected selectedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "selected"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.bool selectedArg ]


{-| Matches elements that have a
[`disabled`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Attributes#disabled)
attribute with the given value.

disabled: Bool -> Test.Html.Selector.Selector
-}
disabled : Bool -> Elm.Expression
disabled disabledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
        )
        [ Elm.bool disabledArg ]


annotation_ : { selector : Type.Annotation }
annotation_ =
    { selector =
        Type.alias
            moduleName_
            "Selector"
            []
            (Type.namedWith
                [ "Test", "Html", "Selector", "Internal" ]
                "Selector"
                []
            )
    }


call_ :
    { tag : Elm.Expression -> Elm.Expression
    , text : Elm.Expression -> Elm.Expression
    , containing : Elm.Expression -> Elm.Expression
    , attribute : Elm.Expression -> Elm.Expression
    , all : Elm.Expression -> Elm.Expression
    , id : Elm.Expression -> Elm.Expression
    , class : Elm.Expression -> Elm.Expression
    , classes : Elm.Expression -> Elm.Expression
    , exactClassName : Elm.Expression -> Elm.Expression
    , style : Elm.Expression -> Elm.Expression -> Elm.Expression
    , checked : Elm.Expression -> Elm.Expression
    , selected : Elm.Expression -> Elm.Expression
    , disabled : Elm.Expression -> Elm.Expression
    }
call_ =
    { tag =
        \tagArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "tag"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ tagArg ]
    , text =
        \textArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "text"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ textArg ]
    , containing =
        \containingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "containing"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Test", "Html", "Selector" ]
                                        "Selector"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ containingArg ]
    , attribute =
        \attributeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "attribute"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Html" ]
                                    "Attribute"
                                    [ Type.namedWith [ "Basics" ] "Never" [] ]
                                ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ attributeArg ]
    , all =
        \allArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "all"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Test", "Html", "Selector" ]
                                        "Selector"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ allArg ]
    , id =
        \idArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "id"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ idArg ]
    , class =
        \classArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "class"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ classArg ]
    , classes =
        \classesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "classes"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list Type.string ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ classesArg ]
    , exactClassName =
        \exactClassNameArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "exactClassName"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ exactClassNameArg ]
    , style =
        \styleArg styleArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "style"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.string ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ styleArg, styleArg0 ]
    , checked =
        \checkedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "checked"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ checkedArg ]
    , selected =
        \selectedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "selected"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ selectedArg ]
    , disabled =
        \disabledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Test", "Html", "Selector" ]
                    , name = "disabled"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.bool ]
                                (Type.namedWith
                                    [ "Test", "Html", "Selector" ]
                                    "Selector"
                                    []
                                )
                            )
                    }
                )
                [ disabledArg ]
    }


values_ :
    { tag : Elm.Expression
    , text : Elm.Expression
    , containing : Elm.Expression
    , attribute : Elm.Expression
    , all : Elm.Expression
    , id : Elm.Expression
    , class : Elm.Expression
    , classes : Elm.Expression
    , exactClassName : Elm.Expression
    , style : Elm.Expression
    , checked : Elm.Expression
    , selected : Elm.Expression
    , disabled : Elm.Expression
    }
values_ =
    { tag =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "tag"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , text =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "text"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , containing =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "containing"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector" ]
                                "Selector"
                                []
                            )
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , attribute =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "attribute"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Html" ]
                            "Attribute"
                            [ Type.namedWith [ "Basics" ] "Never" [] ]
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , all =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "all"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith
                                [ "Test", "Html", "Selector" ]
                                "Selector"
                                []
                            )
                        ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , id =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "id"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , class =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "class"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , classes =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "classes"
            , annotation =
                Just
                    (Type.function
                        [ Type.list Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , exactClassName =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "exactClassName"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , style =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "style"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.string ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , checked =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "checked"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , selected =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "selected"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    , disabled =
        Elm.value
            { importFrom = [ "Test", "Html", "Selector" ]
            , name = "disabled"
            , annotation =
                Just
                    (Type.function
                        [ Type.bool ]
                        (Type.namedWith
                            [ "Test", "Html", "Selector" ]
                            "Selector"
                            []
                        )
                    )
            }
    }


