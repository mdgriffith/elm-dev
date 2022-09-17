module Gen.Browser exposing (annotation_, application, call_, caseOf_, document, element, make_, moduleName_, sandbox, values_)

{-| 
@docs values_, call_, caseOf_, make_, annotation_, application, document, element, sandbox, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Browser" ]


{-| Create a “sandboxed” program that cannot communicate with the outside
world.

This is great for learning the basics of [The Elm Architecture][tea]. You can
see sandboxes in action in the following examples:

  - [Buttons](https://guide.elm-lang.org/architecture/buttons.html)
  - [Text Fields](https://guide.elm-lang.org/architecture/text_fields.html)
  - [Forms](https://guide.elm-lang.org/architecture/forms.html)

Those are nice, but **I very highly recommend reading [this guide][guide]
straight through** to really learn how Elm works. Understanding the
fundamentals actually pays off in this language!

[tea]: https://guide.elm-lang.org/architecture/
[guide]: https://guide.elm-lang.org/

sandbox: 
    { init : model, view : model -> Html.Html msg, update : msg -> model -> model }
    -> Platform.Program () model msg
-}
sandbox :
    { init : Elm.Expression
    , view : Elm.Expression -> Elm.Expression
    , update : Elm.Expression -> Elm.Expression -> Elm.Expression
    }
    -> Elm.Expression
sandbox sandboxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser" ]
            , name = "sandbox"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "init", Type.var "model" )
                            , ( "view"
                              , Type.function
                                    [ Type.var "model" ]
                                    (Type.namedWith
                                        [ "Html" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "update"
                              , Type.function
                                    [ Type.var "msg", Type.var "model" ]
                                    (Type.var "model")
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Platform" ]
                            "Program"
                            [ Type.unit, Type.var "model", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "init" sandboxArg.init
            , Tuple.pair
                "view"
                (Elm.functionReduced "sandboxUnpack" sandboxArg.view)
            , Tuple.pair
                "update"
                (Elm.functionReduced
                    "sandboxUnpack"
                    (\functionReducedUnpack ->
                        Elm.functionReduced
                            "unpack"
                            (sandboxArg.update functionReducedUnpack)
                    )
                )
            ]
        ]


{-| Create an HTML element managed by Elm. The resulting elements are easy to
embed in larger JavaScript projects, and lots of companies that use Elm
started with this approach! Try it out on something small. If it works, great,
do more! If not, revert, no big deal.

Unlike a [`sandbox`](#sandbox), an `element` can talk to the outside world in
a couple ways:

  - `Cmd` &mdash; you can “command” the Elm runtime to do stuff, like HTTP.
  - `Sub` &mdash; you can “subscribe” to event sources, like clock ticks.
  - `flags` &mdash; JavaScript can pass in data when starting the Elm program
  - `ports` &mdash; set up a client-server relationship with JavaScript

As you read [the guide][guide] you will run into a bunch of examples of `element`
in [this section][fx]. You can learn more about flags and ports in [the interop
section][interop].

[guide]: https://guide.elm-lang.org/
[fx]: https://guide.elm-lang.org/effects/
[interop]: https://guide.elm-lang.org/interop/

element: 
    { init : flags -> ( model, Platform.Cmd.Cmd msg )
    , view : model -> Html.Html msg
    , update : msg -> model -> ( model, Platform.Cmd.Cmd msg )
    , subscriptions : model -> Platform.Sub.Sub msg
    }
    -> Platform.Program flags model msg
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
            { importFrom = [ "Browser" ]
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
                                        [ "Html" ]
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
                                    (Type.namedWith [] "Sub" [ Type.var "msg" ])
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Platform" ]
                            "Program"
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


{-| Create an HTML document managed by Elm. This expands upon what `element`
can do in that `view` now gives you control over the `<title>` and `<body>`.

document: 
    { init : flags -> ( model, Platform.Cmd.Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Platform.Cmd.Cmd msg )
    , subscriptions : model -> Platform.Sub.Sub msg
    }
    -> Platform.Program flags model msg
-}
document :
    { init : Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression
    , update : Elm.Expression -> Elm.Expression -> Elm.Expression
    , subscriptions : Elm.Expression -> Elm.Expression
    }
    -> Elm.Expression
document documentArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser" ]
            , name = "document"
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
                                        [ "Browser" ]
                                        "Document"
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
                                    (Type.namedWith [] "Sub" [ Type.var "msg" ])
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Platform" ]
                            "Program"
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
                (Elm.functionReduced "documentUnpack" documentArg.init)
            , Tuple.pair
                "view"
                (Elm.functionReduced "documentUnpack" documentArg.view)
            , Tuple.pair
                "update"
                (Elm.functionReduced
                    "documentUnpack"
                    (\functionReducedUnpack ->
                        Elm.functionReduced
                            "unpack"
                            (documentArg.update functionReducedUnpack)
                    )
                )
            , Tuple.pair
                "subscriptions"
                (Elm.functionReduced "documentUnpack" documentArg.subscriptions)
            ]
        ]


{-| Create an application that manages [`Url`][url] changes.

**When the application starts**, `init` gets the initial `Url`. You can show
different things depending on the `Url`!

**When someone clicks a link**, like `<a href="/home">Home</a>`, it always goes
through `onUrlRequest`. The resulting message goes to your `update` function,
giving you a chance to save scroll position or persist data before changing
the URL yourself with [`pushUrl`][bnp] or [`load`][bnl]. More info on this in
the [`UrlRequest`](#UrlRequest) docs!

**When the URL changes**, the new `Url` goes through `onUrlChange`. The
resulting message goes to `update` where you can decide what to show next.

Applications always use the [`Browser.Navigation`][bn] module for precise
control over `Url` changes.

**More Info:** Here are some example usages of `application` programs:

  - [RealWorld example app](https://github.com/rtfeldman/elm-spa-example)
  - [Elm’s package website](https://github.com/elm/package.elm-lang.org)

These are quite advanced Elm programs, so be sure to go through [the guide][g]
first to get a solid conceptual foundation before diving in! If you start
reading a calculus book from page 314, it might seem confusing. Same here!

**Note:** Can an [`element`](#element) manage the URL too? Read [this]!

[g]: https://guide.elm-lang.org/
[bn]: Browser-Navigation
[bnp]: Browser-Navigation#pushUrl
[bnl]: Browser-Navigation#load
[url]: /packages/elm/url/latest/Url#Url
[this]: https://github.com/elm/browser/blob/1.0.2/notes/navigation-in-elements.md

application: 
    { init :
        flags
        -> Url.Url
        -> Browser.Navigation.Key
        -> ( model, Platform.Cmd.Cmd msg )
    , view : model -> Browser.Document msg
    , update : msg -> model -> ( model, Platform.Cmd.Cmd msg )
    , subscriptions : model -> Platform.Sub.Sub msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url.Url -> msg
    }
    -> Platform.Program flags model msg
-}
application :
    { init :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , view : Elm.Expression -> Elm.Expression
    , update : Elm.Expression -> Elm.Expression -> Elm.Expression
    , subscriptions : Elm.Expression -> Elm.Expression
    , onUrlRequest : Elm.Expression -> Elm.Expression
    , onUrlChange : Elm.Expression -> Elm.Expression
    }
    -> Elm.Expression
application applicationArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser" ]
            , name = "application"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "init"
                              , Type.function
                                    [ Type.var "flags"
                                    , Type.namedWith [ "Url" ] "Url" []
                                    , Type.namedWith
                                        [ "Browser", "Navigation" ]
                                        "Key"
                                        []
                                    ]
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
                                        [ "Browser" ]
                                        "Document"
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
                                    (Type.namedWith [] "Sub" [ Type.var "msg" ])
                              )
                            , ( "onUrlRequest"
                              , Type.function
                                    [ Type.namedWith
                                        [ "Browser" ]
                                        "UrlRequest"
                                        []
                                    ]
                                    (Type.var "msg")
                              )
                            , ( "onUrlChange"
                              , Type.function
                                    [ Type.namedWith [ "Url" ] "Url" [] ]
                                    (Type.var "msg")
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Platform" ]
                            "Program"
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
                (Elm.functionReduced
                    "applicationUnpack"
                    (\functionReducedUnpack ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack0 ->
                                Elm.functionReduced
                                    "unpack"
                                    (applicationArg.init functionReducedUnpack
                                        functionReducedUnpack0
                                    )
                            )
                    )
                )
            , Tuple.pair
                "view"
                (Elm.functionReduced "applicationUnpack" applicationArg.view)
            , Tuple.pair
                "update"
                (Elm.functionReduced
                    "applicationUnpack"
                    (\functionReducedUnpack ->
                        Elm.functionReduced
                            "unpack"
                            (applicationArg.update functionReducedUnpack)
                    )
                )
            , Tuple.pair
                "subscriptions"
                (Elm.functionReduced
                    "applicationUnpack"
                    applicationArg.subscriptions
                )
            , Tuple.pair
                "onUrlRequest"
                (Elm.functionReduced
                    "applicationUnpack"
                    applicationArg.onUrlRequest
                )
            , Tuple.pair
                "onUrlChange"
                (Elm.functionReduced
                    "applicationUnpack"
                    applicationArg.onUrlChange
                )
            ]
        ]


annotation_ :
    { document : Type.Annotation -> Type.Annotation
    , urlRequest : Type.Annotation
    }
annotation_ =
    { document =
        \documentArg0 ->
            Type.alias
                moduleName_
                "Document"
                [ documentArg0 ]
                (Type.record
                    [ ( "title", Type.string )
                    , ( "body"
                      , Type.list
                            (Type.namedWith [ "Html" ] "Html" [ Type.var "msg" ]
                            )
                      )
                    ]
                )
    , urlRequest = Type.namedWith [ "Browser" ] "UrlRequest" []
    }


make_ :
    { document :
        { title : Elm.Expression, body : Elm.Expression } -> Elm.Expression
    , internal : Elm.Expression -> Elm.Expression
    , external : Elm.Expression -> Elm.Expression
    }
make_ =
    { document =
        \document_args ->
            Elm.withType
                (Type.alias
                    [ "Browser" ]
                    "Document"
                    [ Type.var "msg" ]
                    (Type.record
                        [ ( "title", Type.string )
                        , ( "body"
                          , Type.list
                                (Type.namedWith
                                    [ "Html" ]
                                    "Html"
                                    [ Type.var "msg" ]
                                )
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "title" document_args.title
                    , Tuple.pair "body" document_args.body
                    ]
                )
    , internal =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser" ]
                    , name = "Internal"
                    , annotation = Just (Type.namedWith [] "UrlRequest" [])
                    }
                )
                [ ar0 ]
    , external =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser" ]
                    , name = "External"
                    , annotation = Just (Type.namedWith [] "UrlRequest" [])
                    }
                )
                [ ar0 ]
    }


caseOf_ :
    { urlRequest :
        Elm.Expression
        -> { urlRequestTags_0_0
            | internal : Elm.Expression -> Elm.Expression
            , external : Elm.Expression -> Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { urlRequest =
        \urlRequestExpression urlRequestTags ->
            Elm.Case.custom
                urlRequestExpression
                (Type.namedWith [ "Browser" ] "UrlRequest" [])
                [ Elm.Case.branch1
                    "Internal"
                    ( "url.Url", Type.namedWith [ "Url" ] "Url" [] )
                    urlRequestTags.internal
                , Elm.Case.branch1
                    "External"
                    ( "string.String", Type.string )
                    urlRequestTags.external
                ]
    }


call_ :
    { sandbox : Elm.Expression -> Elm.Expression
    , element : Elm.Expression -> Elm.Expression
    , document : Elm.Expression -> Elm.Expression
    , application : Elm.Expression -> Elm.Expression
    }
call_ =
    { sandbox =
        \sandboxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser" ]
                    , name = "sandbox"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "init", Type.var "model" )
                                    , ( "view"
                                      , Type.function
                                            [ Type.var "model" ]
                                            (Type.namedWith
                                                [ "Html" ]
                                                "Html"
                                                [ Type.var "msg" ]
                                            )
                                      )
                                    , ( "update"
                                      , Type.function
                                            [ Type.var "msg", Type.var "model" ]
                                            (Type.var "model")
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Platform" ]
                                    "Program"
                                    [ Type.unit
                                    , Type.var "model"
                                    , Type.var "msg"
                                    ]
                                )
                            )
                    }
                )
                [ sandboxArg ]
    , element =
        \elementArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser" ]
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
                                                [ "Html" ]
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
                                    [ "Platform" ]
                                    "Program"
                                    [ Type.var "flags"
                                    , Type.var "model"
                                    , Type.var "msg"
                                    ]
                                )
                            )
                    }
                )
                [ elementArg ]
    , document =
        \documentArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser" ]
                    , name = "document"
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
                                                [ "Browser" ]
                                                "Document"
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
                                    [ "Platform" ]
                                    "Program"
                                    [ Type.var "flags"
                                    , Type.var "model"
                                    , Type.var "msg"
                                    ]
                                )
                            )
                    }
                )
                [ documentArg ]
    , application =
        \applicationArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser" ]
                    , name = "application"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "init"
                                      , Type.function
                                            [ Type.var "flags"
                                            , Type.namedWith [ "Url" ] "Url" []
                                            , Type.namedWith
                                                [ "Browser", "Navigation" ]
                                                "Key"
                                                []
                                            ]
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
                                                [ "Browser" ]
                                                "Document"
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
                                    , ( "onUrlRequest"
                                      , Type.function
                                            [ Type.namedWith
                                                [ "Browser" ]
                                                "UrlRequest"
                                                []
                                            ]
                                            (Type.var "msg")
                                      )
                                    , ( "onUrlChange"
                                      , Type.function
                                            [ Type.namedWith [ "Url" ] "Url" []
                                            ]
                                            (Type.var "msg")
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Platform" ]
                                    "Program"
                                    [ Type.var "flags"
                                    , Type.var "model"
                                    , Type.var "msg"
                                    ]
                                )
                            )
                    }
                )
                [ applicationArg ]
    }


values_ :
    { sandbox : Elm.Expression
    , element : Elm.Expression
    , document : Elm.Expression
    , application : Elm.Expression
    }
values_ =
    { sandbox =
        Elm.value
            { importFrom = [ "Browser" ]
            , name = "sandbox"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "init", Type.var "model" )
                            , ( "view"
                              , Type.function
                                    [ Type.var "model" ]
                                    (Type.namedWith
                                        [ "Html" ]
                                        "Html"
                                        [ Type.var "msg" ]
                                    )
                              )
                            , ( "update"
                              , Type.function
                                    [ Type.var "msg", Type.var "model" ]
                                    (Type.var "model")
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Platform" ]
                            "Program"
                            [ Type.unit, Type.var "model", Type.var "msg" ]
                        )
                    )
            }
    , element =
        Elm.value
            { importFrom = [ "Browser" ]
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
                                        [ "Html" ]
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
                                    (Type.namedWith [] "Sub" [ Type.var "msg" ])
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Platform" ]
                            "Program"
                            [ Type.var "flags"
                            , Type.var "model"
                            , Type.var "msg"
                            ]
                        )
                    )
            }
    , document =
        Elm.value
            { importFrom = [ "Browser" ]
            , name = "document"
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
                                        [ "Browser" ]
                                        "Document"
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
                                    (Type.namedWith [] "Sub" [ Type.var "msg" ])
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Platform" ]
                            "Program"
                            [ Type.var "flags"
                            , Type.var "model"
                            , Type.var "msg"
                            ]
                        )
                    )
            }
    , application =
        Elm.value
            { importFrom = [ "Browser" ]
            , name = "application"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "init"
                              , Type.function
                                    [ Type.var "flags"
                                    , Type.namedWith [ "Url" ] "Url" []
                                    , Type.namedWith
                                        [ "Browser", "Navigation" ]
                                        "Key"
                                        []
                                    ]
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
                                        [ "Browser" ]
                                        "Document"
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
                                    (Type.namedWith [] "Sub" [ Type.var "msg" ])
                              )
                            , ( "onUrlRequest"
                              , Type.function
                                    [ Type.namedWith
                                        [ "Browser" ]
                                        "UrlRequest"
                                        []
                                    ]
                                    (Type.var "msg")
                              )
                            , ( "onUrlChange"
                              , Type.function
                                    [ Type.namedWith [ "Url" ] "Url" [] ]
                                    (Type.var "msg")
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Platform" ]
                            "Program"
                            [ Type.var "flags"
                            , Type.var "model"
                            , Type.var "msg"
                            ]
                        )
                    )
            }
    }


