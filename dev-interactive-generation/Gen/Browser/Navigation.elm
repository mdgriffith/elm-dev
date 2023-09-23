module Gen.Browser.Navigation exposing (annotation_, back, call_, forward, load, moduleName_, pushUrl, reload, reloadAndSkipCache, replaceUrl, values_)

{-| 
@docs values_, call_, annotation_, reloadAndSkipCache, reload, load, forward, back, replaceUrl, pushUrl, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Browser", "Navigation" ]


{-| Change the URL, but do not trigger a page load.

This will add a new entry to the browser history.

Check out the [`elm/url`][url] package for help building URLs. The
[`Url.Builder.absolute`][abs] and [`Url.Builder.relative`][rel] functions can
be particularly handy!

[url]: /packages/elm/url/latest
[abs]: /packages/elm/url/latest/Url-Builder#absolute
[rel]: /packages/elm/url/latest/Url-Builder#relative

**Note:** If the user has gone `back` a few pages, there will be &ldquo;future
pages&rdquo; that the user can go `forward` to. Adding a new URL in that
scenario will clear out any future pages. It is like going back in time and
making a different choice.

pushUrl: Browser.Navigation.Key -> String -> Platform.Cmd.Cmd msg
-}
pushUrl : Elm.Expression -> String -> Elm.Expression
pushUrl pushUrlArg pushUrlArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "pushUrl"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Browser", "Navigation" ] "Key" []
                        , Type.string
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
        )
        [ pushUrlArg, Elm.string pushUrlArg0 ]


{-| Change the URL, but do not trigger a page load.

This _will not_ add a new entry to the browser history.

This can be useful if you have search box and you want the `?search=hats` in
the URL to match without adding a history entry for every single key stroke.
Imagine how annoying it would be to click `back` thirty times and still be on
the same page!

**Note:** Browsers may rate-limit this function by throwing an exception. The
discussion [here](https://bugs.webkit.org/show_bug.cgi?id=156115) suggests
that the limit is 100 calls per 30 second interval in Safari in 2016. It also
suggests techniques for people changing the URL based on scroll position.

replaceUrl: Browser.Navigation.Key -> String -> Platform.Cmd.Cmd msg
-}
replaceUrl : Elm.Expression -> String -> Elm.Expression
replaceUrl replaceUrlArg replaceUrlArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "replaceUrl"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Browser", "Navigation" ] "Key" []
                        , Type.string
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
        )
        [ replaceUrlArg, Elm.string replaceUrlArg0 ]


{-| Go back some number of pages. So `back 1` goes back one page, and `back 2`
goes back two pages.

**Note:** You only manage the browser history that _you_ created. Think of this
library as letting you have access to a small part of the overall history. So
if you go back farther than the history you own, you will just go back to some
other website!

back: Browser.Navigation.Key -> Int -> Platform.Cmd.Cmd msg
-}
back : Elm.Expression -> Int -> Elm.Expression
back backArg backArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "back"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Browser", "Navigation" ] "Key" []
                        , Type.int
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
        )
        [ backArg, Elm.int backArg0 ]


{-| Go forward some number of pages. So `forward 1` goes forward one page, and
`forward 2` goes forward two pages. If there are no more pages in the future,
this will do nothing.

**Note:** You only manage the browser history that _you_ created. Think of this
library as letting you have access to a small part of the overall history. So
if you go forward farther than the history you own, the user will end up on
whatever website they visited next!

forward: Browser.Navigation.Key -> Int -> Platform.Cmd.Cmd msg
-}
forward : Elm.Expression -> Int -> Elm.Expression
forward forwardArg forwardArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "forward"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Browser", "Navigation" ] "Key" []
                        , Type.int
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
        )
        [ forwardArg, Elm.int forwardArg0 ]


{-| Leave the current page and load the given URL. **This always results in a
page load**, even if the provided URL is the same as the current one.

    gotoElmWebsite : Cmd msg
    gotoElmWebsite =
        load "https://elm-lang.org"

Check out the [`elm/url`][url] package for help building URLs. The
[`Url.absolute`][abs] and [`Url.relative`][rel] functions can be particularly
handy!

[url]: /packages/elm/url/latest
[abs]: /packages/elm/url/latest/Url#absolute
[rel]: /packages/elm/url/latest/Url#relative

load: String -> Platform.Cmd.Cmd msg
-}
load : String -> Elm.Expression
load loadArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "load"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
        )
        [ Elm.string loadArg ]


{-| Reload the current page. **This always results in a page load!**
This may grab resources from the browser cache, so use
[`reloadAndSkipCache`](#reloadAndSkipCache)
if you want to be sure that you are not loading any cached resources.

reload: Platform.Cmd.Cmd msg
-}
reload : Elm.Expression
reload =
    Elm.value
        { importFrom = [ "Browser", "Navigation" ]
        , name = "reload"
        , annotation = Just (Type.namedWith [] "Cmd" [ Type.var "msg" ])
        }


{-| Reload the current page without using the browser cache. **This always
results in a page load!** It is more common to want [`reload`](#reload).

reloadAndSkipCache: Platform.Cmd.Cmd msg
-}
reloadAndSkipCache : Elm.Expression
reloadAndSkipCache =
    Elm.value
        { importFrom = [ "Browser", "Navigation" ]
        , name = "reloadAndSkipCache"
        , annotation = Just (Type.namedWith [] "Cmd" [ Type.var "msg" ])
        }


annotation_ : { key : Type.Annotation }
annotation_ =
    { key = Type.namedWith [ "Browser", "Navigation" ] "Key" [] }


call_ :
    { pushUrl : Elm.Expression -> Elm.Expression -> Elm.Expression
    , replaceUrl : Elm.Expression -> Elm.Expression -> Elm.Expression
    , back : Elm.Expression -> Elm.Expression -> Elm.Expression
    , forward : Elm.Expression -> Elm.Expression -> Elm.Expression
    , load : Elm.Expression -> Elm.Expression
    }
call_ =
    { pushUrl =
        \pushUrlArg pushUrlArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Navigation" ]
                    , name = "pushUrl"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Browser", "Navigation" ]
                                    "Key"
                                    []
                                , Type.string
                                ]
                                (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                            )
                    }
                )
                [ pushUrlArg, pushUrlArg0 ]
    , replaceUrl =
        \replaceUrlArg replaceUrlArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Navigation" ]
                    , name = "replaceUrl"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Browser", "Navigation" ]
                                    "Key"
                                    []
                                , Type.string
                                ]
                                (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                            )
                    }
                )
                [ replaceUrlArg, replaceUrlArg0 ]
    , back =
        \backArg backArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Navigation" ]
                    , name = "back"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Browser", "Navigation" ]
                                    "Key"
                                    []
                                , Type.int
                                ]
                                (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                            )
                    }
                )
                [ backArg, backArg0 ]
    , forward =
        \forwardArg forwardArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Navigation" ]
                    , name = "forward"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Browser", "Navigation" ]
                                    "Key"
                                    []
                                , Type.int
                                ]
                                (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                            )
                    }
                )
                [ forwardArg, forwardArg0 ]
    , load =
        \loadArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Browser", "Navigation" ]
                    , name = "load"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                            )
                    }
                )
                [ loadArg ]
    }


values_ :
    { pushUrl : Elm.Expression
    , replaceUrl : Elm.Expression
    , back : Elm.Expression
    , forward : Elm.Expression
    , load : Elm.Expression
    , reload : Elm.Expression
    , reloadAndSkipCache : Elm.Expression
    }
values_ =
    { pushUrl =
        Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "pushUrl"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Browser", "Navigation" ] "Key" []
                        , Type.string
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
    , replaceUrl =
        Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "replaceUrl"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Browser", "Navigation" ] "Key" []
                        , Type.string
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
    , back =
        Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "back"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Browser", "Navigation" ] "Key" []
                        , Type.int
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
    , forward =
        Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "forward"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Browser", "Navigation" ] "Key" []
                        , Type.int
                        ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
    , load =
        Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "load"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [] "Cmd" [ Type.var "msg" ])
                    )
            }
    , reload =
        Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "reload"
            , annotation = Just (Type.namedWith [] "Cmd" [ Type.var "msg" ])
            }
    , reloadAndSkipCache =
        Elm.value
            { importFrom = [ "Browser", "Navigation" ]
            , name = "reloadAndSkipCache"
            , annotation = Just (Type.namedWith [] "Cmd" [ Type.var "msg" ])
            }
    }


