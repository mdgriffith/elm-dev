module Gen.Element.Region exposing (announce, announceUrgently, aside, call_, description, footer, heading, mainContent, moduleName_, navigation, values_)

{-| 
@docs values_, call_, announceUrgently, announce, description, footer, aside, heading, navigation, mainContent, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Element", "Region" ]


{-| mainContent: Element.Attribute msg -}
mainContent : Elm.Expression
mainContent =
    Elm.value
        { importFrom = [ "Element", "Region" ]
        , name = "mainContent"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| navigation: Element.Attribute msg -}
navigation : Elm.Expression
navigation =
    Elm.value
        { importFrom = [ "Element", "Region" ]
        , name = "navigation"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| This will mark an element as `h1`, `h2`, etc where possible.

Though it's also smart enough to not conflict with existing nodes.

So, this code

    link [ Region.heading 1 ]
        { url = "http://fruits.com"
        , label = text "Best site ever"
        }

will generate

    <a href="http://fruits.com">
        <h1>Best site ever</h1>
    </a>

heading: Int -> Element.Attribute msg
-}
heading : Int -> Elm.Expression
heading headingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Region" ]
            , name = "heading"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int headingArg ]


{-| aside: Element.Attribute msg -}
aside : Elm.Expression
aside =
    Elm.value
        { importFrom = [ "Element", "Region" ]
        , name = "aside"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| footer: Element.Attribute msg -}
footer : Elm.Expression
footer =
    Elm.value
        { importFrom = [ "Element", "Region" ]
        , name = "footer"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| Adds an `aria-label`, which is used by accessibility software to identity otherwise unlabeled elements.

A common use for this would be to label buttons that only have an icon.

description: String -> Element.Attribute msg
-}
description : String -> Elm.Expression
description descriptionArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Region" ]
            , name = "description"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.string descriptionArg ]


{-| Screen readers will announce when changes to this element are made.

announce: Element.Attribute msg
-}
announce : Elm.Expression
announce =
    Elm.value
        { importFrom = [ "Element", "Region" ]
        , name = "announce"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| Screen readers will announce changes to this element and potentially interrupt any other announcement.

announceUrgently: Element.Attribute msg
-}
announceUrgently : Elm.Expression
announceUrgently =
    Elm.value
        { importFrom = [ "Element", "Region" ]
        , name = "announceUrgently"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


call_ :
    { heading : Elm.Expression -> Elm.Expression
    , description : Elm.Expression -> Elm.Expression
    }
call_ =
    { heading =
        \headingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Region" ]
                    , name = "heading"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ headingArg ]
    , description =
        \descriptionArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Region" ]
                    , name = "description"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ descriptionArg ]
    }


values_ :
    { mainContent : Elm.Expression
    , navigation : Elm.Expression
    , heading : Elm.Expression
    , aside : Elm.Expression
    , footer : Elm.Expression
    , description : Elm.Expression
    , announce : Elm.Expression
    , announceUrgently : Elm.Expression
    }
values_ =
    { mainContent =
        Elm.value
            { importFrom = [ "Element", "Region" ]
            , name = "mainContent"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , navigation =
        Elm.value
            { importFrom = [ "Element", "Region" ]
            , name = "navigation"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , heading =
        Elm.value
            { importFrom = [ "Element", "Region" ]
            , name = "heading"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , aside =
        Elm.value
            { importFrom = [ "Element", "Region" ]
            , name = "aside"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , footer =
        Elm.value
            { importFrom = [ "Element", "Region" ]
            , name = "footer"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , description =
        Elm.value
            { importFrom = [ "Element", "Region" ]
            , name = "description"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , announce =
        Elm.value
            { importFrom = [ "Element", "Region" ]
            , name = "announce"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , announceUrgently =
        Elm.value
            { importFrom = [ "Element", "Region" ]
            , name = "announceUrgently"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    }


