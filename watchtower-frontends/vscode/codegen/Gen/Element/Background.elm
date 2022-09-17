module Gen.Element.Background exposing (call_, color, gradient, image, moduleName_, tiled, tiledX, tiledY, uncropped, values_)

{-| 
@docs values_, call_, tiledY, tiledX, tiled, uncropped, image, gradient, color, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Element", "Background" ]


{-| color: Element.Color -> Element.Attr decorative msg -}
color : Elm.Expression -> Elm.Expression
color colorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "color"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Color" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ colorArg ]


{-| A linear gradient.

First you need to specify what direction the gradient is going by providing an angle in radians. `0` is up and `pi` is down.

The colors will be evenly spaced.

gradient: { angle : Float, steps : List Element.Color } -> Element.Attr decorative msg
-}
gradient : { angle : Float, steps : List Elm.Expression } -> Elm.Expression
gradient gradientArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "gradient"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "angle", Type.float )
                            , ( "steps"
                              , Type.list
                                    (Type.namedWith [ "Element" ] "Color" [])
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "angle" (Elm.float gradientArg.angle)
            , Tuple.pair "steps" (Elm.list gradientArg.steps)
            ]
        ]


{-| Resize the image to fit the containing element while maintaining proportions and cropping the overflow.

image: String -> Element.Attribute msg
-}
image : String -> Elm.Expression
image imageArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "image"
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
        [ Elm.string imageArg ]


{-| A centered background image that keeps its natural proportions, but scales to fit the space.

uncropped: String -> Element.Attribute msg
-}
uncropped : String -> Elm.Expression
uncropped uncroppedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "uncropped"
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
        [ Elm.string uncroppedArg ]


{-| Tile an image in the x and y axes.

tiled: String -> Element.Attribute msg
-}
tiled : String -> Elm.Expression
tiled tiledArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "tiled"
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
        [ Elm.string tiledArg ]


{-| Tile an image in the x axis.

tiledX: String -> Element.Attribute msg
-}
tiledX : String -> Elm.Expression
tiledX tiledXArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "tiledX"
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
        [ Elm.string tiledXArg ]


{-| Tile an image in the y axis.

tiledY: String -> Element.Attribute msg
-}
tiledY : String -> Elm.Expression
tiledY tiledYArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "tiledY"
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
        [ Elm.string tiledYArg ]


call_ :
    { color : Elm.Expression -> Elm.Expression
    , gradient : Elm.Expression -> Elm.Expression
    , image : Elm.Expression -> Elm.Expression
    , uncropped : Elm.Expression -> Elm.Expression
    , tiled : Elm.Expression -> Elm.Expression
    , tiledX : Elm.Expression -> Elm.Expression
    , tiledY : Elm.Expression -> Elm.Expression
    }
call_ =
    { color =
        \colorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Background" ]
                    , name = "color"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Element" ] "Color" [] ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ colorArg ]
    , gradient =
        \gradientArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Background" ]
                    , name = "gradient"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "angle", Type.float )
                                    , ( "steps"
                                      , Type.list
                                            (Type.namedWith
                                                [ "Element" ]
                                                "Color"
                                                []
                                            )
                                      )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ gradientArg ]
    , image =
        \imageArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Background" ]
                    , name = "image"
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
                [ imageArg ]
    , uncropped =
        \uncroppedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Background" ]
                    , name = "uncropped"
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
                [ uncroppedArg ]
    , tiled =
        \tiledArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Background" ]
                    , name = "tiled"
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
                [ tiledArg ]
    , tiledX =
        \tiledXArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Background" ]
                    , name = "tiledX"
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
                [ tiledXArg ]
    , tiledY =
        \tiledYArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Background" ]
                    , name = "tiledY"
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
                [ tiledYArg ]
    }


values_ :
    { color : Elm.Expression
    , gradient : Elm.Expression
    , image : Elm.Expression
    , uncropped : Elm.Expression
    , tiled : Elm.Expression
    , tiledX : Elm.Expression
    , tiledY : Elm.Expression
    }
values_ =
    { color =
        Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "color"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Color" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , gradient =
        Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "gradient"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "angle", Type.float )
                            , ( "steps"
                              , Type.list
                                    (Type.namedWith [ "Element" ] "Color" [])
                              )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , image =
        Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "image"
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
    , uncropped =
        Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "uncropped"
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
    , tiled =
        Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "tiled"
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
    , tiledX =
        Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "tiledX"
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
    , tiledY =
        Elm.value
            { importFrom = [ "Element", "Background" ]
            , name = "tiledY"
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
    }


