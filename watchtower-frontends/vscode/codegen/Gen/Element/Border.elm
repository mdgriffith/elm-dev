module Gen.Element.Border exposing (call_, color, dashed, dotted, glow, innerGlow, innerShadow, moduleName_, roundEach, rounded, shadow, solid, values_, width, widthEach, widthXY)

{-| 
@docs values_, call_, innerShadow, shadow, innerGlow, glow, roundEach, rounded, dotted, dashed, solid, widthEach, widthXY, width, color, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Element", "Border" ]


{-| color: Element.Color -> Element.Attr decorative msg -}
color : Elm.Expression -> Elm.Expression
color colorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Border" ]
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


{-| width: Int -> Element.Attribute msg -}
width : Int -> Elm.Expression
width widthArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "width"
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
        [ Elm.int widthArg ]


{-| Set horizontal and vertical borders.

widthXY: Int -> Int -> Element.Attribute msg
-}
widthXY : Int -> Int -> Elm.Expression
widthXY widthXYArg widthXYArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "widthXY"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int widthXYArg, Elm.int widthXYArg0 ]


{-| widthEach: { bottom : Int, left : Int, right : Int, top : Int } -> Element.Attribute msg -}
widthEach :
    { bottom : Int, left : Int, right : Int, top : Int } -> Elm.Expression
widthEach widthEachArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "widthEach"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "bottom", Type.int )
                            , ( "left", Type.int )
                            , ( "right", Type.int )
                            , ( "top", Type.int )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "bottom" (Elm.int widthEachArg.bottom)
            , Tuple.pair "left" (Elm.int widthEachArg.left)
            , Tuple.pair "right" (Elm.int widthEachArg.right)
            , Tuple.pair "top" (Elm.int widthEachArg.top)
            ]
        ]


{-| solid: Element.Attribute msg -}
solid : Elm.Expression
solid =
    Elm.value
        { importFrom = [ "Element", "Border" ]
        , name = "solid"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| dashed: Element.Attribute msg -}
dashed : Elm.Expression
dashed =
    Elm.value
        { importFrom = [ "Element", "Border" ]
        , name = "dashed"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| dotted: Element.Attribute msg -}
dotted : Elm.Expression
dotted =
    Elm.value
        { importFrom = [ "Element", "Border" ]
        , name = "dotted"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| Round all corners.

rounded: Int -> Element.Attribute msg
-}
rounded : Int -> Elm.Expression
rounded roundedArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "rounded"
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
        [ Elm.int roundedArg ]


{-| roundEach: 
    { topLeft : Int, topRight : Int, bottomLeft : Int, bottomRight : Int }
    -> Element.Attribute msg
-}
roundEach :
    { topLeft : Int, topRight : Int, bottomLeft : Int, bottomRight : Int }
    -> Elm.Expression
roundEach roundEachArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "roundEach"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "topLeft", Type.int )
                            , ( "topRight", Type.int )
                            , ( "bottomLeft", Type.int )
                            , ( "bottomRight", Type.int )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "topLeft" (Elm.int roundEachArg.topLeft)
            , Tuple.pair "topRight" (Elm.int roundEachArg.topRight)
            , Tuple.pair "bottomLeft" (Elm.int roundEachArg.bottomLeft)
            , Tuple.pair "bottomRight" (Elm.int roundEachArg.bottomRight)
            ]
        ]


{-| A simple glow by specifying the color and size.

glow: Element.Color -> Float -> Element.Attr decorative msg
-}
glow : Elm.Expression -> Float -> Elm.Expression
glow glowArg glowArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "glow"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Color" [], Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ glowArg, Elm.float glowArg0 ]


{-| innerGlow: Element.Color -> Float -> Element.Attr decorative msg -}
innerGlow : Elm.Expression -> Float -> Elm.Expression
innerGlow innerGlowArg innerGlowArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "innerGlow"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Color" [], Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ innerGlowArg, Elm.float innerGlowArg0 ]


{-| shadow: 
    { offset : ( Float, Float ), size : Float, blur : Float, color : Element.Color }
    -> Element.Attr decorative msg
-}
shadow :
    { offset : Elm.Expression
    , size : Float
    , blur : Float
    , color : Elm.Expression
    }
    -> Elm.Expression
shadow shadowArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "shadow"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "offset", Type.tuple Type.float Type.float )
                            , ( "size", Type.float )
                            , ( "blur", Type.float )
                            , ( "color"
                              , Type.namedWith [ "Element" ] "Color" []
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
            [ Tuple.pair "offset" shadowArg.offset
            , Tuple.pair "size" (Elm.float shadowArg.size)
            , Tuple.pair "blur" (Elm.float shadowArg.blur)
            , Tuple.pair "color" shadowArg.color
            ]
        ]


{-| innerShadow: 
    { offset : ( Float, Float ), size : Float, blur : Float, color : Element.Color }
    -> Element.Attr decorative msg
-}
innerShadow :
    { offset : Elm.Expression
    , size : Float
    , blur : Float
    , color : Elm.Expression
    }
    -> Elm.Expression
innerShadow innerShadowArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "innerShadow"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "offset", Type.tuple Type.float Type.float )
                            , ( "size", Type.float )
                            , ( "blur", Type.float )
                            , ( "color"
                              , Type.namedWith [ "Element" ] "Color" []
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
            [ Tuple.pair "offset" innerShadowArg.offset
            , Tuple.pair "size" (Elm.float innerShadowArg.size)
            , Tuple.pair "blur" (Elm.float innerShadowArg.blur)
            , Tuple.pair "color" innerShadowArg.color
            ]
        ]


call_ :
    { color : Elm.Expression -> Elm.Expression
    , width : Elm.Expression -> Elm.Expression
    , widthXY : Elm.Expression -> Elm.Expression -> Elm.Expression
    , widthEach : Elm.Expression -> Elm.Expression
    , rounded : Elm.Expression -> Elm.Expression
    , roundEach : Elm.Expression -> Elm.Expression
    , glow : Elm.Expression -> Elm.Expression -> Elm.Expression
    , innerGlow : Elm.Expression -> Elm.Expression -> Elm.Expression
    , shadow : Elm.Expression -> Elm.Expression
    , innerShadow : Elm.Expression -> Elm.Expression
    }
call_ =
    { color =
        \colorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Border" ]
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
    , width =
        \widthArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Border" ]
                    , name = "width"
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
                [ widthArg ]
    , widthXY =
        \widthXYArg widthXYArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Border" ]
                    , name = "widthXY"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int, Type.int ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ widthXYArg, widthXYArg0 ]
    , widthEach =
        \widthEachArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Border" ]
                    , name = "widthEach"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "bottom", Type.int )
                                    , ( "left", Type.int )
                                    , ( "right", Type.int )
                                    , ( "top", Type.int )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ widthEachArg ]
    , rounded =
        \roundedArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Border" ]
                    , name = "rounded"
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
                [ roundedArg ]
    , roundEach =
        \roundEachArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Border" ]
                    , name = "roundEach"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "topLeft", Type.int )
                                    , ( "topRight", Type.int )
                                    , ( "bottomLeft", Type.int )
                                    , ( "bottomRight", Type.int )
                                    ]
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ roundEachArg ]
    , glow =
        \glowArg glowArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Border" ]
                    , name = "glow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Element" ] "Color" []
                                , Type.float
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ glowArg, glowArg0 ]
    , innerGlow =
        \innerGlowArg innerGlowArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Border" ]
                    , name = "innerGlow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith [ "Element" ] "Color" []
                                , Type.float
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ innerGlowArg, innerGlowArg0 ]
    , shadow =
        \shadowArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Border" ]
                    , name = "shadow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "offset"
                                      , Type.tuple Type.float Type.float
                                      )
                                    , ( "size", Type.float )
                                    , ( "blur", Type.float )
                                    , ( "color"
                                      , Type.namedWith [ "Element" ] "Color" []
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
                [ shadowArg ]
    , innerShadow =
        \innerShadowArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Border" ]
                    , name = "innerShadow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "offset"
                                      , Type.tuple Type.float Type.float
                                      )
                                    , ( "size", Type.float )
                                    , ( "blur", Type.float )
                                    , ( "color"
                                      , Type.namedWith [ "Element" ] "Color" []
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
                [ innerShadowArg ]
    }


values_ :
    { color : Elm.Expression
    , width : Elm.Expression
    , widthXY : Elm.Expression
    , widthEach : Elm.Expression
    , solid : Elm.Expression
    , dashed : Elm.Expression
    , dotted : Elm.Expression
    , rounded : Elm.Expression
    , roundEach : Elm.Expression
    , glow : Elm.Expression
    , innerGlow : Elm.Expression
    , shadow : Elm.Expression
    , innerShadow : Elm.Expression
    }
values_ =
    { color =
        Elm.value
            { importFrom = [ "Element", "Border" ]
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
    , width =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "width"
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
    , widthXY =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "widthXY"
            , annotation =
                Just
                    (Type.function
                        [ Type.int, Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , widthEach =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "widthEach"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "bottom", Type.int )
                            , ( "left", Type.int )
                            , ( "right", Type.int )
                            , ( "top", Type.int )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , solid =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "solid"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , dashed =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "dashed"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , dotted =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "dotted"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , rounded =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "rounded"
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
    , roundEach =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "roundEach"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "topLeft", Type.int )
                            , ( "topRight", Type.int )
                            , ( "bottomLeft", Type.int )
                            , ( "bottomRight", Type.int )
                            ]
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , glow =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "glow"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Color" [], Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , innerGlow =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "innerGlow"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element" ] "Color" [], Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , shadow =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "shadow"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "offset", Type.tuple Type.float Type.float )
                            , ( "size", Type.float )
                            , ( "blur", Type.float )
                            , ( "color"
                              , Type.namedWith [ "Element" ] "Color" []
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
    , innerShadow =
        Elm.value
            { importFrom = [ "Element", "Border" ]
            , name = "innerShadow"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "offset", Type.tuple Type.float Type.float )
                            , ( "size", Type.float )
                            , ( "blur", Type.float )
                            , ( "color"
                              , Type.namedWith [ "Element" ] "Color" []
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
    }


