module Gen.Element.Font exposing (alignLeft, alignRight, annotation_, bold, call_, center, color, diagonalFractions, external, extraBold, extraLight, family, feature, glow, hairline, heavy, indexed, italic, justify, letterSpacing, ligatures, light, medium, moduleName_, monospace, ordinal, regular, sansSerif, semiBold, serif, shadow, size, slashedZero, smallCaps, stackedFractions, strike, swash, tabularNumbers, typeface, underline, unitalicized, values_, variant, variantList, wordSpacing)

{-| 
@docs values_, call_, annotation_, shadow, glow, indexed, feature, swash, diagonalFractions, stackedFractions, tabularNumbers, ordinal, ligatures, slashedZero, smallCaps, variantList, variant, hairline, extraLight, light, regular, medium, semiBold, bold, extraBold, heavy, unitalicized, italic, strike, underline, wordSpacing, letterSpacing, justify, center, alignRight, alignLeft, external, monospace, sansSerif, serif, typeface, family, size, color, moduleName_
-}


import Elm
import Elm.Annotation as Type
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Element", "Font" ]


{-| color: Element.Color -> Element.Attr decorative msg -}
color : Elm.Expression -> Elm.Expression
color colorArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
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


{-| Font sizes are always given as `px`.

size: Int -> Element.Attr decorative msg
-}
size : Int -> Elm.Expression
size sizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "size"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.int sizeArg ]


{-| import Element
    import Element.Font as Font

    myElement =
        Element.el
            [ Font.family
                [ Font.typeface "Helvetica"
                , Font.sansSerif
                ]
            ]
            (text "")

family: List Element.Font.Font -> Element.Attribute msg
-}
family : List Elm.Expression -> Elm.Expression
family familyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "family"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Element", "Font" ] "Font" [])
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list familyArg ]


{-| typeface: String -> Element.Font.Font -}
typeface : String -> Elm.Expression
typeface typefaceArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "typeface"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Element", "Font" ] "Font" [])
                    )
            }
        )
        [ Elm.string typefaceArg ]


{-| serif: Element.Font.Font -}
serif : Elm.Expression
serif =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "serif"
        , annotation = Just (Type.namedWith [ "Element", "Font" ] "Font" [])
        }


{-| sansSerif: Element.Font.Font -}
sansSerif : Elm.Expression
sansSerif =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "sansSerif"
        , annotation = Just (Type.namedWith [ "Element", "Font" ] "Font" [])
        }


{-| monospace: Element.Font.Font -}
monospace : Elm.Expression
monospace =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "monospace"
        , annotation = Just (Type.namedWith [ "Element", "Font" ] "Font" [])
        }


{-| **Note** it's likely that `Font.external` will cause a flash on your page on loading.

To bypass this, import your fonts using a separate stylesheet and just use `Font.typeface`.

It's likely that `Font.external` will be removed or redesigned in the future to avoid the flashing.

`Font.external` can be used to import font files. Let's say you found a neat font on <http://fonts.google.com>:

    import Element
    import Element.Font as Font

    view =
        Element.el
            [ Font.family
                [ Font.external
                    { name = "Roboto"
                    , url = "https://fonts.googleapis.com/css?family=Roboto"
                    }
                , Font.sansSerif
                ]
            ]
            (Element.text "Woohoo, I'm stylish text")

external: { url : String, name : String } -> Element.Font.Font
-}
external : { url : String, name : String } -> Elm.Expression
external externalArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "external"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "url", Type.string ), ( "name", Type.string ) ]
                        ]
                        (Type.namedWith [ "Element", "Font" ] "Font" [])
                    )
            }
        )
        [ Elm.record
            [ Tuple.pair "url" (Elm.string externalArg.url)
            , Tuple.pair "name" (Elm.string externalArg.name)
            ]
        ]


{-| Align the font to the left.

alignLeft: Element.Attribute msg
-}
alignLeft : Elm.Expression
alignLeft =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "alignLeft"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| Align the font to the right.

alignRight: Element.Attribute msg
-}
alignRight : Elm.Expression
alignRight =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "alignRight"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| Center align the font.

center: Element.Attribute msg
-}
center : Elm.Expression
center =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "center"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| justify: Element.Attribute msg -}
justify : Elm.Expression
justify =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "justify"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| In `px`.

letterSpacing: Float -> Element.Attribute msg
-}
letterSpacing : Float -> Elm.Expression
letterSpacing letterSpacingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "letterSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float letterSpacingArg ]


{-| In `px`.

wordSpacing: Float -> Element.Attribute msg
-}
wordSpacing : Float -> Elm.Expression
wordSpacing wordSpacingArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "wordSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.float wordSpacingArg ]


{-| underline: Element.Attribute msg -}
underline : Elm.Expression
underline =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "underline"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| strike: Element.Attribute msg -}
strike : Elm.Expression
strike =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "strike"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| italic: Element.Attribute msg -}
italic : Elm.Expression
italic =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "italic"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| This will reset bold and italic.

unitalicized: Element.Attribute msg
-}
unitalicized : Elm.Expression
unitalicized =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "unitalicized"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| heavy: Element.Attribute msg -}
heavy : Elm.Expression
heavy =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "heavy"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| extraBold: Element.Attribute msg -}
extraBold : Elm.Expression
extraBold =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "extraBold"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| bold: Element.Attribute msg -}
bold : Elm.Expression
bold =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "bold"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| semiBold: Element.Attribute msg -}
semiBold : Elm.Expression
semiBold =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "semiBold"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| medium: Element.Attribute msg -}
medium : Elm.Expression
medium =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "medium"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| regular: Element.Attribute msg -}
regular : Elm.Expression
regular =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "regular"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| light: Element.Attribute msg -}
light : Elm.Expression
light =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "light"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| extraLight: Element.Attribute msg -}
extraLight : Elm.Expression
extraLight =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "extraLight"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| hairline: Element.Attribute msg -}
hairline : Elm.Expression
hairline =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "hairline"
        , annotation =
            Just (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ])
        }


{-| You can use this to set a single variant on an element itself such as:

    el
        [ Font.variant Font.smallCaps
        ]
        (text "rendered with smallCaps")

**Note** These will **not** stack. If you want multiple variants, you should use `Font.variantList`.

variant: Element.Font.Variant -> Element.Attribute msg
-}
variant : Elm.Expression -> Elm.Expression
variant variantArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "variant"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element", "Font" ] "Variant" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ variantArg ]


{-| variantList: List Element.Font.Variant -> Element.Attribute msg -}
variantList : List Elm.Expression -> Elm.Expression
variantList variantListArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "variantList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Element", "Font" ] "Variant" [])
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
        )
        [ Elm.list variantListArg ]


{-| [Small caps](https://en.wikipedia.org/wiki/Small_caps) are rendered using uppercase glyphs, but at the size of lowercase glyphs.

smallCaps: Element.Font.Variant
-}
smallCaps : Elm.Expression
smallCaps =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "smallCaps"
        , annotation = Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
        }


{-| Add a slash when rendering `0`

slashedZero: Element.Font.Variant
-}
slashedZero : Elm.Expression
slashedZero =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "slashedZero"
        , annotation = Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
        }


{-| ligatures: Element.Font.Variant -}
ligatures : Elm.Expression
ligatures =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "ligatures"
        , annotation = Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
        }


{-| Oridinal markers like `1st` and `2nd` will receive special glyphs.

ordinal: Element.Font.Variant
-}
ordinal : Elm.Expression
ordinal =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "ordinal"
        , annotation = Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
        }


{-| Number figures will each take up the same space, allowing them to be easily aligned, such as in tables.

tabularNumbers: Element.Font.Variant
-}
tabularNumbers : Elm.Expression
tabularNumbers =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "tabularNumbers"
        , annotation = Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
        }


{-| Render fractions with the numerator stacked on top of the denominator.

stackedFractions: Element.Font.Variant
-}
stackedFractions : Elm.Expression
stackedFractions =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "stackedFractions"
        , annotation = Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
        }


{-| Render fractions

diagonalFractions: Element.Font.Variant
-}
diagonalFractions : Elm.Expression
diagonalFractions =
    Elm.value
        { importFrom = [ "Element", "Font" ]
        , name = "diagonalFractions"
        , annotation = Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
        }


{-| swash: Int -> Element.Font.Variant -}
swash : Int -> Elm.Expression
swash swashArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "swash"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Element", "Font" ] "Variant" [])
                    )
            }
        )
        [ Elm.int swashArg ]


{-| Set a feature by name and whether it should be on or off.

Feature names are four-letter names as defined in the [OpenType specification](https://docs.microsoft.com/en-us/typography/opentype/spec/featurelist).

feature: String -> Bool -> Element.Font.Variant
-}
feature : String -> Bool -> Elm.Expression
feature featureArg featureArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "feature"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.bool ]
                        (Type.namedWith [ "Element", "Font" ] "Variant" [])
                    )
            }
        )
        [ Elm.string featureArg, Elm.bool featureArg0 ]


{-| A font variant might have multiple versions within the font.

In these cases we need to specify the index of the version we want.

indexed: String -> Int -> Element.Font.Variant
-}
indexed : String -> Int -> Elm.Expression
indexed indexedArg indexedArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "indexed"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.int ]
                        (Type.namedWith [ "Element", "Font" ] "Variant" [])
                    )
            }
        )
        [ Elm.string indexedArg, Elm.int indexedArg0 ]


{-| A glow is just a simplified shadow.

glow: Element.Color -> Float -> Element.Attr decorative msg
-}
glow : Elm.Expression -> Float -> Elm.Expression
glow glowArg glowArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
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


{-| shadow: 
    { offset : ( Float, Float ), blur : Float, color : Element.Color }
    -> Element.Attr decorative msg
-}
shadow :
    { offset : Elm.Expression, blur : Float, color : Elm.Expression }
    -> Elm.Expression
shadow shadowArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "shadow"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "offset", Type.tuple Type.float Type.float )
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
            , Tuple.pair "blur" (Elm.float shadowArg.blur)
            , Tuple.pair "color" shadowArg.color
            ]
        ]


annotation_ : { font : Type.Annotation, variant : Type.Annotation }
annotation_ =
    { font =
        Type.alias
            moduleName_
            "Font"
            []
            (Type.namedWith [ "Internal", "Model" ] "Font" [])
    , variant =
        Type.alias
            moduleName_
            "Variant"
            []
            (Type.namedWith [ "Internal", "Model" ] "Variant" [])
    }


call_ :
    { color : Elm.Expression -> Elm.Expression
    , size : Elm.Expression -> Elm.Expression
    , family : Elm.Expression -> Elm.Expression
    , typeface : Elm.Expression -> Elm.Expression
    , external : Elm.Expression -> Elm.Expression
    , letterSpacing : Elm.Expression -> Elm.Expression
    , wordSpacing : Elm.Expression -> Elm.Expression
    , variant : Elm.Expression -> Elm.Expression
    , variantList : Elm.Expression -> Elm.Expression
    , swash : Elm.Expression -> Elm.Expression
    , feature : Elm.Expression -> Elm.Expression -> Elm.Expression
    , indexed : Elm.Expression -> Elm.Expression -> Elm.Expression
    , glow : Elm.Expression -> Elm.Expression -> Elm.Expression
    , shadow : Elm.Expression -> Elm.Expression
    }
call_ =
    { color =
        \colorArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
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
    , size =
        \sizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "size"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attr"
                                    [ Type.var "decorative", Type.var "msg" ]
                                )
                            )
                    }
                )
                [ sizeArg ]
    , family =
        \familyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "family"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element", "Font" ]
                                        "Font"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ familyArg ]
    , typeface =
        \typefaceArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "typeface"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string ]
                                (Type.namedWith [ "Element", "Font" ] "Font" [])
                            )
                    }
                )
                [ typefaceArg ]
    , external =
        \externalArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "external"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "url", Type.string )
                                    , ( "name", Type.string )
                                    ]
                                ]
                                (Type.namedWith [ "Element", "Font" ] "Font" [])
                            )
                    }
                )
                [ externalArg ]
    , letterSpacing =
        \letterSpacingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "letterSpacing"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ letterSpacingArg ]
    , wordSpacing =
        \wordSpacingArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "wordSpacing"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.float ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ wordSpacingArg ]
    , variant =
        \variantArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "variant"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Element", "Font" ]
                                    "Variant"
                                    []
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ variantArg ]
    , variantList =
        \variantListArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "variantList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.namedWith
                                        [ "Element", "Font" ]
                                        "Variant"
                                        []
                                    )
                                ]
                                (Type.namedWith
                                    [ "Element" ]
                                    "Attribute"
                                    [ Type.var "msg" ]
                                )
                            )
                    }
                )
                [ variantListArg ]
    , swash =
        \swashArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "swash"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.int ]
                                (Type.namedWith
                                    [ "Element", "Font" ]
                                    "Variant"
                                    []
                                )
                            )
                    }
                )
                [ swashArg ]
    , feature =
        \featureArg featureArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "feature"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.bool ]
                                (Type.namedWith
                                    [ "Element", "Font" ]
                                    "Variant"
                                    []
                                )
                            )
                    }
                )
                [ featureArg, featureArg0 ]
    , indexed =
        \indexedArg indexedArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "indexed"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.string, Type.int ]
                                (Type.namedWith
                                    [ "Element", "Font" ]
                                    "Variant"
                                    []
                                )
                            )
                    }
                )
                [ indexedArg, indexedArg0 ]
    , glow =
        \glowArg glowArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
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
    , shadow =
        \shadowArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Element", "Font" ]
                    , name = "shadow"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.record
                                    [ ( "offset"
                                      , Type.tuple Type.float Type.float
                                      )
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
    }


values_ :
    { color : Elm.Expression
    , size : Elm.Expression
    , family : Elm.Expression
    , typeface : Elm.Expression
    , serif : Elm.Expression
    , sansSerif : Elm.Expression
    , monospace : Elm.Expression
    , external : Elm.Expression
    , alignLeft : Elm.Expression
    , alignRight : Elm.Expression
    , center : Elm.Expression
    , justify : Elm.Expression
    , letterSpacing : Elm.Expression
    , wordSpacing : Elm.Expression
    , underline : Elm.Expression
    , strike : Elm.Expression
    , italic : Elm.Expression
    , unitalicized : Elm.Expression
    , heavy : Elm.Expression
    , extraBold : Elm.Expression
    , bold : Elm.Expression
    , semiBold : Elm.Expression
    , medium : Elm.Expression
    , regular : Elm.Expression
    , light : Elm.Expression
    , extraLight : Elm.Expression
    , hairline : Elm.Expression
    , variant : Elm.Expression
    , variantList : Elm.Expression
    , smallCaps : Elm.Expression
    , slashedZero : Elm.Expression
    , ligatures : Elm.Expression
    , ordinal : Elm.Expression
    , tabularNumbers : Elm.Expression
    , stackedFractions : Elm.Expression
    , diagonalFractions : Elm.Expression
    , swash : Elm.Expression
    , feature : Elm.Expression
    , indexed : Elm.Expression
    , glow : Elm.Expression
    , shadow : Elm.Expression
    }
values_ =
    { color =
        Elm.value
            { importFrom = [ "Element", "Font" ]
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
    , size =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "size"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attr"
                            [ Type.var "decorative", Type.var "msg" ]
                        )
                    )
            }
    , family =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "family"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Element", "Font" ] "Font" [])
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , typeface =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "typeface"
            , annotation =
                Just
                    (Type.function
                        [ Type.string ]
                        (Type.namedWith [ "Element", "Font" ] "Font" [])
                    )
            }
    , serif =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "serif"
            , annotation = Just (Type.namedWith [ "Element", "Font" ] "Font" [])
            }
    , sansSerif =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "sansSerif"
            , annotation = Just (Type.namedWith [ "Element", "Font" ] "Font" [])
            }
    , monospace =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "monospace"
            , annotation = Just (Type.namedWith [ "Element", "Font" ] "Font" [])
            }
    , external =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "external"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "url", Type.string ), ( "name", Type.string ) ]
                        ]
                        (Type.namedWith [ "Element", "Font" ] "Font" [])
                    )
            }
    , alignLeft =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "alignLeft"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , alignRight =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "alignRight"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , center =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "center"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , justify =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "justify"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , letterSpacing =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "letterSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , wordSpacing =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "wordSpacing"
            , annotation =
                Just
                    (Type.function
                        [ Type.float ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , underline =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "underline"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , strike =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "strike"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , italic =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "italic"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , unitalicized =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "unitalicized"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , heavy =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "heavy"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , extraBold =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "extraBold"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , bold =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "bold"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , semiBold =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "semiBold"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , medium =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "medium"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , regular =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "regular"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , light =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "light"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , extraLight =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "extraLight"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , hairline =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "hairline"
            , annotation =
                Just
                    (Type.namedWith [ "Element" ] "Attribute" [ Type.var "msg" ]
                    )
            }
    , variant =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "variant"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Element", "Font" ] "Variant" [] ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , variantList =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "variantList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.namedWith [ "Element", "Font" ] "Variant" [])
                        ]
                        (Type.namedWith
                            [ "Element" ]
                            "Attribute"
                            [ Type.var "msg" ]
                        )
                    )
            }
    , smallCaps =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "smallCaps"
            , annotation =
                Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
            }
    , slashedZero =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "slashedZero"
            , annotation =
                Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
            }
    , ligatures =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "ligatures"
            , annotation =
                Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
            }
    , ordinal =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "ordinal"
            , annotation =
                Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
            }
    , tabularNumbers =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "tabularNumbers"
            , annotation =
                Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
            }
    , stackedFractions =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "stackedFractions"
            , annotation =
                Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
            }
    , diagonalFractions =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "diagonalFractions"
            , annotation =
                Just (Type.namedWith [ "Element", "Font" ] "Variant" [])
            }
    , swash =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "swash"
            , annotation =
                Just
                    (Type.function
                        [ Type.int ]
                        (Type.namedWith [ "Element", "Font" ] "Variant" [])
                    )
            }
    , feature =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "feature"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.bool ]
                        (Type.namedWith [ "Element", "Font" ] "Variant" [])
                    )
            }
    , indexed =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "indexed"
            , annotation =
                Just
                    (Type.function
                        [ Type.string, Type.int ]
                        (Type.namedWith [ "Element", "Font" ] "Variant" [])
                    )
            }
    , glow =
        Elm.value
            { importFrom = [ "Element", "Font" ]
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
    , shadow =
        Elm.value
            { importFrom = [ "Element", "Font" ]
            , name = "shadow"
            , annotation =
                Just
                    (Type.function
                        [ Type.record
                            [ ( "offset", Type.tuple Type.float Type.float )
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


