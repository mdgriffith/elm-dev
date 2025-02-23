module Theme.Decoder exposing (decode)

{-| -}

import Color
import Dict
import Json.Decode
import Parser exposing ((|.), (|=))
import Set
import Theme exposing (..)
import Theme.Color


literal : String -> a -> Json.Decode.Decoder a
literal str value =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str2 ->
                if str == str2 then
                    Json.Decode.succeed value

                else
                    Json.Decode.fail ("Expected " ++ str ++ " but got " ++ str2)
            )


gatherAltnerateThemeNames : List Theme.ColorDefinition -> Set.Set String -> Set.Set String
gatherAltnerateThemeNames defs existing =
    List.foldl
        (\def set ->
            case def.theme of
                Just name ->
                    Set.insert name set

                Nothing ->
                    set
        )
        existing
        defs


decode : Json.Decode.Decoder Theme
decode =
    Json.Decode.at [ "colors", "palette" ] decodeColorSwatch
        |> Json.Decode.andThen
            (\colorSwatches ->
                Json.Decode.map6 (Theme "ui" colorSwatches)
                    (Json.Decode.field "target"
                        (Json.Decode.oneOf
                            [ literal "html" HTML
                            , literal "elm-ui" ElmUI
                            ]
                        )
                    )
                    (Json.Decode.map Just
                        (Json.Decode.at [ "colors", "aliases" ]
                            (decodeSemanticMap
                                [ "primary"
                                , "neutral"
                                , "success"
                                , "error"
                                ]
                            )
                            |> Json.Decode.andThen
                                (\semanticMap ->
                                    Json.Decode.field "colors"
                                        (Json.Decode.map3
                                            (\text bgs borders ->
                                                let
                                                    uniqueThemeNames =
                                                        Set.empty
                                                            |> gatherAltnerateThemeNames text
                                                            |> gatherAltnerateThemeNames bgs
                                                            |> gatherAltnerateThemeNames borders
                                                in
                                                { default =
                                                    { text = List.filter (\t -> t.theme == Nothing) text
                                                    , background = List.filter (\t -> t.theme == Nothing) bgs
                                                    , border = List.filter (\t -> t.theme == Nothing) borders
                                                    }
                                                , alternates =
                                                    Set.toList uniqueThemeNames
                                                        |> List.map
                                                            (\name ->
                                                                { name = Name name
                                                                , item =
                                                                    { text = List.filter (\t -> t.theme == Just name) text
                                                                    , background = List.filter (\t -> t.theme == Just name) bgs
                                                                    , border = List.filter (\t -> t.theme == Just name) borders
                                                                    }
                                                                }
                                                            )
                                                }
                                            )
                                            (Json.Decode.field "text" (decodeNamedColorDefinitions colorSwatches semanticMap))
                                            (Json.Decode.field "background" (decodeNamedColorDefinitions colorSwatches semanticMap))
                                            (Json.Decode.field "border" (decodeNamedColorDefinitions colorSwatches semanticMap))
                                        )
                                )
                        )
                    )
                    (Json.Decode.field "spacing"
                        (decodeNamed Json.Decode.int)
                    )
                    (Json.Decode.field "typography"
                        (Json.Decode.map List.concat (Json.Decode.list decodeTypeface))
                    )
                    (Json.Decode.maybe
                        (Json.Decode.field "borders"
                            (Json.Decode.field "radius" (decodeNamed Json.Decode.int))
                        )
                        |> Json.Decode.map (Maybe.withDefault [])
                    )
                    (Json.Decode.maybe
                        (Json.Decode.field "borders"
                            (Json.Decode.field "width" (decodeNamed Json.Decode.int))
                        )
                        |> Json.Decode.map (Maybe.withDefault [])
                    )
            )


{-| A map of color names to semantic names
-}
type alias SemanticMap =
    Dict.Dict String String


decodeSemanticMap : List String -> Json.Decode.Decoder SemanticMap
decodeSemanticMap allowedKeys =
    case allowedKeys of
        [] ->
            Json.Decode.succeed Dict.empty

        key :: remaining ->
            Json.Decode.maybe (Json.Decode.field key Json.Decode.string)
                |> Json.Decode.andThen
                    (\maybeValue ->
                        decodeSemanticMap remaining
                            |> Json.Decode.map
                                (\restOfMap ->
                                    case maybeValue of
                                        Nothing ->
                                            restOfMap

                                        Just value ->
                                            Dict.insert value key restOfMap
                                )
                    )


decodeNamedColorDefinitions : List Theme.ColorInstance -> SemanticMap -> Json.Decode.Decoder (List Theme.ColorDefinition)
decodeNamedColorDefinitions colors semanticMap =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\colorStr ->
                    case lookupExactColor colorStr colors of
                        Just color ->
                            Json.Decode.succeed
                                [ { name = ""
                                  , color = color
                                  , hover = Nothing
                                  , active = Nothing
                                  , focus = Nothing
                                  , theme = Nothing
                                  }
                                ]

                        Nothing ->
                            Json.Decode.fail ("I don't recognize this color: " ++ colorStr)
                )
        , Json.Decode.keyValuePairs (decodeColorDefinition "" colors semanticMap)
            |> Json.Decode.map
                (\keyValues ->
                    List.concatMap
                        (\( name, defs ) ->
                            defs
                                |> List.map
                                    (\def ->
                                        { def
                                            | name = name
                                        }
                                    )
                        )
                        keyValues
                )
        ]


{-| -}
decodeColorDefinition :
    String
    -> List Theme.ColorInstance
    -> SemanticMap
    -> Json.Decode.Decoder (List Theme.ColorDefinition)
decodeColorDefinition styleName colors semanticMap =
    Json.Decode.oneOf
        [ -- Handle simple string case
          Json.Decode.string
            |> Json.Decode.map
                (\colorStr ->
                    case lookupExactColor colorStr colors of
                        Just color ->
                            [ { name = ""
                              , color = color
                              , hover = Nothing
                              , active = Nothing
                              , focus = Nothing
                              , theme = Nothing
                              }
                            ]

                        Nothing ->
                            []
                )

        -- Handle object case
        , Json.Decode.map2 (::)
            (decodeColorDefinitionNode Nothing styleName colors semanticMap)
            (Json.Decode.keyValuePairs Json.Decode.value
                |> Json.Decode.andThen
                    (\keyValues ->
                        let
                            decodePair ( key, value ) =
                                if String.startsWith "@" key then
                                    case
                                        Json.Decode.decodeValue
                                            (decodeColorDefinitionNode
                                                (Just (String.dropLeft 1 key))
                                                styleName
                                                colors
                                                semanticMap
                                            )
                                            value
                                    of
                                        Ok colorDef ->
                                            Ok (Just colorDef)

                                        Err err ->
                                            Err err

                                else
                                    Ok Nothing

                            decodedResults =
                                List.map decodePair keyValues
                        in
                        case
                            List.foldr
                                (\result acc ->
                                    case ( result, acc ) of
                                        ( Ok maybeVal, Ok vals ) ->
                                            case maybeVal of
                                                Just val ->
                                                    Ok (val :: vals)

                                                Nothing ->
                                                    Ok vals

                                        ( Err err, _ ) ->
                                            Err err

                                        ( _, Err err ) ->
                                            Err err
                                )
                                (Ok [])
                                decodedResults
                        of
                            Ok values ->
                                Json.Decode.succeed values

                            Err err ->
                                Json.Decode.fail (Json.Decode.errorToString err)
                    )
            )
        ]


decodeColorDefinitionNode :
    Maybe String
    -> String
    -> List Theme.ColorInstance
    -> SemanticMap
    -> Json.Decode.Decoder Theme.ColorDefinition
decodeColorDefinitionNode maybeTheme styleName colors semanticMap =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\colorStr ->
                    case lookupExactColor colorStr colors of
                        Just color ->
                            Json.Decode.succeed
                                { name = styleName
                                , color = color
                                , hover = Nothing
                                , active = Nothing
                                , focus = Nothing
                                , theme = Nothing
                                }

                        Nothing ->
                            Json.Decode.fail ("I don't recognize this color: " ++ colorStr)
                )
        , Json.Decode.map4
            (\color hover active focus ->
                { name = styleName
                , color = color
                , hover = hover
                , active = active
                , focus = focus
                , theme = maybeTheme
                }
            )
            (Json.Decode.field "color" (decodeColorRef colors))
            (maybeField ":hover" (decodeColorRef colors))
            (maybeField ":active" (decodeColorRef colors))
            (maybeField ":focus" (decodeColorRef colors))
        ]


maybeField : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
maybeField key decoder =
    Json.Decode.oneOf
        [ Json.Decode.field key decoder |> Json.Decode.map Just
        , Json.Decode.field key (Json.Decode.succeed ())
            |> Json.Decode.andThen
                (\_ ->
                    Json.Decode.fail ("I don't recognize the value for " ++ key)
                )
        , Json.Decode.succeed Nothing
        ]


decodeColorRef : List Theme.ColorInstance -> Json.Decode.Decoder Theme.ColorInstance
decodeColorRef colors =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case lookupExactColor str colors of
                    Just color ->
                        Json.Decode.succeed color

                    Nothing ->
                        Json.Decode.fail ("I don't recognize this color: " ++ str)
            )


lookupExactColor : String -> List Theme.ColorInstance -> Maybe Theme.ColorInstance
lookupExactColor colorVar colors =
    List.head (lookupColorPath colorVar colors)


{-|

    A color var can be:

        - purple: matches the full swatch
        - primary: matches the full swatch
        - 700: Matches a slice of each swatch
        - primary700: matches exactly one color
        - purple700: matches exactly one color

-}
lookupColorPath :
    String
    -> List Theme.ColorInstance
    -> List Theme.ColorInstance
lookupColorPath colorVar colors =
    let
        matchVariant instance =
            case instance.variant of
                Just variant ->
                    colorVar == String.fromInt variant

                Nothing ->
                    False

        match instance =
            (colorVar == instance.name)
                || matchVariant instance
                || (colorVar == Theme.toColorName instance)
    in
    List.filter match colors


getNuance : String -> List String -> Maybe String
getNuance base path =
    case path of
        [] ->
            Nothing

        "neutral" :: remaining ->
            getNuance base remaining

        "primary" :: remaining ->
            getNuance base remaining

        "success" :: remaining ->
            getNuance base remaining

        "error" :: remaining ->
            getNuance base remaining

        "default" :: remaining ->
            getNuance base remaining

        "active" :: remaining ->
            getNuance base remaining

        "focus" :: remaining ->
            getNuance base remaining

        "hover" :: remaining ->
            getNuance base remaining

        nuance :: remain ->
            if base /= nuance then
                Just nuance

            else
                getNuance base remain


getState : List String -> Maybe Theme.State
getState path =
    case path of
        [] ->
            Nothing

        "hover" :: remaining ->
            Just Hover

        "active" :: remaining ->
            Just Active

        "focus" :: remaining ->
            Just Focus

        _ :: remain ->
            getState remain



-- decodeColorReference : List Theme.ColorInstance -> Json.Decode.Decoder Theme.Color.Color
-- decodeColorReference colors =
--     Json.Decode.string
--         |> Json.Decode.andThen
--             (\string ->
--                 case Dict.get string colors of
--                     Just color ->
--                         Json.Decode.succeed color
--                     Nothing ->
--                         Json.Decode.fail ("I don't recognize this color: " ++ string)
--             )
-- type alias ColorInstance =
--     { color : Theme.Color.Color
--     , name : String
--     , variant : Maybe String
--     }


type ColorIntermediate
    = SingleColor Theme.Color.Color
    | PaletteColor
        { colors : List { name : String, color : Theme.Color.Color }
        }


decodeColorSwatch : Json.Decode.Decoder (List Theme.ColorInstance)
decodeColorSwatch =
    Json.Decode.keyValuePairs
        (Json.Decode.oneOf
            [ Json.Decode.map SingleColor decodeColor
            , Json.Decode.keyValuePairs (Json.Decode.maybe decodeColor)
                |> Json.Decode.andThen
                    (\colorPairs ->
                        Json.Decode.succeed
                            (PaletteColor
                                { colors =
                                    List.filterMap
                                        (\( name, maybeColor ) ->
                                            case maybeColor of
                                                Nothing ->
                                                    Nothing

                                                Just color ->
                                                    Just
                                                        { name = name
                                                        , color = color
                                                        }
                                        )
                                        colorPairs
                                }
                            )
                    )
            ]
        )
        |> Json.Decode.map
            (List.concatMap
                (\( key, inner ) ->
                    case inner of
                        SingleColor (Theme.Color.Grad grad) ->
                            [ { color = Theme.Color.Grad grad
                              , name = key
                              , variant = Nothing
                              }
                            ]

                        SingleColor (Theme.Color.Color _ color) ->
                            autoswatch key color

                        PaletteColor pal ->
                            List.map
                                (\item ->
                                    { color = item.color
                                    , name = key
                                    , variant =
                                        String.toInt item.name
                                            |> Maybe.map
                                                (\n ->
                                                    min 100 (max 0 n)
                                                )
                                    }
                                )
                                pal.colors
                )
            )


autoswatch : String -> Color.Color -> List Theme.ColorInstance
autoswatch baseName baseColor =
    let
        toLuminance n color =
            { color = Theme.Color.atLightness n color
            , name = baseName
            , variant = Just n
            }
    in
    [ toLuminance 5 baseColor
    , toLuminance 10 baseColor
    , toLuminance 20 baseColor
    , toLuminance 30 baseColor
    , toLuminance 40 baseColor
    , toLuminance 50 baseColor
    , toLuminance 60 baseColor
    , toLuminance 70 baseColor
    , toLuminance 80 baseColor
    , toLuminance 90 baseColor
    , toLuminance 95 baseColor
    ]


decodeColor : Json.Decode.Decoder Theme.Color.Color
decodeColor =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case Parser.run Theme.Color.cssParser string of
                    Ok color ->
                        Json.Decode.succeed color

                    Err err ->
                        Json.Decode.fail ("I don't recognize this color: " ++ string)
            )


decodeNamed : Json.Decode.Decoder a -> Json.Decode.Decoder (List (Named a))
decodeNamed inner =
    Json.Decode.keyValuePairs inner
        |> Json.Decode.map
            (List.map
                (\( key, value ) ->
                    { name = Name key
                    , item = value
                    }
                )
            )


{-|

    "typography": {
        "interface": {
            "font": ["EB Garamond", "serif"]
            "sizes":
                { "huge":
                    { "size": 120
                    , "weight": 700
                    }
                }
            }
        }
    }

-}
decodeTypeface : Json.Decode.Decoder (List (Named Typeface))
decodeTypeface =
    Json.Decode.map3
        (\( font, fallback ) maybeCapitalSizing namedSizes ->
            List.concatMap
                (\( name, sizes ) ->
                    List.map
                        (\size ->
                            Named (Name name)
                                { face = font
                                , fallback = fallback
                                , weight = size.weight
                                , size = size.size
                                , lineHeight = size.lineHeight
                                , variants = size.variants
                                , capitalSizing = maybeCapitalSizing
                                }
                        )
                        sizes
                )
                namedSizes
        )
        (Json.Decode.field "font" (nonEmptyList Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "capitalSizing" decodeCapitalSizing))
        (Json.Decode.field "sizes"
            (Json.Decode.keyValuePairs decodeTypefaceSize)
        )


decodeCapitalSizing :
    Json.Decode.Decoder
        { top : Float
        , bottom : Float
        , fontSizeByCapital : Maybe Float
        }
decodeCapitalSizing =
    Json.Decode.map3
        (\top bottom fontSizeByCapital ->
            { top = Maybe.withDefault 0 top
            , bottom = Maybe.withDefault 0 bottom
            , fontSizeByCapital = fontSizeByCapital
            }
        )
        (Json.Decode.maybe (Json.Decode.field "top" Json.Decode.float))
        (Json.Decode.maybe (Json.Decode.field "bottom" Json.Decode.float))
        (Json.Decode.maybe (Json.Decode.field "fontSizeByCapital" Json.Decode.float))


decodeTypefaceSize :
    Json.Decode.Decoder
        (List
            { size : Int
            , weight : ( WeightName, Int )
            , lineHeight : Float
            , variants : List String
            }
        )
decodeTypefaceSize =
    Json.Decode.map4
        (\size weights variants lineHeight ->
            List.map
                (\weight ->
                    { size = size
                    , weight = weight
                    , lineHeight = lineHeight
                    , variants = variants
                    }
                )
                weights
        )
        (Json.Decode.field "size" Json.Decode.int)
        decodeWeights
        decodeVariants
        (Json.Decode.maybe (Json.Decode.field "lineHeight" Json.Decode.float)
            |> Json.Decode.map (Maybe.withDefault 1.2)
        )


decodeWeights : Json.Decode.Decoder (List ( WeightName, Int ))
decodeWeights =
    Json.Decode.oneOf
        [ Json.Decode.field "weight" (Json.Decode.map (\weight -> [ ( Default, weight ) ]) Json.Decode.int)
        , Json.Decode.field "weights"
            (Json.Decode.andThen
                (\weights ->
                    case List.sort weights of
                        [] ->
                            Json.Decode.succeed [ ( Default, 400 ) ]

                        [ weightOne ] ->
                            Json.Decode.succeed [ ( Default, weightOne ) ]

                        [ weightOne, weightTwo ] ->
                            if weightOne < 400 then
                                Json.Decode.succeed [ ( Light, weightOne ), ( Regular, weightTwo ) ]

                            else
                                Json.Decode.succeed [ ( Regular, weightOne ), ( Bold, weightTwo ) ]

                        [ weightOne, weightTwo, weightThree ] ->
                            Json.Decode.succeed
                                [ ( Light, weightOne )
                                , ( Regular, weightTwo )
                                , ( Bold, weightThree )
                                ]

                        tooMany ->
                            Json.Decode.fail
                                ("I can only support a max of 3 distinct weights for a given typography size!  But I found " ++ String.join ", " (List.map String.fromInt weights))
                )
                (Json.Decode.list Json.Decode.int)
            )
        , Json.Decode.succeed [ ( Default, 400 ) ]
        ]


decodeVariants =
    Json.Decode.oneOf
        [ Json.Decode.field "variant" (Json.Decode.map List.singleton Json.Decode.string)
        , Json.Decode.field "variants" (Json.Decode.list Json.Decode.string)
        , Json.Decode.succeed []
        ]


nonEmptyList : Json.Decode.Decoder a -> Json.Decode.Decoder ( a, List a )
nonEmptyList decoder =
    Json.Decode.list decoder
        |> Json.Decode.andThen
            (\list ->
                case list of
                    [] ->
                        Json.Decode.fail "Expected a non-empty list"

                    first :: rest ->
                        Json.Decode.succeed ( first, rest )
            )


decodePalette : Json.Decode.Decoder thing -> Json.Decode.Decoder (Palette thing)
decodePalette decodeThing =
    Json.Decode.oneOf
        [ Json.Decode.map Single decodeThing
        , Json.Decode.map Palette (decodeNamed decodeThing)
        ]
