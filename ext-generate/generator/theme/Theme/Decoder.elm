module Theme.Decoder exposing (decode)

{-| An example json config here

    {
        "target": "elm-ui",
        "colors": {
            "palette": {
                "white": "#FFFFFF",
                "black": "#000000",
                "grey": {
                    "900": "#121214",
                    "800": "#27272F",
                    "700": "#53535F",
                    "600": "#72727E",
                    "500": "#B1B1B8",
                    "400": "#CECED6",
                    "300": "#E5E5EA",
                    "200": "#F3F4F6",
                    "100": "#FAFAFC"
                },
            },
            "aliases": {
                "primary": "white",
                "neutral": "grey",
                "success": "green",
                "error": "red"
            }
        },
        "spacing": {
            "sm": 4,
            "md": 8,
            "lg": 12,
            "xl": 16,
            "2xl": 20,
            "3xl": 24,
        },
        "typography": [
            {
                "font": ["EB Garamond", "serif"],
                "capitalSizing": {
                    "top": 0.1,
                    "bottom": 0.1,
                    "fontSizeByCapital": 0.1
                },
                "sizes": {
                    "huge": {
                        "size": 120,
                        "weights": [700, 400],
                        "lineHeight": 1.2,
                        "variants": ["huge-bold", "huge-regular"]
                    }
                }
            }
        ],
        "borders": {
            "radius": {
                "sm": 4,
                "md": 8,
                "lg": 12,
                "xl": 16,
                "2xl": 20,
            },
            "width": {
                "sm": 1,
                "md": 2,
                "lg": 3,
                "xl": 4,
                "2xl": 5,
            }
        }
    }

-}

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
    Json.Decode.field "colors" decodeColorSwatch
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
                    (Json.Decode.field "colorRoles" (decodeColorRoles colorSwatches) |> Json.Decode.map Just)
                    (Json.Decode.field "scale" decodeSpacingFromScale)
                    (Json.Decode.field "typography" decodeTypography)
                    (Json.Decode.maybe
                        (Json.Decode.field "borders"
                            (Json.Decode.field "radius" (decodeNamed Json.Decode.int))
                        )
                        |> Json.Decode.map (Maybe.withDefault [])
                    )
                    decodeBorderWidthScale
            )


decodeSpacingFromScale : Json.Decode.Decoder Int
decodeSpacingFromScale =
    Json.Decode.int
        |> Json.Decode.andThen
            (\scale ->
                if scale <= 0 then
                    Json.Decode.fail "`scale` must be a positive integer"

                else
                    Json.Decode.succeed scale
            )


decodeBorderWidthScale : Json.Decode.Decoder Int
decodeBorderWidthScale =
    Json.Decode.maybe (Json.Decode.field "borderWidthScale" Json.Decode.int)
        |> Json.Decode.andThen
            (\maybeScale ->
                let
                    scale =
                        Maybe.withDefault 1 maybeScale
                in
                if scale <= 0 then
                    Json.Decode.fail "`borderWidthScale` must be a positive integer"

                else
                    Json.Decode.succeed scale
            )


decodeColorRoles : List Theme.ColorInstance -> Json.Decode.Decoder Theme.ColorThemeDefinitions
decodeColorRoles colors =
    Json.Decode.keyValuePairs Json.Decode.value
        |> Json.Decode.andThen
            (\pairs ->
                let
                    expectedNamespaces =
                        [ "text", "background", "border" ]

                    seenNamespaces =
                        List.map Tuple.first pairs

                    unknownNamespaces =
                        List.filter (\name -> not (List.member name expectedNamespaces)) seenNamespaces

                    missingNamespaces =
                        List.filter (\name -> not (List.member name seenNamespaces)) expectedNamespaces
                in
                case ( unknownNamespaces, missingNamespaces ) of
                    ( unknown :: _, _ ) ->
                        Json.Decode.fail ("Unknown colorRoles namespace `" ++ unknown ++ "`. Supported namespaces: text, background, border")

                    ( [], missing :: _ ) ->
                        Json.Decode.fail ("Missing required colorRoles namespace `" ++ missing ++ "`")

                    ( [], [] ) ->
                        decodeColorRolesFromPairs colors pairs
            )


decodeColorRolesFromPairs :
    List Theme.ColorInstance
    -> List ( String, Json.Decode.Value )
    -> Json.Decode.Decoder Theme.ColorThemeDefinitions
decodeColorRolesFromPairs colors pairs =
    let
        findValue key =
            pairs
                |> List.filter (\( name, _ ) -> name == key)
                |> List.head
                |> Maybe.map Tuple.second
    in
    case ( findValue "text", findValue "background", findValue "border" ) of
        ( Just textValue, Just backgroundValue, Just borderValue ) ->
            Json.Decode.map3
                (\text background border ->
                    let
                        hasDarkOverrides =
                            text.dark /= Nothing || background.dark /= Nothing || border.dark /= Nothing

                        darkTheme =
                            { text = Maybe.withDefault text.base text.dark
                            , background = Maybe.withDefault background.base background.dark
                            , border = Maybe.withDefault border.base border.dark
                            }
                    in
                    { default =
                        { text = text.base
                        , background = background.base
                        , border = border.base
                        }
                    , alternates =
                        if hasDarkOverrides then
                            [ { name = Name "darkmode", item = darkTheme } ]

                        else
                            []
                    }
                )
                (decodeColorRoleNamespace colors "text" textValue)
                (decodeColorRoleNamespace colors "background" backgroundValue)
                (decodeColorRoleNamespace colors "border" borderValue)

        _ ->
            Json.Decode.fail "colorRoles is missing one of: text, background, border"


type alias DecodedRoleNamespace =
    { base : List Theme.ColorDefinition
    , dark : Maybe (List Theme.ColorDefinition)
    }


decodeColorRoleNamespace :
    List Theme.ColorInstance
    -> String
    -> Json.Decode.Value
    -> Json.Decode.Decoder DecodedRoleNamespace
decodeColorRoleNamespace colors namespaceName value =
    case Json.Decode.decodeValue (Json.Decode.keyValuePairs Json.Decode.value) value of
        Err err ->
            Json.Decode.fail (Json.Decode.errorToString err)

        Ok pairs ->
            let
                modeKeys =
                    List.filter (\( key, _ ) -> String.startsWith "@" key) pairs

                invalidModeKeys =
                    List.filter (\( key, _ ) -> key /= "@dark") modeKeys

                basePairs =
                    List.filter (\( key, _ ) -> not (String.startsWith "@" key)) pairs

                maybeDarkValue =
                    List.filter (\( key, _ ) -> key == "@dark") pairs
                        |> List.head
                        |> Maybe.map Tuple.second
            in
            case invalidModeKeys of
                ( badMode, _ ) :: _ ->
                    Json.Decode.fail ("Unsupported mode `" ++ badMode ++ "` under colorRoles." ++ namespaceName ++ ". Only @dark is supported")

                [] ->
                    decodeRoleDefinitions colors namespaceName Nothing basePairs
                        |> Json.Decode.andThen
                            (\baseDefs ->
                                case maybeDarkValue of
                                    Nothing ->
                                        Json.Decode.succeed
                                            { base = baseDefs
                                            , dark = Nothing
                                            }

                                    Just darkValue ->
                                        Json.Decode.decodeValue (decodeDarkModeDefinitions colors namespaceName baseDefs) darkValue
                                            |> Result.mapError Json.Decode.errorToString
                                            |> resultToDecoder
                            )


decodeDarkModeDefinitions :
    List Theme.ColorInstance
    -> String
    -> List Theme.ColorDefinition
    -> Json.Decode.Decoder DecodedRoleNamespace
decodeDarkModeDefinitions colors namespaceName baseDefs =
    Json.Decode.keyValuePairs Json.Decode.value
        |> Json.Decode.andThen
            (\darkPairs ->
                decodeRoleDefinitions colors namespaceName (Just "darkmode") darkPairs
                    |> Json.Decode.andThen
                        (\darkDefs ->
                            let
                                baseKeys =
                                    Set.fromList (List.map .name baseDefs)

                                darkKeys =
                                    Set.fromList (List.map .name darkDefs)
                            in
                            if baseKeys == darkKeys then
                                Json.Decode.succeed
                                    { base = baseDefs
                                    , dark = Just darkDefs
                                    }

                            else
                                Json.Decode.fail
                                    ("colorRoles." ++ namespaceName ++ ".@dark must define exactly the same keys as colorRoles." ++ namespaceName)
                        )
            )


decodeRoleDefinitions :
    List Theme.ColorInstance
    -> String
    -> Maybe String
    -> List ( String, Json.Decode.Value )
    -> Json.Decode.Decoder (List Theme.ColorDefinition)
decodeRoleDefinitions colors namespaceName maybeThemeName pairs =
    pairs
        |> List.map
            (\( roleName, roleValue ) ->
                Json.Decode.decodeValue (decodeRoleDefinition colors roleName maybeThemeName) roleValue
            )
        |> foldResults
        |> Result.mapError
            (\error ->
                "Error in colorRoles."
                    ++ namespaceName
                    ++ ": "
                    ++ Json.Decode.errorToString error
            )
        |> Result.map List.reverse
        |> resultToDecoder


decodeRoleDefinition :
    List Theme.ColorInstance
    -> String
    -> Maybe String
    -> Json.Decode.Decoder Theme.ColorDefinition
decodeRoleDefinition colors roleName maybeThemeName =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\tokenName ->
                    case lookupExactColor tokenName colors of
                        Nothing ->
                            Json.Decode.fail ("I don't recognize this color token: " ++ tokenName)

                        Just color ->
                            Json.Decode.succeed
                                { name = roleName
                                , color = color
                                , hover = Nothing
                                , active = Nothing
                                , focus = Nothing
                                , theme = maybeThemeName
                                }
                )
        , Json.Decode.map4
            (\base hover active focus ->
                { name = roleName
                , color = base
                , hover = hover
                , active = active
                , focus = focus
                , theme = maybeThemeName
                }
            )
            (Json.Decode.field "default" (decodeColorRef colors))
            (maybeField "hover" (decodeColorRef colors))
            (maybeField "active" (decodeColorRef colors))
            (maybeField "focus" (decodeColorRef colors))
        ]


decodeTypography : Json.Decode.Decoder (List (Named Typeface))
decodeTypography =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "families" (Json.Decode.dict (nonEmptyList Json.Decode.string)))
        (Json.Decode.field "instances" (Json.Decode.keyValuePairs decodeTypographyInstance))
        |> Json.Decode.andThen
            (\( families, instances ) ->
                instances
                    |> List.map
                        (\( instanceName, instanceDef ) ->
                            case Dict.get instanceDef.family families of
                                Nothing ->
                                    Json.Decode.fail
                                        ("Typography instance `"
                                            ++ instanceName
                                            ++ "` references unknown family `"
                                            ++ instanceDef.family
                                            ++ "`"
                                        )

                                Just ( face, fallback ) ->
                                    Json.Decode.succeed
                                        { name = Name instanceName
                                        , item =
                                            { face = face
                                            , fallback = fallback
                                            , weight = ( toWeightName instanceDef.weight, instanceDef.weight )
                                            , size = instanceDef.size
                                            , lineHeight = instanceDef.lineHeight
                                            , variants = []
                                            , capitalSizing = Nothing
                                            }
                                        }
                        )
                    |> List.foldl
                        (\decoder acc ->
                            Json.Decode.map2 (::) decoder acc
                        )
                        (Json.Decode.succeed [])
                    |> Json.Decode.map List.reverse
            )


decodeTypographyInstance :
    Json.Decode.Decoder
        { family : String
        , size : Int
        , weight : Int
        , lineHeight : Float
        }
decodeTypographyInstance =
    Json.Decode.map4
        (\family size weight lineHeight ->
            { family = family
            , size = size
            , weight = weight
            , lineHeight = lineHeight
            }
        )
        (Json.Decode.field "family" Json.Decode.string)
        (Json.Decode.field "size" Json.Decode.int)
        (Json.Decode.field "weight" Json.Decode.int)
        (Json.Decode.field "lineHeight" Json.Decode.float)


toWeightName : Int -> WeightName
toWeightName weight =
    if weight < 400 then
        Light

    else if weight >= 700 then
        Bold

    else
        Default


resultToDecoder : Result String a -> Json.Decode.Decoder a
resultToDecoder result =
    case result of
        Ok value ->
            Json.Decode.succeed value

        Err message ->
            Json.Decode.fail message


foldResults : List (Result x a) -> Result x (List a)
foldResults results =
    List.foldl
        (\next gathered ->
            case ( next, gathered ) of
                ( Ok value, Ok list ) ->
                    Ok (value :: list)

                ( Err err, _ ) ->
                    Err err

                ( _, Err err ) ->
                    Err err
        )
        (Ok [])
        results


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


{-| -}
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
    | SwatchFrom Color.Color


decodeColorSwatch : Json.Decode.Decoder (List Theme.ColorInstance)
decodeColorSwatch =
    Json.Decode.keyValuePairs
        (Json.Decode.oneOf
            [ Json.Decode.map SingleColor decodeColor
            , Json.Decode.field "swatchFrom" decodeColor
                |> Json.Decode.andThen
                    (\themeColor ->
                        case themeColor of
                            Theme.Color.Color _ color ->
                                Json.Decode.succeed (SwatchFrom color)

                            Theme.Color.Grad _ ->
                                Json.Decode.fail "`swatchFrom` only supports concrete colors, not gradients"
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

                        SwatchFrom color ->
                            autoswatch key color
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
                            { name = Name name
                            , item =
                                { face = font
                                , fallback = fallback
                                , weight = size.weight
                                , size = size.size
                                , lineHeight = size.lineHeight
                                , variants = size.variants
                                , capitalSizing = maybeCapitalSizing
                                }
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
