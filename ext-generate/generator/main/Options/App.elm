module Options.App exposing
    ( Options, decode
    , Route, Store
    , toUrlVariables, ParserError, QueryParams, UrlParsedPattern(..), UrlPattern(..), UrlPatternDetails, UrlPiece(..)
    , PageUsage
    )

{-|

@docs Options, decode

@docs Route, Store

@docs Pageusage

@docs toUrlVariables, ParserError, QueryParams, UrlParsedPattern, UrlPattern, UrlPatternDetails, UrlPiece, decode, toUrlVariables

-}

import Json.Decode
import Parser exposing ((|.), (|=))
import Set exposing (Set)


type alias Options =
    { pages : List PageUsage
    , stores : List Store
    }


type alias PageUsage =
    { id : String
    , moduleName : List String
    , value : String
    , paramType : Maybe String
    , elmModuleIsPresent : Bool
    , urlOnly : Bool

    --
    , route : Maybe Route
    }


type alias Store =
    { id : String
    }


decode : Json.Decode.Decoder Options
decode =
    Json.Decode.map2 Options
        (Json.Decode.field "pages" decodePageUsages)
        (Json.Decode.field "stores" (Json.Decode.list decodeStore))


decodeStore : Json.Decode.Decoder Store
decodeStore =
    Json.Decode.map Store
        (Json.Decode.field "id" Json.Decode.string)


decodePageUsages : Json.Decode.Decoder (List PageUsage)
decodePageUsages =
    Json.Decode.list
        (Json.Decode.map7 PageUsage
            (Json.Decode.field "id" Json.Decode.string)
            (Json.Decode.field "moduleName" (Json.Decode.list Json.Decode.string))
            (Json.Decode.field "value" Json.Decode.string)
            (Json.Decode.field "paramType"
                (Json.Decode.oneOf
                    [ Json.Decode.map Just Json.Decode.string
                    , Json.Decode.null Nothing
                    ]
                )
            )
            (Json.Decode.field "elmModuleIsPresent" Json.Decode.bool)
            (Json.Decode.field "urlOnly" Json.Decode.bool)
            (Json.Decode.maybe (Json.Decode.field "route" decodeRoute))
        )


type alias Route =
    { id : String
    , url : UrlParsedPattern
    , redirectFrom : List UrlParsedPattern
    }


type UrlPattern
    = UrlPattern UrlPatternDetails


type UrlParsedPattern
    = UrlParsedPattern UrlPatternDetails
    | UrlError ParserError


type alias ParserError =
    { name : String
    , isRedirect : Bool
    , pattern : String
    , deadEnds : List Parser.DeadEnd
    }


type alias UrlPatternDetails =
    { pattern : String
    , path : List UrlPiece
    , includePathTail : Bool
    , queryParams : QueryParams
    }


type alias QueryParams =
    { includeCatchAll : Bool
    , specificFields : Set String
    }


type UrlPiece
    = Token String
    | Variable String


toUrlVariables : Route -> List String
toUrlVariables route =
    case route.url of
        UrlParsedPattern urlPattern ->
            List.filterMap
                (\piece ->
                    case piece of
                        Token _ ->
                            Nothing

                        Variable variable ->
                            Just variable
                )
                urlPattern.path

        UrlError _ ->
            []


decodeRoute : Json.Decode.Decoder Route
decodeRoute =
    Json.Decode.field "id" Json.Decode.string
        |> Json.Decode.andThen
            (\id ->
                Json.Decode.map2 (Route id)
                    (Json.Decode.field "url" (decodeUrlPattern False id))
                    (Json.Decode.field "redirectFrom" (Json.Decode.list (decodeUrlPattern True id)))
            )


decodeUrlPattern : Bool -> String -> Json.Decode.Decoder UrlParsedPattern
decodeUrlPattern isRedirect id =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case Parser.run (parseUrlPattern string) string of
                    Ok urlPattern ->
                        Json.Decode.succeed (UrlParsedPattern urlPattern)

                    Err err ->
                        Json.Decode.succeed
                            (UrlError
                                { name = id
                                , isRedirect = isRedirect
                                , pattern = string
                                , deadEnds = err
                                }
                            )
            )


{-| Parses a format like

    /users/:id/*?{search}

Which parses

  - id into a string
  - \* into a list of strings
  - and `search` into a list of strings from ?search

-}
parseUrlPattern : String -> Parser.Parser UrlPatternDetails
parseUrlPattern pattern =
    Parser.succeed
        (\path queryParams ->
            { pattern = pattern
            , path = path.path
            , includePathTail = path.includePathTail
            , queryParams = queryParams
            }
        )
        |= parsePath
        |= parseQueryParams


parsePath :
    Parser.Parser
        { includePathTail : Bool
        , path : List UrlPiece
        }
parsePath =
    Parser.loop []
        (\pieces ->
            Parser.oneOf
                [ Parser.succeed (\val -> val)
                    |. Parser.symbol "/"
                    |= Parser.oneOf
                        [ Parser.succeed
                            (Parser.Done
                                { includePathTail = True
                                , path = List.reverse pieces
                                }
                            )
                            |. Parser.symbol "*"
                        , Parser.succeed
                            (\isVariable label ->
                                if isBlank label then
                                    Parser.Loop pieces

                                else
                                    Parser.Loop <|
                                        if isVariable then
                                            Variable label :: pieces

                                        else
                                            Token label :: pieces
                            )
                            |= Parser.oneOf
                                [ Parser.succeed True
                                    |. Parser.chompIf (\c -> c == ':')
                                , Parser.succeed False
                                ]
                            |= Parser.getChompedString
                                (Parser.chompWhile
                                    (\c ->
                                        not (List.member c [ '/', ':', '?' ])
                                    )
                                )
                        ]
                , case pieces of
                    [] ->
                        Parser.oneOf
                            [ Parser.succeed
                                (Parser.Done
                                    { includePathTail = False
                                    , path = List.reverse pieces
                                    }
                                )
                                |. Parser.end
                            , Parser.problem "paths must start with /"
                            ]

                    _ ->
                        Parser.succeed
                            (Parser.Done
                                { includePathTail = False
                                , path = List.reverse pieces
                                }
                            )
                ]
        )


parseQueryParams : Parser.Parser QueryParams
parseQueryParams =
    Parser.oneOf
        [ Parser.succeed
            { includeCatchAll = False
            , specificFields = Set.empty
            }
            |. Parser.end
        , Parser.succeed (\params -> params)
            |. Parser.symbol "?"
            |. Parser.symbol "{"
            |= Parser.oneOf
                [ Parser.succeed
                    { includeCatchAll = True
                    , specificFields = Set.empty
                    }
                    |. Parser.symbol "**"
                , Parser.loop
                    { includeCatchAll = False
                    , specificFields = Set.empty
                    }
                    (\params ->
                        Parser.oneOf
                            [ Parser.succeed
                                (\fieldName ->
                                    Parser.Loop { params | specificFields = Set.insert fieldName params.specificFields }
                                )
                                |= Parser.getChompedString
                                    (Parser.succeed ()
                                        |. Parser.chompIf Char.isAlpha
                                        |. Parser.chompWhile Char.isAlpha
                                    )
                                |. Parser.chompWhile (\c -> c == ',')
                            , Parser.succeed (Parser.Done params)
                            ]
                    )
                ]
            |. Parser.symbol "}"
        ]


isBlank : String -> Bool
isBlank str =
    String.isEmpty (String.trim str)
