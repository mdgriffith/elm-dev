module Data.ProjectStatus exposing (..)

{-| -}

import Data.Editor as Editor
import Dict
import Json.Decode as Decode


successful : List Status -> Bool
successful statuses =
    List.all
        (\status ->
            case status of
                NoData ->
                    True

                Success ->
                    True

                GlobalError _ ->
                    False

                CompilerError _ ->
                    False
        )
        statuses


type alias Project =
    { shortId : Int
    , root : String
    , name : String
    , projectRoot : String
    , entrypoints : List String
    , status : Status
    , docs : DocsOverview
    , dependencies : Deps
    , testDependencies : Deps
    }


type alias Deps =
    { direct : List PackageInfo
    , indirect : List PackageInfo
    }


type alias DocsOverview =
    { modules : List String
    , guides : List String
    , interactive : List String
    }


type alias PackageInfo =
    { name : String
    , version : String
    }


emptyDocsOverview : DocsOverview
emptyDocsOverview =
    { modules = []
    , guides = []
    , interactive = []
    }


type Status
    = NoData
    | Success
    | GlobalError GlobalErrorDetails
    | CompilerError
        { errors : List File
        }


type Warning
    = UnusedVariable
        { region : Editor.Region
        , context : String
        , name : String
        }
    | MissingAnnotation
        { region : Editor.Region
        , name : String
        , signature : String
        }
    | UnusedImport
        { region : Editor.Region
        , name : String
        }


type alias GlobalErrorDetails =
    { path : Maybe String
    , problem :
        { title : String
        , message : List Text
        }
    }


type alias File =
    { path : String
    , name : String
    , problem : List Problem
    }


type alias Problem =
    { title : String
    , message : List Text
    , region : Editor.Region
    }


type alias CodeReferenceKey =
    String


globalKey : GlobalErrorDetails -> Int -> CodeReferenceKey
globalKey global index =
    Maybe.withDefault "global" global.path ++ ":" ++ String.fromInt index


fileKey : File -> Problem -> Int -> CodeReferenceKey
fileKey file problem index =
    file.path ++ ":" ++ Editor.regionToString problem.region ++ String.fromInt index


type Text
    = Plain String
    | Styled StyledText String
      -- This is when we're quoting code from the actual elm file
    | CodeQuote Int (List Text)
      -- This is when we're referencing new code
      -- Like "did you mean xyz?"
    | CodeSection Int (List Text)


type alias StyledText =
    { color : Maybe Color
    , underline : Bool
    , bold : Bool
    }


type Color
    = Red
    | Yellow
    | Green
    | Cyan


type ErrType
    = Single
    | Many


inEditor : File -> Editor.Editor -> Bool
inEditor file editor =
    file.path == editor.filepath



{- DECODERS -}
{- HELPERS -}


decodeProject : Decode.Decoder Project
decodeProject =
    Decode.map8
        (\shortId root name projectRoot entrypoints status docs allDeps ->
            Project shortId root name projectRoot entrypoints status docs allDeps.dependencies allDeps.testDependencies
        )
        (Decode.field "shortId" Decode.int)
        (Decode.field "root" Decode.string)
        (Decode.field "root" Decode.string |> Decode.map nameFromRoot)
        (Decode.field "projectRoot" Decode.string)
        (Decode.field "entrypoints" (Decode.list Decode.string))
        (Decode.field "status" decodeStatus)
        (Decode.map (Maybe.withDefault emptyDocsOverview)
            (Decode.maybe (Decode.field "docs" decodeDocsOverview))
        )
        (Decode.field "elmJson" Decode.string
            |> Decode.andThen
                (\str ->
                    case Decode.decodeString decodeElmJsonFile str of
                        Ok parsed ->
                            Decode.succeed parsed

                        Err _ ->
                            Decode.fail "Failed to decode elm.json"
                )
        )


decodeDocsOverview : Decode.Decoder DocsOverview
decodeDocsOverview =
    Decode.map3 DocsOverview
        (Decode.field "modules" (Decode.list Decode.string))
        (Decode.field "guides" (Decode.list Decode.string))
        (Decode.field "interactive" (Decode.list Decode.string))


{-| Derive a human-friendly project name from a root path.

  - Takes the last path segment
  - Splits on '-' and '\_'
  - Splits camelCase while preserving acronym sequences
  - Capitalizes words, preserving all-caps acronyms

-}
nameFromRoot : String -> String
nameFromRoot root =
    let
        base =
            root
                |> String.split "/"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault root

        delimiterNormalized =
            base
                |> String.map
                    (\c ->
                        if c == '-' || c == '_' then
                            ' '

                        else
                            c
                    )

        words =
            delimiterNormalized
                |> String.words
                |> List.concatMap splitCamelPreserveAcronyms
                |> List.map capitalizePreservingAcronym
    in
    String.join " " words


splitCamelPreserveAcronyms : String -> List String
splitCamelPreserveAcronyms segment =
    let
        chars =
            String.toList segment

        isUpper c =
            let
                s =
                    String.fromChar c
            in
            s == String.toUpper s && s /= String.toLower s

        isLower c =
            let
                s =
                    String.fromChar c
            in
            s == String.toLower s && s /= String.toUpper s

        step prev currentToken revTokens remaining =
            case remaining of
                [] ->
                    case currentToken of
                        [] ->
                            List.reverse revTokens

                        _ ->
                            List.reverse (String.fromList (List.reverse currentToken) :: revTokens)

                c :: xs ->
                    let
                        next =
                            case xs of
                                n :: _ ->
                                    Just n

                                [] ->
                                    Nothing

                        boundary =
                            case prev of
                                Nothing ->
                                    False

                                Just p ->
                                    (isLower p && isUpper c)
                                        || (isUpper p
                                                && isUpper c
                                                && (case next of
                                                        Just n ->
                                                            isLower n

                                                        Nothing ->
                                                            False
                                                   )
                                           )
                    in
                    if boundary then
                        step (Just c) [ c ] (String.fromList (List.reverse currentToken) :: revTokens) xs

                    else
                        step (Just c) (c :: currentToken) revTokens xs
    in
    step Nothing [] [] chars


capitalizePreservingAcronym : String -> String
capitalizePreservingAcronym word =
    if word == String.toUpper word && word /= String.toLower word then
        word

    else
        case String.uncons word of
            Nothing ->
                word

            Just ( first, rest ) ->
                String.toUpper (String.fromChar first) ++ String.toLower rest


decodeStatus : Decode.Decoder Status
decodeStatus =
    Decode.oneOf
        [ Decode.field "compiled" Decode.bool
            |> Decode.map (\_ -> Success)
        , Decode.field "type" decodeErrorType
            |> Decode.andThen
                (\errorType ->
                    case errorType of
                        Single ->
                            Decode.map GlobalError
                                (Decode.map3
                                    (\path title message ->
                                        GlobalErrorDetails path
                                            { title = title
                                            , message = message
                                            }
                                    )
                                    (Decode.field "path"
                                        (Decode.nullable Decode.string)
                                    )
                                    (Decode.field "title" Decode.string)
                                    (Decode.field "message" (Decode.list text))
                                )

                        Many ->
                            Decode.map
                                (\err ->
                                    CompilerError
                                        { errors = err
                                        }
                                )
                                (Decode.field "errors" (Decode.list fileError))
                )
        ]


decodeWarning : Decode.Decoder Warning
decodeWarning =
    Decode.field "warning" Decode.string
        |> Decode.andThen
            (\warning ->
                case warning of
                    "UnusedVariable" ->
                        Decode.map3
                            (\region context name ->
                                UnusedVariable
                                    { region = region
                                    , context = context
                                    , name = name
                                    }
                            )
                            (Decode.field "region"
                                Editor.decodeRegion
                            )
                            (Decode.field "context"
                                Decode.string
                            )
                            (Decode.field "name"
                                Decode.string
                            )

                    "MissingAnnotation" ->
                        Decode.map3
                            (\region signature name ->
                                MissingAnnotation
                                    { region = region
                                    , signature = signature
                                    , name = name
                                    }
                            )
                            (Decode.field "region"
                                Editor.decodeRegion
                            )
                            (Decode.field "signature"
                                Decode.string
                            )
                            (Decode.field "name"
                                Decode.string
                            )

                    "UnusedImport" ->
                        Decode.map2
                            (\region name ->
                                UnusedImport
                                    { region = region
                                    , name = name
                                    }
                            )
                            (Decode.field "region"
                                Editor.decodeRegion
                            )
                            (Decode.field "name"
                                Decode.string
                            )

                    _ ->
                        Decode.fail "Unknown warning"
            )


decodeErrorType : Decode.Decoder ErrType
decodeErrorType =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "error" ->
                        Decode.succeed Single

                    "compile-errors" ->
                        Decode.succeed Many

                    _ ->
                        Decode.fail ("Unsupported error type: " ++ str)
            )


fileError : Decode.Decoder File
fileError =
    Decode.map3 File
        (Decode.field "path" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "problems"
            (Decode.map
                (List.sortBy (.region >> .start >> .line))
                (Decode.list decodeProblem)
            )
        )


decodeProblem : Decode.Decoder Problem
decodeProblem =
    Decode.map3 Problem
        (Decode.field "title" Decode.string)
        (Decode.field "message"
            (Decode.map nestCodingSections (Decode.list text))
        )
        (Decode.field "region" Editor.decodeRegion)


nestCodingSections : List Text -> List Text
nestCodingSections texts =
    nestCodingSectionsHelper texts NoCapture []


nestCodingSectionsHelper texts gatheredCodeRegion gathered =
    case texts of
        [] ->
            case gatheredCodeRegion of
                NoCapture ->
                    List.reverse gathered

                Quote codeRegion ->
                    let
                        ( reversed, lineCount ) =
                            reverseAndCount codeRegion
                    in
                    List.reverse (CodeQuote lineCount reversed :: gathered)

                Reference codeRegion ->
                    let
                        ( reversed, lineCount ) =
                            reverseAndCount codeRegion
                    in
                    List.reverse (CodeSection lineCount reversed :: gathered)

        (Plain txt) :: remaining ->
            let
                ( newCodeRegion, newGathered ) =
                    gatherCodeSectionRecurse Plain
                        txt
                        gatheredCodeRegion
                        gathered
            in
            nestCodingSectionsHelper remaining newCodeRegion newGathered

        (Styled style txt) :: remaining ->
            let
                ( newCodeRegion, newGathered ) =
                    gatherCodeSectionRecurse (Styled style)
                        txt
                        gatheredCodeRegion
                        gathered
            in
            nestCodingSectionsHelper remaining newCodeRegion newGathered

        (CodeSection lineCount txt) :: remaining ->
            -- This branch generally shouldn't happen because we haven't built code sections yet
            nestCodingSectionsHelper remaining gatheredCodeRegion (CodeSection lineCount txt :: gathered)

        (CodeQuote lineCount txt) :: remaining ->
            -- This branch generally shouldn't happen because we haven't built code sections yet
            nestCodingSectionsHelper remaining gatheredCodeRegion (CodeQuote lineCount txt :: gathered)


reverseAndCount : List a -> ( List a, number )
reverseAndCount items =
    reverseAndCountHelper items ( [], 0 )


reverseAndCountHelper items ( reved, count ) =
    case items of
        [] ->
            ( reved, count )

        top :: remain ->
            reverseAndCountHelper remain ( top :: reved, count + 1 )


type Capture
    = NoCapture
    | Quote (List Text)
    | Reference (List Text)


gatherCodeSectionRecurse : (String -> Text) -> String -> Capture -> List Text -> ( Capture, List Text )
gatherCodeSectionRecurse toText str capture accum =
    gatherCodeSectionRecurseHelper toText (String.lines str) True capture accum


{-| -}
gatherCodeSectionRecurseHelper toText lines isFirst capture accum =
    case lines of
        [] ->
            ( capture, accum )

        [ last ] ->
            if isFirst then
                -- there were no newlines
                -- just add normally
                case capture of
                    NoCapture ->
                        ( NoCapture
                        , toText last :: accum
                        )

                    Quote reg ->
                        ( Quote (toText last :: reg)
                        , accum
                        )

                    Reference reg ->
                        ( Reference (toText last :: reg)
                        , accum
                        )

            else
                let
                    line =
                        "\n" ++ last
                in
                -- there were newlines previously
                if startsWithNum last || startsWithWs last then
                    case capture of
                        NoCapture ->
                            ( if startsWithWs last then
                                Reference [ toText last ]

                              else
                                Quote [ toText last ]
                            , toText "\n" :: accum
                            )

                        Quote reg ->
                            ( Quote (toText line :: reg)
                            , accum
                            )

                        Reference reg ->
                            ( Reference (toText line :: reg)
                            , accum
                            )

                else
                    case capture of
                        NoCapture ->
                            ( NoCapture
                            , toText line :: accum
                            )

                        Quote items ->
                            let
                                ( reversed, lineCount ) =
                                    reverseAndCount items
                            in
                            ( NoCapture
                            , toText line :: CodeQuote lineCount reversed :: accum
                            )

                        Reference items ->
                            let
                                ( reversed, lineCount ) =
                                    reverseAndCount items
                            in
                            ( NoCapture
                            , toText line :: CodeSection lineCount reversed :: accum
                            )

        topLine :: remainingLines ->
            if isFirst then
                -- there were no newlines
                -- just add normally
                case capture of
                    NoCapture ->
                        gatherCodeSectionRecurseHelper toText
                            remainingLines
                            False
                            NoCapture
                            (toText topLine :: accum)

                    Quote reg ->
                        gatherCodeSectionRecurseHelper toText
                            remainingLines
                            False
                            (Quote (toText topLine :: reg))
                            accum

                    Reference reg ->
                        gatherCodeSectionRecurseHelper toText
                            remainingLines
                            False
                            (Reference (toText topLine :: reg))
                            accum

            else
                let
                    line =
                        "\n" ++ topLine
                in
                -- there were newlines previously
                if startsWithNum topLine || startsWithWs topLine then
                    case capture of
                        NoCapture ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                (if startsWithWs topLine then
                                    Reference [ toText topLine ]

                                 else
                                    Quote [ toText topLine ]
                                )
                                (toText "\n" :: accum)

                        Quote reg ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                (Quote (toText line :: reg))
                                accum

                        Reference reg ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                (Reference (toText line :: reg))
                                accum

                else
                    case capture of
                        NoCapture ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                NoCapture
                                (toText line :: accum)

                        Quote items ->
                            let
                                ( reversed, lineCount ) =
                                    reverseAndCount items
                            in
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                NoCapture
                                (toText line :: CodeQuote lineCount reversed :: accum)

                        Reference items ->
                            let
                                ( reversed, lineCount ) =
                                    reverseAndCount items
                            in
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                NoCapture
                                (toText line :: CodeSection lineCount reversed :: accum)


startsWithNum : String -> Bool
startsWithNum str =
    case String.uncons (String.left 1 str) of
        Nothing ->
            False

        Just ( top, _ ) ->
            Char.isDigit top


startsWithWs : String -> Bool
startsWithWs str =
    case String.uncons (String.left 1 str) of
        Nothing ->
            False

        Just ( top, _ ) ->
            top == ' '


text : Decode.Decoder Text
text =
    Decode.oneOf
        [ Decode.map Plain Decode.string
        , Decode.map2 Styled
            styledText
            (Decode.field "string" Decode.string)
        ]


styledText : Decode.Decoder StyledText
styledText =
    Decode.map3 StyledText
        (Decode.field "color" maybeColor)
        (Decode.field "underline" Decode.bool)
        (Decode.field "bold" Decode.bool)


maybeColor : Decode.Decoder (Maybe Color)
maybeColor =
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                (\val ->
                    case String.toUpper val of
                        "YELLOW" ->
                            Decode.succeed (Just Yellow)

                        "RED" ->
                            Decode.succeed (Just Red)

                        "CYAN" ->
                            Decode.succeed (Just Cyan)

                        "GREEN" ->
                            Decode.succeed (Just Green)

                        "" ->
                            Decode.succeed Nothing

                        _ ->
                            Decode.fail ("Unknown Color: " ++ val)
                )
        , Decode.null Nothing
        ]



{- elm.json decoding -}


type alias ElmJsonDeps =
    { dependencies : Deps
    , testDependencies : Deps
    }


emptyDeps : Deps
emptyDeps =
    { direct = [], indirect = [] }


decodeElmJsonFile : Decode.Decoder ElmJsonDeps
decodeElmJsonFile =
    Decode.map2 ElmJsonDeps
        (Decode.field "dependencies" decodeElmJsonDeps)
        (Decode.field "test-dependencies" decodeElmJsonDeps)


decodeElmJsonDeps : Decode.Decoder Deps
decodeElmJsonDeps =
    Decode.map2 Deps
        (Decode.field "direct" decodeDeps)
        (Decode.field "indirect" decodeDeps)


decodeDeps : Decode.Decoder (List PackageInfo)
decodeDeps =
    Decode.keyValuePairs Decode.string
        |> Decode.map (List.map (\( name, version ) -> PackageInfo name version))

