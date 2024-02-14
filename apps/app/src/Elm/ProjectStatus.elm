module Elm.ProjectStatus exposing (..)

{-| -}

import Editor
import Json.Decode as Decode
import Set


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
    { root : String
    , entrypoints : List String
    , status : Status
    }


type Status
    = NoData
    | Success
    | GlobalError GlobalErrorDetails
    | CompilerError
        { errors : List File
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
    Decode.map3 Project
        (Decode.field "root" Decode.string)
        -- disabled for now, it's not being reported
        -- (Decode.field "entrypoints" (Decode.succeed []))
        (Decode.succeed [])
        (Decode.field "status" decodeStatus)


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
