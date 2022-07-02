module Elm exposing (..)

{-| -}

import Editor
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


type Text
    = Plain String
    | Styled StyledText String
    | CodeSection (List Text)


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
    file.path == editor.fileName



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
                (List.sortBy (.region >> .start >> .row))
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
    nestCodingSectionsHelper texts Nothing []


nestCodingSectionsHelper texts gatheredCodeRegion gathered =
    case texts of
        [] ->
            case gatheredCodeRegion of
                Nothing ->
                    List.reverse gathered

                Just codeRegion ->
                    List.reverse (CodeSection (List.reverse codeRegion) :: gathered)

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

        (CodeSection txt) :: remaining ->
            -- This branch generally shouldn't happen because we haven't built code sections yet
            nestCodingSectionsHelper remaining gatheredCodeRegion (CodeSection txt :: gathered)


gatherCodeSectionRecurse toText str maybeRegion accum =
    gatherCodeSectionRecurseHelper toText (String.lines str) True maybeRegion accum


{-| -}
gatherCodeSectionRecurseHelper toText lines isFirst maybeRegion accum =
    case lines of
        [] ->
            ( maybeRegion, accum )

        [ last ] ->
            if isFirst then
                -- there were no newlines
                -- just add normally
                case maybeRegion of
                    Nothing ->
                        ( Nothing
                        , toText last :: accum
                        )

                    Just reg ->
                        ( Just (toText last :: reg)
                        , accum
                        )

            else
                let
                    line =
                        "\n" ++ last
                in
                -- there were newlines previously
                if startsWithNum last then
                    case maybeRegion of
                        Nothing ->
                            ( Just [ toText line ]
                            , accum
                            )

                        Just reg ->
                            ( Just (toText line :: reg)
                            , accum
                            )

                else
                    case maybeRegion of
                        Nothing ->
                            ( Nothing
                            , toText line :: accum
                            )

                        Just reg ->
                            ( Nothing
                            , toText line :: CodeSection (List.reverse reg) :: accum
                            )

        topLine :: remainingLines ->
            if isFirst then
                -- there were no newlines
                -- just add normally
                case maybeRegion of
                    Nothing ->
                        gatherCodeSectionRecurseHelper toText
                            remainingLines
                            False
                            Nothing
                            (toText topLine :: accum)

                    Just reg ->
                        gatherCodeSectionRecurseHelper toText
                            remainingLines
                            False
                            (Just (toText topLine :: reg))
                            accum

            else
                let
                    line =
                        "\n" ++ topLine
                in
                -- there were newlines previously
                if startsWithNum topLine then
                    case maybeRegion of
                        Nothing ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                (Just [ toText line ])
                                accum

                        Just reg ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                (Just (toText line :: reg))
                                accum

                else
                    case maybeRegion of
                        Nothing ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                Nothing
                                (toText line :: accum)

                        Just reg ->
                            gatherCodeSectionRecurseHelper toText
                                remainingLines
                                False
                                Nothing
                                (toText line :: CodeSection (List.reverse reg) :: accum)


startsWithNum str =
    case String.uncons (String.left 1 str) of
        Nothing ->
            False

        Just ( top, _ ) ->
            Char.isDigit top


gatherCodeSection toText line ( maybeRegion, accum ) =
    case String.uncons (String.left 1 line) of
        Nothing ->
            -- we are not in a code section or have exited one
            case maybeRegion of
                Nothing ->
                    ( Nothing
                    , toText (line ++ "\n") :: accum
                    )

                Just reg ->
                    ( Nothing
                    , toText (line ++ "\n") :: CodeSection reg :: accum
                    )

        Just ( top, remain ) ->
            if Char.isDigit top then
                -- We are in a code section or started one
                case maybeRegion of
                    Nothing ->
                        ( Just [ toText line ]
                        , accum
                        )

                    Just reg ->
                        ( Just (toText line :: reg)
                        , accum
                        )

            else
                -- we are not in a code section or have exited one
                case maybeRegion of
                    Nothing ->
                        ( Nothing
                        , toText line :: accum
                        )

                    Just reg ->
                        ( Just (toText line :: reg)
                        , accum
                        )



-- type alias Section =
--     { before : String
--     , code : String
--     }
-- splitCodeRegionText inCodeRegion str =
--     List.foldl
--         (\line gathered ->
--             case String.uncons (String.left 1 line) of
--                 Nothing ->
--                     -- we are not in a code section or have exited one
--                     { inCodeRegion = False
--                     }
--                 Just ( top, remain ) ->
--                     if Char.isDigit top then
--                         -- We are in a code section or started one
--                         -- if gathered.inCodeRegion then
--                         { inCodeRegion = True
--                         }
--                     else
--                         -- we are not in a code section or have exited one
--                         { inCodeRegion = False
--                         }
--         )
--         { inCodeRegion = inCodeRegion
--         , before = []
--         , codeRegion = []
--         , after = []
--         }
--         (String.lines str)
--         |> List.reverse


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
