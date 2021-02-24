module Elm exposing (..)

{-| -}

import Editor
import Json.Decode as Decode


type alias Project =
    { root : String
    , entrypoints : List String
    , status : Error
    }


type Error
    = NoData
    | Success
    | GlobalError GlobalErrorDetails
    | CompilerError
        { errors : List File
        }


type alias GlobalErrorDetails =
    { path : String
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
    | Styled StyledText


type alias StyledText =
    { color : Maybe Color
    , underline : Bool
    , bold : Bool
    , string : String
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


decodeProject =
    Decode.map3 Project
        (Decode.field "root" Decode.string)
        (Decode.field "entrypoints" (Decode.list Decode.string))
        (Decode.field "status" decodeError)


decodeError =
    Decode.oneOf
        [ Decode.null Success
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
                                    (Decode.field "path" Decode.string)
                                    (Decode.field "title" Decode.string)
                                    (Decode.field "message" (Decode.list text))
                                )

                        Many ->
                            Decode.map (\err -> CompilerError { errors = err })
                                (Decode.field "errors" (Decode.list fileError))
                )
        ]


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


fileError =
    Decode.map3 File
        (Decode.field "path" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "problems" (Decode.list decodeProblem))


decodeProblem =
    Decode.map3 Problem
        (Decode.field "title" Decode.string)
        (Decode.field "message" (Decode.list text))
        (Decode.field "region" Editor.decodeRegion)


text =
    Decode.oneOf
        [ Decode.map Plain Decode.string
        , Decode.map Styled styledText
        ]


styledText =
    Decode.map4 StyledText
        (Decode.field "color" maybeColor)
        (Decode.field "underline" Decode.bool)
        (Decode.field "bold" Decode.bool)
        (Decode.field "string" Decode.string)


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
