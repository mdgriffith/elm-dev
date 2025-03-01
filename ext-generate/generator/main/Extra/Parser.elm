module Extra.Parser exposing (annotate, deadEndToString, deadEndsToString, problemToString)

import Parser


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    deadEnds
        |> List.map deadEndToString
        |> String.join "\n"


{-| Draw a little ascii annotation under the given dead end.

        hereismystring
        ^
        I was expecting a / here

-}
annotate : List Parser.DeadEnd -> String -> String
annotate deadEnds message =
    let
        column =
            List.map .col deadEnds
                |> List.maximum
                |> Maybe.withDefault 1
    in
    message
        ++ "\n"
        ++ deadEndToAnnotation column deadEnds


deadEndToAnnotation : Int -> List Parser.DeadEnd -> String
deadEndToAnnotation column deadEnds =
    (String.repeat (column - 1) " " ++ "^\n")
        ++ "I was expecting "
        ++ String.join ", " (List.map (problemToInlineString << .problem) deadEnds)


{-| Turn a parsing problem into the default String representation.
-}
deadEndToString : Parser.DeadEnd -> String
deadEndToString deadEnd =
    "Problem at row " ++ String.fromInt deadEnd.row ++ ", " ++ String.fromInt deadEnd.col ++ "\n" ++ problemToString deadEnd.problem


problemToInlineString : Parser.Problem -> String
problemToInlineString problem =
    case problem of
        Parser.Expecting string ->
            string

        Parser.ExpectingInt ->
            "an int"

        Parser.ExpectingHex ->
            "a hex"

        Parser.ExpectingOctal ->
            "an octal"

        Parser.ExpectingBinary ->
            "some binary"

        Parser.ExpectingFloat ->
            "a float"

        Parser.ExpectingNumber ->
            "a number"

        Parser.ExpectingVariable ->
            "a variable"

        Parser.ExpectingSymbol string ->
            string

        Parser.ExpectingKeyword string ->
            string

        Parser.ExpectingEnd ->
            "the end of input"

        Parser.UnexpectedChar ->
            "unexpected char"

        Parser.Problem problemDescription ->
            problemDescription

        Parser.BadRepeat ->
            "bad repeat"


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Parser.Expecting string ->
            "Expecting " ++ string

        Parser.ExpectingInt ->
            "Expecting int"

        Parser.ExpectingHex ->
            "Expecting hex"

        Parser.ExpectingOctal ->
            "Expecting octal"

        Parser.ExpectingBinary ->
            "Expecting binary"

        Parser.ExpectingFloat ->
            "Expecting float"

        Parser.ExpectingNumber ->
            "Expecting number"

        Parser.ExpectingVariable ->
            "Expecting variable"

        Parser.ExpectingSymbol string ->
            "Expecting symbol " ++ string

        Parser.ExpectingKeyword string ->
            "Expecting keyword " ++ string

        Parser.ExpectingEnd ->
            "Expecting keyword end"

        Parser.UnexpectedChar ->
            "Unexpected char"

        Parser.Problem problemDescription ->
            problemDescription

        Parser.BadRepeat ->
            "Bad repeat"
