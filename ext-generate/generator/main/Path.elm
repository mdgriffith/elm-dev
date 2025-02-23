module Path exposing (extension, join, relative, removeExtension, toFilename)


join : String -> String -> String
join one two =
    if String.endsWith "/" one && String.startsWith "/" two then
        one ++ String.dropLeft 1 two

    else if String.startsWith "/" two then
        one ++ two

    else
        one ++ "/" ++ two


relative : String -> String -> String
relative base full =
    if full |> String.startsWith base then
        String.dropLeft (String.length base) full

    else
        full


extension : String -> ( String, String )
extension str =
    let
        ext =
            if str |> String.startsWith "." then
                ""

            else
                str
                    |> String.split "."
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault ""

        base =
            String.dropRight (String.length ext + 1) str
    in
    ( base, ext )


removeExtension : String -> String
removeExtension str =
    if str |> String.startsWith "." then
        str

    else
        str
            |> String.split "."
            |> List.reverse
            |> List.drop 1
            |> List.reverse
            |> String.join "."


toFilename : String -> String
toFilename path =
    let
        fileName =
            String.split "/" path
                |> List.reverse
                |> List.head
                |> Maybe.withDefault path
    in
    fileName
        |> removeExtension
        |> String.replace "." ""
        |> String.replace "-" ""
        |> String.replace "_" ""
        |> String.replace " " ""
        |> String.replace "/" ""
        |> String.replace "’" ""
        |> String.replace "'" ""
        |> decapitalize


decapitalize : String -> String
decapitalize str =
    case String.uncons str of
        Nothing ->
            str

        Just ( first, tail ) ->
            String.fromChar (Char.toLower first) ++ tail
