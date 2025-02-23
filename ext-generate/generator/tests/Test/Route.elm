module Test.Route exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Generate.Route
import Options.Route
import Parser
import Test exposing (Test)


parseRoute : String -> Result (List Parser.DeadEnd) Options.Route.UrlPatternDetails
parseRoute string =
    Parser.run (Options.Route.parseUrlPattern string) string


parsePage : String -> String -> Result (List Parser.DeadEnd) Options.Route.Page
parsePage id string =
    Result.map
        (\route ->
            { id = id
            , url = Options.Route.UrlPattern route
            , redirectFrom = []
            }
        )
        (parseRoute string)


parseParsedPage : String -> String -> Result (List Parser.DeadEnd) Options.Route.ParsedPage
parseParsedPage id string =
    Result.map
        (\route ->
            { id = id
            , url = Options.Route.UrlParsedPattern route
            , redirectFrom = []
            }
        )
        (parseRoute string)


suite : Test
suite =
    Test.describe "Routes"
        [ overlappingFields
        , overlappingRoutes
        , routeOrder
        , parsing
        ]


overlappingFields : Test
overlappingFields =
    Test.describe "Overlapping fields"
        [ Test.test "Path pieces" <|
            \_ ->
                case parseParsedPage "Home" "/test/:id/other/:id" of
                    Err err ->
                        Expect.fail
                            ("Failed to parse route: " ++ Parser.deadEndsToString err)

                    Ok route ->
                        case Generate.Route.generate [ route ] of
                            Ok _ ->
                                Expect.fail "Fields are not flagged as overlapping"

                            Err [ Generate.Route.FieldCollision collision ] ->
                                Expect.pass

                            Err err ->
                                Expect.fail "An unexpected error was returned"
        , Test.test "Path piece + query parameter" <|
            \_ ->
                case parseParsedPage "Home" "/test/:id/other?{id}" of
                    Err err ->
                        Expect.fail
                            ("Failed to parse route: " ++ Parser.deadEndsToString err)

                    Ok route ->
                        case Generate.Route.generate [ route ] of
                            Ok _ ->
                                Expect.fail "Fields are not flagged as overlapping"

                            Err [ Generate.Route.FieldCollision collision ] ->
                                Expect.pass

                            Err err ->
                                Expect.fail "An unexpected error was returned"
        ]


overlappingRoutes : Test
overlappingRoutes =
    Test.describe "Overlapping routes"
        [ Test.test "Exact match" <|
            \_ ->
                let
                    one =
                        parseParsedPage "Home" "/test/:id/other/"

                    two =
                        parseParsedPage "Other" "/test/:id/other/"
                in
                case Result.map2 Tuple.pair one two of
                    Err err ->
                        Expect.fail
                            ("Failed to parse route: " ++ Parser.deadEndsToString err)

                    Ok ( oneRoute, twoRoute ) ->
                        case Generate.Route.generate [ oneRoute, twoRoute ] of
                            Ok _ ->
                                Expect.fail "Duplicate routes are not overlapping"

                            Err [ Generate.Route.OverlappingRoutes collision ] ->
                                Expect.pass

                            Err err ->
                                Expect.fail ("An unexpected error was returned" ++ Debug.toString err)
        , Test.test "Exact match, different variable names shouldnt matter" <|
            \_ ->
                let
                    one =
                        parseParsedPage "Home" "/test/:id/other/"

                    two =
                        parseParsedPage "Other" "/test/:id123/other/"
                in
                case Result.map2 Tuple.pair one two of
                    Err err ->
                        Expect.fail
                            ("Failed to parse route: " ++ Parser.deadEndsToString err)

                    Ok ( oneRoute, twoRoute ) ->
                        case Generate.Route.generate [ oneRoute, twoRoute ] of
                            Ok _ ->
                                Expect.fail "Duplicate routes are not overlapping"

                            Err [ Generate.Route.OverlappingRoutes collision ] ->
                                Expect.pass

                            Err err ->
                                Expect.fail ("An unexpected error was returned" ++ Debug.toString err)
        ]


parsing : Test
parsing =
    Test.describe "Parsing"
        [ Test.test "Single slash is valid" <|
            \_ ->
                case parsePage "Other" "/" of
                    Err err ->
                        Expect.fail
                            ("Failed to parse route: " ++ Debug.toString err)

                    Ok _ ->
                        Expect.pass
        , Test.test "Simple route is valid" <|
            \_ ->
                case parsePage "Other" "/test/thing/" of
                    Err err ->
                        Expect.fail
                            ("Failed to parse route: " ++ Debug.toString err)

                    Ok _ ->
                        Expect.pass
        , Test.test "The final slash is optional" <|
            \_ ->
                let
                    one =
                        parsePage "Home" "/:id"

                    two =
                        parsePage "Other" "/:id/"
                in
                case Result.map2 Tuple.pair one two of
                    Err err ->
                        Expect.fail
                            ("Failed to parse route: " ++ Parser.deadEndsToString err)

                    Ok ( onePage, twoPage ) ->
                        Expect.equal (toPath onePage.url) (toPath twoPage.url)
        , Test.test "First slash is required" <|
            \_ ->
                case parsePage "Other" ":id/" of
                    Err err ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "The first slash is required"
        , Test.test "Question mark needs something after it" <|
            \_ ->
                case parsePage "Other" "/:id/?" of
                    Err err ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "Question mark needs something after it"
        ]


toPath : Options.Route.UrlPattern -> List Options.Route.UrlPiece
toPath (Options.Route.UrlPattern url) =
    url.path


routeOrder : Test
routeOrder =
    Test.describe "Route order"
        [ Test.test "Catch all should be last" <|
            \_ ->
                let
                    one =
                        parsePage "Home" "/:id/*"

                    two =
                        parsePage "Other" "/:id/"
                in
                case Result.map2 Tuple.pair one two of
                    Err err ->
                        Expect.fail
                            ("Failed to parse route: " ++ Parser.deadEndsToString err)

                    Ok ( onePage, twoPage ) ->
                        let
                            sorted =
                                [ onePage, twoPage ]
                                    |> List.concatMap Generate.Route.toUrlPatterns
                                    |> List.sortBy (.pattern >> Generate.Route.routeOrder)
                                    |> List.map
                                        (.page >> .id)
                        in
                        case sorted of
                            [ "Other", "Home" ] ->
                                Expect.pass

                            _ ->
                                Expect.fail ("An unexpected order was returned! \n" ++ String.join "\n-" sorted)
        , Test.test "Catch all should be last, " <|
            \_ ->
                let
                    one =
                        parsePage "Home" "/:id/*"

                    two =
                        parsePage "Other" "/:id/tests/:thing/other/:thing"
                in
                case Result.map2 Tuple.pair one two of
                    Err err ->
                        Expect.fail
                            ("Failed to parse route: " ++ Parser.deadEndsToString err)

                    Ok ( onePage, twoPage ) ->
                        let
                            sorted =
                                [ onePage, twoPage ]
                                    |> List.concatMap Generate.Route.toUrlPatterns
                                    |> List.sortBy (.pattern >> Generate.Route.routeOrder)
                                    |> List.map
                                        (.page >> .id)
                        in
                        case sorted of
                            [ "Other", "Home" ] ->
                                Expect.pass

                            _ ->
                                Expect.fail ("An unexpected order was returned! \n" ++ String.join "\n-" sorted)
        , Test.test "Known tokens should come before variables" <|
            \_ ->
                let
                    {-

                    -}
                    one =
                        parsePage "Home" "/:id/"

                    two =
                        parsePage "Other" "/test/:id/other/"
                in
                case Result.map2 Tuple.pair one two of
                    Err err ->
                        Expect.fail
                            ("Failed to parse route: " ++ Parser.deadEndsToString err)

                    Ok ( onePage, twoPage ) ->
                        let
                            sorted =
                                [ onePage, twoPage ]
                                    |> List.concatMap Generate.Route.toUrlPatterns
                                    |> List.sortBy (.pattern >> Generate.Route.routeOrder)
                                    |> List.map
                                        (.page >> .id)
                        in
                        case sorted of
                            [ "Other", "Home" ] ->
                                Expect.pass

                            _ ->
                                Expect.fail ("An unexpected order was returned! " ++ String.join "\n-" sorted)
        ]
