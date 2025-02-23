module Asset.FrontMatter exposing (parse, parseOrFail)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=))


{-| Parse the frontmatter of a Markdown file
-}
parse :
    String
    ->
        { attrs : Dict String String
        , body : String
        }
parse input =
    case Parser.run frontMatter input of
        Ok result ->
            result

        Err err ->
            { attrs = Dict.empty
            , body = input
            }


{-| -}
parseOrFail :
    String
    ->
        Result
            (List Parser.DeadEnd)
            { attrs : Dict String String
            , body : String
            }
parseOrFail input =
    Parser.run frontMatter input


{-| Parses a block of frontmatter into a dictionary of keys and their multiline values.
-}
frontMatter : Parser.Parser { attrs : Dict String String, body : String }
frontMatter =
    Parser.succeed
        (\attrs markdown ->
            { attrs = attrs
            , body = markdown
            }
        )
        |. Parser.spaces
        |= Parser.loop ( Dict.empty, Nothing )
            frontMatterItem
        |= Parser.getChompedString
            (Parser.chompWhile (\_ -> True))


type alias Cursor =
    ( String, String )


type KeyOrContent
    = Key String
    | Content String


keyOrContent : Parser.Parser KeyOrContent
keyOrContent =
    Parser.succeed
        (\first str isKey ->
            if isKey then
                Key (String.trim (first ++ str))

            else
                Content (first ++ str)
        )
        |= Parser.getChompedString
            (Parser.chompIf
                (\char ->
                    (char /= ':')
                        && (char /= '\n')
                )
            )
        |= Parser.getChompedString
            (Parser.chompWhile
                (\char ->
                    (char /= ':')
                        && (char /= '\n')
                )
            )
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.chompIf (\char -> char == ':')
            , Parser.succeed False
            ]
        |. Parser.chompWhile (\char -> char == ' ')
        |. Parser.chompWhile (\char -> char == '\n')


{-| Parses a block of frontmatter into a dictionary of keys and their multiline values.
-}
frontMatterItem : ( Dict String String, Maybe Cursor ) -> Parser.Parser (Parser.Step ( Dict String String, Maybe Cursor ) (Dict String String))
frontMatterItem ( captured, maybeCursor ) =
    Parser.oneOf
        [ Parser.succeed (Parser.Done captured)
            |. Parser.oneOf
                [ Parser.end
                , Parser.succeed ()
                    |. Parser.chompIf (\char -> char == '-')
                    |. Parser.chompWhile (\char -> char == '-')
                    |. Parser.chompWhile (\char -> char == '\n')
                ]
        , Parser.succeed
            (\keyOrContentWhoKnows value ->
                case keyOrContentWhoKnows of
                    Key key ->
                        Parser.Loop ( Dict.insert key value captured, Just ( key, value ) )

                    Content startingContent ->
                        case maybeCursor of
                            Just ( key, content ) ->
                                Parser.Loop
                                    ( Dict.insert key (content ++ "\n" ++ startingContent) captured
                                    , Just ( key, value )
                                    )

                            Nothing ->
                                Parser.Loop ( captured, maybeCursor )
            )
            |= keyOrContent
            |= Parser.getChompedString
                (Parser.chompWhile (\char -> char /= '\n'))
            |. Parser.chompWhile (\char -> char == '\n')
        , --
          Parser.succeed (Parser.Done captured)
        ]
