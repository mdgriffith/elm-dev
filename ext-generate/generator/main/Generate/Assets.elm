module Generate.Assets exposing (frontmatterParser, generate, trimFrontMatter)

{-| -}

import Dict exposing (Dict)
import Elm
import Elm.Annotation as Type
import Elm.Arg
import Elm.Case
import Markdown.Block
import Markdown.Parser
import Options.Assets
import Parser exposing ((|.), (|=))
import Path


generate : List Options.Assets.AssetGroup -> List Elm.File
generate assetGroups =
    List.concatMap generateAssetGroup assetGroups


generateAssetGroup : Options.Assets.AssetGroup -> List Elm.File
generateAssetGroup group =
    assetRootFile
        :: List.filterMap identity
            [ generateAssetGroupDirectory group
            , generateAssetGroupSource group
            ]


assetRootFile : Elm.File
assetRootFile =
    Elm.file [ "Asset" ]
        [ Elm.customType "Src"
            [ Elm.variantWith
                "Src"
                [ Type.string
                ]
            ]
        , Elm.declaration "toString"
            (Elm.fn (Elm.Arg.varWith "src" (Type.named [] "Src"))
                (\src ->
                    Elm.Case.custom src
                        (Type.named [] "Src")
                        [ Elm.Case.branch
                            (Elm.Arg.customType "Src" identity
                                |> Elm.Arg.item (Elm.Arg.varWith "innerSrc" Type.string)
                            )
                            identity
                        ]
                )
            )
        , Elm.customType "Content"
            [ Elm.variantWith
                "Binary"
                []
            , Elm.variantWith
                "Text"
                []
            , Elm.variantWith
                "Markdown"
                [ Type.record
                    [ ( "title", Type.string )
                    , ( "frontmatter"
                      , Type.record
                            [ ( "source_", Type.string )
                            ]
                      )
                    , ( "headers"
                      , Type.list
                            (Type.record
                                [ ( "level"
                                  , Type.int
                                  )
                                , ( "text", Type.string )
                                ]
                            )
                      )
                    ]
                ]
            ]
        ]


generateAssetGroupSource : Options.Assets.AssetGroup -> Maybe Elm.File
generateAssetGroupSource group =
    let
        assetSources =
            List.filterMap toSourceDeclaration group.files
    in
    if List.isEmpty assetSources then
        Nothing

    else
        Just <|
            Elm.file [ "Assets", group.name, "Source" ]
                assetSources


toSourceDeclaration : Options.Assets.File -> Maybe Elm.Declaration
toSourceDeclaration file =
    case file.content of
        Options.Assets.Binary ->
            Nothing

        Options.Assets.Text source ->
            Elm.declaration (declarationName file)
                (Elm.string source)
                |> Elm.exposeConstructor
                |> Just


declarationName : Options.Assets.File -> String
declarationName file =
    case file.crumbs of
        [] ->
            toCamelCase file.name

        _ ->
            String.join "_" (List.map toCamelCase file.crumbs) ++ "_" ++ toCamelCase file.name


toCamelCase : String -> String
toCamelCase str =
    let
        parts =
            String.split "-" str
    in
    case parts of
        [] ->
            ""

        top :: tail ->
            decapitalize top
                ++ String.join ""
                    (List.map capitalize tail)


decapitalize : String -> String
decapitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toLower top ++ remain


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain


{-| The directory file contains

  - A top level entry for each file in the asset group.
  - A top level list called `all` that contains all the files in the asset group.

-}
generateAssetGroupDirectory : Options.Assets.AssetGroup -> Maybe Elm.File
generateAssetGroupDirectory group =
    let
        entries =
            List.map toDirectoryEntry group.files

        directory =
            let
                directoryItems =
                    List.map (toFileInfo group) group.files
            in
            case directoryItems of
                [] ->
                    []

                _ ->
                    [ Elm.declaration "directory_"
                        (Elm.list
                            (List.map
                                encodeFileInfo
                                directoryItems
                            )
                            |> Elm.withType
                                (Type.list
                                    (Type.record
                                        [ ( "name", Type.string )
                                        , ( "crumbs", Type.list Type.string )
                                        , ( "pathOnServer", Type.named [ "Asset" ] "Src" )
                                        , ( "content", Type.named [ "Asset" ] "Content" )
                                        ]
                                    )
                                )
                        )
                        |> Elm.exposeConstructor
                    ]
    in
    if List.isEmpty entries then
        Nothing

    else
        Just <|
            Elm.file [ "Assets", group.name ]
                (List.concat
                    [ entries
                    , directory
                    ]
                )


encodeFileInfo : FileInfo -> Elm.Expression
encodeFileInfo info =
    Elm.record
        [ ( "name", Elm.string info.name )
        , ( "crumbs", Elm.list (List.map Elm.string info.crumbs) )
        , ( "pathOnServer"
          , Elm.apply
                (Elm.value
                    { importFrom = [ "Asset" ]
                    , name = "Src"
                    , annotation = Just (Type.named [ "Asset" ] "Src")
                    }
                )
                [ Elm.string info.pathOnServer ]
          )
        , ( "content", encodeContent info.content )
        ]


encodeContent : Content -> Elm.Expression
encodeContent content =
    case content of
        Binary ->
            Elm.value
                { importFrom = [ "Asset" ]
                , name = "Binary"
                , annotation = Just (Type.named [ "Asset" ] "Content")
                }

        Text ->
            Elm.value
                { importFrom = [ "Asset" ]
                , name = "Text"
                , annotation = Just (Type.named [ "Asset" ] "Content")
                }

        Markdown { title, headers, frontmatter } ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Asset" ]
                    , name = "Markdown"
                    , annotation = Just (Type.named [ "Asset" ] "Content")
                    }
                )
                [ Elm.record
                    [ ( "title", Elm.string title )
                    , ( "headers", Elm.list (List.map encodeHeader headers) )
                    , ( "frontmatter"
                      , Elm.record
                            (( "source_", Elm.string frontmatter.source_ )
                                :: frontmatter.attrs
                            )
                      )
                    ]
                ]


encodeHeader : ( Int, String ) -> Elm.Expression
encodeHeader ( level, text ) =
    Elm.record
        [ ( "level", Elm.int level )
        , ( "text", Elm.string text )
        ]


type alias FileInfo =
    { name : String
    , crumbs : List String
    , pathOnServer : String
    , content : Content
    }


type Content
    = Binary
    | Text
    | Markdown
        { title : String
        , headers : List ( Int, String )
        , frontmatter :
            { source_ : String
            , attrs : List ( String, Elm.Expression )
            }
        }


toFileInfo : Options.Assets.AssetGroup -> Options.Assets.File -> FileInfo
toFileInfo group file =
    { name = file.name
    , crumbs = file.crumbs
    , pathOnServer = file.pathOnServer
    , content =
        case file.content of
            Options.Assets.Binary ->
                Binary

            Options.Assets.Text source ->
                let
                    ( _, ext ) =
                        Path.extension file.pathOnServer
                in
                if List.member ext [ "markdown", "md" ] then
                    let
                        headers =
                            getHeaders source
                    in
                    Markdown
                        { title =
                            List.head headers
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault file.name
                        , headers = headers
                        , frontmatter =
                            getFrontMatterSource group.fileInfo.markdown.frontmatter source
                        }

                else
                    Text
    }


getFrontMatterSource :
    Dict String String
    -> String
    ->
        { source_ : String
        , attrs : List ( String, Elm.Expression )
        }
getFrontMatterSource allowedAttrs source =
    case Parser.run frontmatterParser source of
        Err _ ->
            { source_ = ""
            , attrs = []
            }

        Ok frontmatter ->
            let
                attrDict =
                    Dict.fromList frontmatter.attrs
            in
            { source_ = trimFrontMatter frontmatter.source_
            , attrs =
                Dict.map
                    (\key _ ->
                        case Dict.get key attrDict of
                            Just value ->
                                value

                            Nothing ->
                                Elm.string ""
                    )
                    allowedAttrs
                    |> Dict.toList
            }


trimFrontMatter : String -> String
trimFrontMatter source =
    source
        |> dropWhileLeft (\c -> c == '-' || c == '\n')
        |> dropWhileRight (\c -> c == '-' || c == '\n' || c == '#')


dropWhileLeft : (Char -> Bool) -> String -> String
dropWhileLeft predicate string =
    case String.uncons string of
        Just ( c, rest ) ->
            if predicate c then
                dropWhileLeft predicate rest

            else
                string

        Nothing ->
            string


dropWhileRight : (Char -> Bool) -> String -> String
dropWhileRight predicate string =
    String.reverse (dropWhileLeft predicate (String.reverse string))


frontmatterParser :
    Parser.Parser
        { source_ : String
        , attrs : List ( String, Elm.Expression )
        }
frontmatterParser =
    Parser.mapChompedString
        (\source attrs ->
            { source_ = source
            , attrs = attrs
            }
        )
        frontmatterParserAttributes


frontmatterParserAttributes : Parser.Parser (List ( String, Elm.Expression ))
frontmatterParserAttributes =
    Parser.succeed identity
        |. Parser.chompWhile (\c -> c == '-' || c == '\n')
        |= Parser.loop []
            (\attrs ->
                Parser.oneOf
                    [ Parser.succeed (Parser.Done attrs)
                        |. Parser.chompIf (\c -> c == '-')
                        |. Parser.chompWhile (\c -> c == '-')
                        |. Parser.chompWhile (\c -> c == '\n')
                    , Parser.succeed (Parser.Loop attrs)
                        |. Parser.chompIf (\c -> c == '\n')
                    , Parser.succeed (Parser.Done attrs)
                        |. Parser.end
                    , Parser.succeed (Parser.Done attrs)
                        |. Parser.chompIf (\c -> c == '#')
                    , Parser.succeed
                        (\attrName content ->
                            Parser.Loop (( attrName, Elm.string (String.trim content) ) :: attrs)
                        )
                        |= (Parser.chompWhile (\c -> c /= ':' && c /= '\n')
                                |> Parser.getChompedString
                           )
                        |. Parser.spaces
                        |. Parser.chompWhile (\c -> c == ':')
                        |. Parser.spaces
                        |= indentedString
                    ]
            )


indentedString : Parser.Parser String
indentedString =
    Parser.loop ( True, "" )
        (\( isFirst, str ) ->
            Parser.oneOf
                [ if isFirst then
                    Parser.succeed
                        (\line -> Parser.Loop ( False, str ++ line ++ "\n" ))
                        |= (Parser.chompWhile (\c -> c /= '\n')
                                |> Parser.getChompedString
                           )
                        |. Parser.chompIf (\c -> c == '\n')

                  else
                    Parser.succeed
                        (\spaces line -> Parser.Loop ( False, str ++ spaces ++ line ++ "\n" ))
                        |. Parser.chompIf (\c -> c == ' ')
                        |= (Parser.chompWhile (\c -> c == ' ')
                                |> Parser.getChompedString
                           )
                        |= (Parser.chompWhile (\c -> c /= '\n')
                                |> Parser.getChompedString
                           )
                        |. Parser.chompIf (\c -> c == '\n')
                , Parser.succeed (Parser.Done ( False, str ))
                ]
        )
        |> Parser.map Tuple.second


{-| -}
toDirectoryEntry : Options.Assets.File -> Elm.Declaration
toDirectoryEntry file =
    Elm.declaration (declarationName file)
        (Elm.apply
            (Elm.value
                { importFrom = [ "Asset" ]
                , name = "Src"
                , annotation = Nothing
                }
            )
            [ Elm.string file.pathOnServer ]
            |> Elm.withType (Type.named [ "Asset" ] "Src")
        )
        |> Elm.expose


getHeaders : String -> List ( Int, String )
getHeaders src =
    case Markdown.Parser.parse src of
        Ok blocks ->
            List.filterMap
                (\block ->
                    case block of
                        Markdown.Block.Heading level contents ->
                            Just
                                ( Markdown.Block.headingLevelToInt level
                                , Markdown.Block.extractInlineText contents
                                )

                        _ ->
                            Nothing
                )
                blocks

        Err _ ->
            []
