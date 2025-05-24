module Ui.Module exposing (view, viewBlock)

import Elm.Docs
import Elm.Type
import Html exposing (..)
import Html.Attributes as Attr
import Theme
import Theme.Color
import Ui.Markdown
import Ui.Syntax
import Ui.Type


type alias Options msg =
    { onClick : Maybe (TypeName -> msg)
    }


type alias TypeName =
    String


view : Options msg -> Elm.Docs.Module -> Html msg
view options mod =
    Theme.column.lg []
        [ Html.h2 [] [ Html.text mod.name ]
        , Theme.column.lg
            []
            (mod
                |> Elm.Docs.toBlocks
                |> List.map
                    (viewBlock mod.name options)
            )
        ]


viewBlock : ModuleName -> Options msg -> Elm.Docs.Block -> Html msg
viewBlock moduleName options block =
    case block of
        Elm.Docs.MarkdownBlock markdown ->
            viewMarkdown markdown

        Elm.Docs.UnionBlock details ->
            Html.div []
                [ viewUnionDefinition moduleName options details
                , viewMarkdown details.comment
                ]

        Elm.Docs.AliasBlock details ->
            Html.div []
                [ viewAliasDefinition moduleName options details
                , viewMarkdown details.comment
                ]

        Elm.Docs.ValueBlock details ->
            Html.div []
                [ viewValueDefinition moduleName options details
                , viewMarkdown details.comment
                ]

        Elm.Docs.BinopBlock details ->
            Html.div []
                [ viewName details.name
                , viewMarkdown details.comment
                ]

        Elm.Docs.UnknownBlock text ->
            Html.text text


viewMarkdown : String -> Html msg
viewMarkdown comment =
    if String.startsWith "#" (String.trim comment) then
        Ui.Markdown.view comment

    else
        Html.div [ Attr.style "padding-left" "3rem" ]
            [ Ui.Markdown.view comment ]


type alias ModuleName =
    String


code : List (Html msg) -> Html msg
code content =
    Html.pre
        [ Attr.style "line-height" lineHeight
        , Theme.Color.backgroundDefault
        , Theme.Color.textDefault
        , Attr.style "border" "none"
        ]
        [ Html.code [ Attr.style "line-height" lineHeight ] content ]


viewAliasDefinition : ModuleName -> Options msg -> Elm.Docs.Alias -> Html msg
viewAliasDefinition modName options details =
    code
        (Html.span [ Ui.Syntax.keyword ] [ Html.text "type alias " ]
            :: Html.span [] [ Html.text (details.name ++ " ") ]
            :: Html.span [] (List.map (\v -> Html.text (v ++ " ")) details.args)
            :: Html.span [] [ Html.text "= " ]
            :: Ui.Type.viewWithIndent
                { currentModule = Just modName
                , onClick = options.onClick
                }
                5
                details.tipe
        )


viewUnionDefinition : ModuleName -> Options msg -> Elm.Docs.Union -> Html msg
viewUnionDefinition modName options details =
    code
        [ Html.span [ Ui.Syntax.keyword ] [ Html.text "type " ]
        , Html.span [] [ Html.text (details.name ++ " ") ]
        , Html.span [] (List.map (\v -> Html.text (v ++ " ")) details.args)
        , Html.span []
            (List.foldl
                (\( tagName, pieces ) ( isFirst, gathered ) ->
                    let
                        divider =
                            if isFirst then
                                "="

                            else
                                "|"

                        isMultiline =
                            List.any Ui.Type.shouldBeMultiline pieces
                    in
                    ( False
                    , Html.span []
                        [ Html.span [ Ui.Syntax.keyword ] [ Html.text ("\n    " ++ divider) ]
                        , Html.text (" " ++ tagName ++ " ")
                        , Html.span []
                            (List.concatMap
                                (\tipe ->
                                    let
                                        lineIsMulti =
                                            Ui.Type.shouldBeMultiline tipe

                                        needsParens =
                                            Ui.Type.needsParens tipe
                                    in
                                    if needsParens then
                                        if isMultiline then
                                            let
                                                end =
                                                    if lineIsMulti then
                                                        "\n         )"

                                                    else
                                                        ")"
                                            in
                                            Html.text "\n         ("
                                                :: Ui.Type.viewWithIndent
                                                    { currentModule = Just modName
                                                    , onClick = options.onClick
                                                    }
                                                    9
                                                    tipe
                                                ++ [ Html.text end ]

                                        else
                                            Html.text "("
                                                :: Ui.Type.view
                                                    { currentModule = Just modName
                                                    , onClick = options.onClick
                                                    }
                                                    tipe
                                                ++ [ Html.text ") " ]

                                    else if isMultiline then
                                        Html.text "          "
                                            :: Ui.Type.viewWithIndent
                                                { currentModule = Just modName
                                                , onClick = options.onClick
                                                }
                                                9
                                                tipe

                                    else
                                        Ui.Type.viewWithIndent
                                            { currentModule = Just modName
                                            , onClick = options.onClick
                                            }
                                            9
                                            tipe
                                            ++ [ Html.text " " ]
                                )
                                pieces
                            )
                        ]
                        :: gathered
                    )
                )
                ( True, [] )
                details.tags
                |> Tuple.second
                |> List.reverse
            )
        ]


viewName : String -> Html msg
viewName name =
    Html.h2 []
        [ Html.text name
        ]


lineHeight : String
lineHeight =
    "1.5"


viewValueDefinition : ModuleName -> Options msg -> { docs | name : String, tipe : Elm.Type.Type } -> Html msg
viewValueDefinition moduleName options details =
    let
        isMultiline =
            Ui.Type.shouldBeMultiline details.tipe
    in
    code
        (Html.text
            (details.name
                ++ " : "
                ++ (if isMultiline then
                        "\n    "

                    else
                        ""
                   )
            )
            :: Ui.Type.viewWithIndent
                { currentModule = Just moduleName
                , onClick = options.onClick
                }
                6
                details.tipe
        )
