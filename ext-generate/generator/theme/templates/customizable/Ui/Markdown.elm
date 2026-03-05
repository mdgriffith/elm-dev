module Ui.Markdown exposing (parse, render, view)

import Char
import Html exposing (Html)
import Markdown.Block as Block
import Markdown.Parser
import Theme
import Theme.Color
import Theme.Color.Palette as Palette
import Theme.Text
import Ui
import Ui.Divider
import Ui.Font
import Ui.Prose
import Ui.Table


view : String -> Ui.Element msg
view src =
    case parse src of
        Ok blocks ->
            Ui.column [ Theme.gap 1 ]
                (List.map render blocks)

        Err _ ->
            Ui.text src


parse : String -> Result (List String) (List Block.Block)
parse src =
    Markdown.Parser.parse src
        |> Result.mapError (List.map Markdown.Parser.deadEndToString)


render : Block.Block -> Ui.Element msg
render block =
    case block of
        Block.Heading Block.H1 content ->
            Theme.Text.title [] (Block.extractInlineText content)

        Block.Heading Block.H2 content ->
            Theme.Text.title [] (Block.extractInlineText content)

        Block.Heading _ content ->
            Theme.Text.body [ Ui.Font.bold ] (Block.extractInlineText content)

        Block.Paragraph inlines ->
            paragraph []
                (List.map renderInline inlines)

        Block.HtmlBlock _ ->
            Ui.none

        Block.UnorderedList _ items ->
            Ui.column [ Theme.gap 1 ]
                (List.map
                    (\(Block.ListItem _ innerBlocks) ->
                        Ui.row [ Theme.gap 1 ]
                            [ Ui.text "-"
                            , paragraph [] (List.map render innerBlocks)
                            ]
                    )
                    items
                )

        Block.OrderedList _ startingIndex items ->
            Ui.column [ Theme.gap 1 ]
                (List.indexedMap
                    (\index innerBlocks ->
                        Ui.row [ Theme.gap 1 ]
                            [ Ui.text (String.fromInt (startingIndex + index) ++ ".")
                            , paragraph [] (List.map render innerBlocks)
                            ]
                    )
                    items
                )

        Block.CodeBlock codeBlock ->
            Ui.el
                [ Theme.pad 3
                , Ui.background Palette.neutral95
                , Theme.borderRadius.sm
                ]
                (Ui.text codeBlock.body)

        Block.ThematicBreak ->
            Ui.Divider.horizontal

        Block.BlockQuote nestedBlocks ->
            paragraph [ Theme.pad 2 ]
                (List.map render nestedBlocks)

        Block.Table headers rows ->
            let
                columns =
                    Ui.Table.columns
                        (List.indexedMap
                            (\index cell_ ->
                                let
                                    isNumeric =
                                        rows
                                            |> List.take 3
                                            |> List.any
                                                (isNumericText << Block.extractInlineText << getIndex index)
                                in
                                Ui.Table.column
                                    { header =
                                        header
                                            [ if isNumeric then
                                                Ui.alignRight

                                              else
                                                Ui.noAttr
                                            ]
                                            (Block.extractInlineText cell_.label)
                                    , view =
                                        \row ->
                                            let
                                                inlines =
                                                    getIndex index row
                                            in
                                            inlines
                                                |> List.map renderInline
                                                |> paragraph
                                                    [ if isNumeric then
                                                        Ui.alignRight

                                                      else
                                                        Ui.width Ui.fill
                                                    ]
                                                |> Ui.el [ Ui.width Ui.fill ]
                                                |> Ui.Table.cell
                                                    [ if index == 0 then
                                                        Ui.width (Ui.px 10)

                                                      else
                                                        Ui.noAttr
                                                    ]
                                    }
                                    |> Ui.Table.withWidth
                                        { fill = index /= 0
                                        , min = Nothing
                                        , max = Nothing
                                        }
                            )
                            headers
                        )
            in
            Ui.Table.view [ Ui.width Ui.fill ] columns rows


renderInline : Block.Inline -> Ui.Element msg
renderInline inline =
    case inline of
        Block.Strong innerInlines ->
            paragraph [ Ui.Font.weight Ui.Font.bold ]
                (List.map renderInline innerInlines)

        Block.Emphasis innerInlines ->
            paragraph [ Ui.Font.italic ]
                (List.map renderInline innerInlines)

        Block.Strikethrough innerInlines ->
            paragraph [ Ui.Font.strike ]
                (List.map renderInline innerInlines)

        Block.Image _ _ _ ->
            Ui.none

        Block.Text string ->
            Ui.text string

        Block.CodeSpan string ->
            Ui.el
                [ Theme.pad 1
                , Ui.background Palette.neutral95
                , Theme.borderRadius.sm
                ]
                (Ui.text string)

        Block.Link destination _ inlines ->
            Ui.el [ Ui.link destination ]
                (Ui.text (Block.extractInlineText inlines))

        Block.HardLineBreak ->
            Ui.el [ Ui.Font.exactWhitespace ]
                (Ui.html (Html.text "\n"))

        Block.HtmlInline _ ->
            Ui.none


isNumericText : String -> Bool
isNumericText str =
    String.any Char.isDigit str


getIndex : Int -> List (List thing) -> List thing
getIndex index items =
    case items of
        [] ->
            []

        first :: remaining ->
            if index <= 0 then
                first

            else
                getIndex (index - 1) remaining


header attrs content =
    Ui.Table.cell
        [ Theme.Color.borderDefault
        , Theme.borderWidthBottom 1
        , Theme.padXY 4 2
        , Ui.height Ui.fill
        ]
        (Ui.el [ Ui.width Ui.fill ]
            (Ui.el attrs (Ui.text content))
        )


paragraph attrs =
    Ui.Prose.paragraph (Ui.Font.lineHeight 1.4 :: attrs)
