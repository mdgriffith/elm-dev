module Ui.Markdown exposing (view)

import Html exposing (Html)
import Html.Attributes
import Markdown.Parser
import Markdown.Renderer


view : String -> Html msg
view markdown =
    let
        parsedResult =
            Markdown.Parser.parse markdown
    in
    case parsedResult of
        Err _ ->
            Html.div
                [ Html.Attributes.class "markdown" ]
                [ Html.text "Error parsing markdown" ]

        Ok parsed ->
            Html.div
                [ Html.Attributes.class "markdown" ]
                (case Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer parsed of
                    Err errString ->
                        [ Html.text errString ]

                    Ok rendered ->
                        rendered
                )
