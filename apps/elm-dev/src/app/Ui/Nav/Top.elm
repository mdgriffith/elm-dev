module Ui.Nav.Top exposing
    ( view
    , statusSummary
    )

{-| Reusable top navigation bar shown on project-related pages.

@docs view

-}

import Data.ProjectStatus as ProjectStatus
import Html exposing (Html)
import Html.Attributes as Attr
import String


view : { title : String, back : String, status : ProjectStatus.Status } -> Html msg
view params =
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "space-between"
        , Attr.style "gap" "12px"
        , Attr.style "padding" "8px 16px"
        , Attr.style "border-bottom" "1px solid #e5e7eb"
        , Attr.style "position" "sticky"
        , Attr.style "top" "0"
        , Attr.style "background" "white"
        , Attr.style "z-index" "1"
        , Attr.style "color" "#111827"
        ]
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "8px"
            ]
            [ Html.a
                [ Attr.href params.back
                , Attr.style "text-decoration" "none"
                , Attr.style "display" "inline-flex"
                , Attr.style "align-items" "center"
                , Attr.style "gap" "6px"
                , Attr.style "padding" "6px 8px"
                , Attr.style "border-radius" "6px"
                , Attr.style "border" "1px solid #e5e7eb"
                , Attr.style "background" "#f9fafb"
                ]
                [ Html.text "‹"
                ]
            , Html.h1
                [ Attr.style "margin" "0"
                , Attr.style "font-size" "18px"
                , Attr.style "font-weight" "600"
                ]
                [ Html.text params.title ]
            ]
        , Html.div
            [ Attr.style "color" "#6b7280"
            , Attr.style "font-size" "12px"
            ]
            [ Html.text (statusSummary params.status) ]
        ]


statusSummary : ProjectStatus.Status -> String
statusSummary status =
    case status of
        ProjectStatus.NoData ->
            "Unknown"

        ProjectStatus.Success ->
            "0 errors"

        ProjectStatus.GlobalError _ ->
            "1 error"

        ProjectStatus.CompilerError record ->
            let
                total =
                    record.errors
                        |> List.map (\file -> List.length file.problem)
                        |> List.sum
            in
            String.fromInt total ++ " errors"

