module Ui.Nav exposing (view)

import App.Route
import Data.ProjectStatus
import Html exposing (Html, i)
import Html.Attributes as Attr
import Theme
import Ui.Attr


padding : Int
padding =
    24


width : Int
width =
    256


view : { project : Data.ProjectStatus.Project } -> Html msg
view options =
    Html.nav
        [ Ui.Attr.width width
        , Ui.Attr.widthMax width
        , Ui.Attr.noShrink
        , Ui.Attr.noGrow
        , Ui.Attr.borderBox
        , heightWindow
        ]
        [ Theme.column.lg
            [ Attr.style "position" "fixed"
            , heightWindow
            , Ui.Attr.pad padding
            , Ui.Attr.width width
            , Ui.Attr.widthMax width
            , Ui.Attr.borderBox
            , Ui.Attr.scrollbars
            , Attr.class "navbar"
            ]
            [ -- Project home link
              Html.a
                [ Attr.href
                    (App.Route.toString
                        (App.Route.Project { projectid = String.fromInt options.project.shortId })
                    )
                , Attr.style "display" "block"
                , Attr.style "font-weight" "600"
                , Attr.style "margin-bottom" "0.75rem"
                ]
                [ Html.text "Overview" ]
            , viewSection "Guides"
                (List.map viewGuidePath options.project.docs.guides)
            , viewSection "Modules"
                (List.map viewModuleName options.project.docs.modules)
            , viewSection "Packages"
                (List.map viewPackageInfo options.project.docs.packages)
            ]
        ]


viewModuleName : String -> Html msg
viewModuleName name =
    let
        path_ =
            if String.contains "." name then
                String.split "." name

            else
                String.split "/" name
    in
    Html.a
        [ Attr.href
            (App.Route.toString
                (App.Route.Module { path_ = path_ })
            )
        , Ui.Attr.ellipsis
        , Ui.Attr.width (width - (padding * 2))
        , Attr.style "display" "inline-block"
        ]
        [ Html.text name ]


viewPackageInfo : Data.ProjectStatus.PackageInfo -> Html msg
viewPackageInfo package =
    Html.a
        [ Attr.href
            (App.Route.toString
                (App.Route.Package { path_ = String.split "/" package.name })
            )
        , Ui.Attr.ellipsis
        , Ui.Attr.width (width - (padding * 2))
        , Attr.style "display" "inline-block"
        ]
        [ Html.text (package.name ++ " @" ++ package.version) ]


heightWindow =
    Attr.style "height" "100vh"


sectionHeader : String -> Html msg
sectionHeader title =
    Html.h2
        [ Ui.Attr.pad 0
        , Attr.style "font-size" "1rem"
        , Attr.style "font-weight" "bold    "
        ]
        [ Html.text title ]


sectionSubheader : String -> Html msg
sectionSubheader title =
    Html.h3
        [ Ui.Attr.pad 0
        , Attr.style "font-size" "0.9rem"
        , Attr.style "padding-top" "0.4rem"
        , Attr.style "width" "100%"
        , Attr.style "display" "flex"
        , Attr.style "gap" "0.5rem"
        , Attr.style "flex-direction" "row"

        -- small caps
        , Attr.style "font-variant" "small-caps"
        ]
        [ Html.span
            [ Attr.style "flex-shrink" "0"
            , Attr.style "opacity" "0.5"
            ]
            [ Html.text title ]

        -- divider line
        , Html.hr
            [ Attr.style "height" "1px"
            , Attr.style "background-color" "#eee"
            , Attr.style "width" "100%"
            , Attr.style "border" "none"
            , Attr.style "top" "3px"
            , Attr.style "position" "relative"
            ]
            []
        ]


viewSection : String -> List (Html msg) -> Html msg
viewSection title items =
    if List.isEmpty items then
        Html.text ""

    else
        Theme.column.sm []
            [ sectionHeader title
            , Theme.column.sm [] items
            ]


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain


viewGuidePath : String -> Html msg
viewGuidePath path =
    let
        pieces =
            String.split "/" path

        name =
            List.reverse pieces
                |> List.head
                |> Maybe.withDefault path
                |> String.split "."
                |> List.head
                |> Maybe.withDefault path
                |> capitalize
                |> String.replace "-" " "
                |> String.replace "_" " "
    in
    Html.a
        [ Attr.href
            (App.Route.toString
                (App.Route.Guide
                    { path_ =
                        String.split "/"
                            path
                    }
                )
            )
        , Ui.Attr.ellipsis
        , Attr.style "padding-left" "0.5rem"
        , Ui.Attr.width (width - (padding * 2))
        , Attr.style "display" "inline-block"
        ]
        [ Html.text name ]

