module Ui.Nav exposing (view)

import App.Route
import Docs.Guides
import Docs.Modules
import Docs.Packages
import Elm.Docs
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


view : {} -> Html msg
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
            [ viewSection "Guides"
                (Docs.Guides.all_
                    |> List.foldl gatherGuideGroups
                        { label = [], rendered = [] }
                    |> .rendered
                    |> List.reverse
                )
            , viewSection "Modules"
                (List.map viewModules Docs.Modules.modules)
            , viewSection "Packages"
                (List.map viewPackage Docs.Packages.directory)
            ]
        ]


gatherGuideGroups :
    { content : String, path : String }
    ->
        { label : List String
        , rendered : List (Html msg)
        }
    ->
        { label : List String
        , rendered : List (Html msg)
        }
gatherGuideGroups guide { label, rendered } =
    let
        pieces =
            String.split "/" guide.path

        groupStack =
            pieces
                -- Drop 'guides' at the beginning
                |> List.drop 1
                |> List.reverse
                -- Drop the filename
                |> List.drop 1
                |> List.reverse
                |> List.map
                    (\inner ->
                        inner
                            |> capitalize
                            |> String.replace "-" " "
                            |> String.replace "_" " "
                    )

        newHeaders =
            headerDiff label groupStack
                |> List.reverse
    in
    { label = groupStack
    , rendered =
        case newHeaders of
            [] ->
                viewGuide guide :: rendered

            _ ->
                viewGuide guide :: List.map sectionSubheader newHeaders ++ rendered
    }


headerDiff : List String -> List String -> List String
headerDiff existing new =
    case existing of
        [] ->
            new

        top :: remain ->
            case new of
                [] ->
                    []

                topNew :: remainingNew ->
                    if top == topNew then
                        headerDiff remain remainingNew

                    else
                        new


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
        Theme.column.sm2 []
            [ sectionHeader title
            , Theme.column.sm3 [] items
            ]


viewModules : Elm.Docs.Module -> Html msg
viewModules module_ =
    Html.a
        [ Attr.href
            (App.Route.toString
                (App.Route.Module
                    { path_ =
                        String.split "/"
                            module_.name
                    }
                )
            )
        , Ui.Attr.ellipsis
        , Ui.Attr.width (width - (padding * 2))
        , Attr.style "display" "inline-block"
        ]
        [ Html.text module_.name ]


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain


viewGuide : { path : String, content : String } -> Html msg
viewGuide guide =
    let
        pieces =
            String.split "/" guide.path

        name =
            List.reverse pieces
                |> List.head
                |> Maybe.withDefault guide.path
                |> String.split "."
                |> List.head
                |> Maybe.withDefault guide.path
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
                            guide.path
                    }
                )
            )
        , Ui.Attr.ellipsis
        , Attr.style "padding-left" "0.5rem"
        , Ui.Attr.width (width - (padding * 2))
        , Attr.style "display" "inline-block"
        ]
        [ Html.text name ]


viewPackage : { name : String, modules : List Elm.Docs.Module } -> Html msg
viewPackage package =
    Html.div []
        [ Html.a
            [ Attr.href
                (App.Route.toString
                    (App.Route.Package { path_ = String.split "/" package.name })
                )
            , Ui.Attr.ellipsis
            , Ui.Attr.width (width - (padding * 2))
            , Attr.style "display" "inline-block"
            ]
            [ Html.text package.name ]
        ]
