module Page.Project exposing (page, Model, Msg)

{-|

@docs page, Model, Msg

-}

import App.Page
import App.Page.Id
import App.Route
import App.Stores
import App.View
import App.View.Region
import Data.ProjectStatus as ProjectStatus
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes as Attr
import Listen exposing (Listen)
import Store.Projects
import String
import Ui
import Ui.Nav


{-| -}
type alias Model =
    { project : ProjectStatus.Project }


{-| -}
type Msg
    = ReplaceMe


page : App.Page.Page App.Stores.Stores App.Page.Id.Project_Params Msg Model
page =
    App.Page.page
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : App.Page.Id.Id -> App.Page.Id.Project_Params -> App.Stores.Stores -> Maybe Model -> App.Page.Init Msg Model
init _ params stores _ =
    case String.toInt params.projectid of
        Just shortId ->
            Store.Projects.lookup shortId stores.projects
                |> Maybe.map (\p -> App.Page.init { project = p })
                |> Maybe.withDefault App.Page.notFound

        Nothing ->
            App.Page.notFound


update : App.Stores.Stores -> Msg -> Model -> ( Model, Effect Msg )
update _ _ model =
    ( model, Effect.none )


subscriptions : App.Stores.Stores -> Model -> Listen Msg
subscriptions _ _ =
    Listen.none


view : App.View.Region.Id -> App.Stores.Stores -> Model -> App.View.View Msg
view _ _ model =
    let
        project =
            model.project

        projectName =
            project.name
    in
    { title = projectName
    , body =
        Ui.column []
            [ viewTopBar project
            , Ui.row [ Attr.style "width" "100%" ]
                [ -- left nav (fixed)
                  Ui.Nav.view { project = project }
                , -- main content with left gutter to clear fixed nav
                  Html.div
                    [ Attr.style "padding" "16px"
                    , Attr.style "box-sizing" "border-box"
                    , Attr.style "flex-grow" "1"
                    ]
                    [ viewDetailsCard project
                    ]
                ]
            ]
    }


viewTopBar : ProjectStatus.Project -> Html Msg
viewTopBar project =
    let
        backHref =
            -- Fall back to root if Home route constructor is not available here
            "/"
    in
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
                [ Attr.href backHref
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
                [ Html.text project.name ]
            ]
        , Html.div
            [ Attr.style "color" "#6b7280"
            , Attr.style "font-size" "12px"
            ]
            [ Html.text (statusSummary project.status) ]
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


viewDetailsCard : ProjectStatus.Project -> Html Msg
viewDetailsCard project =
    Html.div
        [ Attr.style "margin-top" "16px"
        , Attr.style "border" "1px solid #e5e7eb"
        , Attr.style "border-radius" "8px"
        , Attr.style "padding" "12px"
        ]
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "justify-content" "space-between"
            , Attr.style "align-items" "center"
            , Attr.style "margin-bottom" "8px"
            ]
            [ Html.div
                [ Attr.style "font-weight" "600"
                , Attr.style "font-size" "14px"
                ]
                [ Html.text "Details" ]
            , Html.div
                [ Attr.style "color" "#6b7280"
                , Attr.style "font-size" "12px"
                ]
                [ Html.text (statusSummary project.status) ]
            ]
        , Html.div
            [ Attr.style "margin-bottom" "8px"
            , Attr.style "opacity" "0.8"
            , Attr.style "font-size" "12px"
            ]
            [ Html.text ("Root: " ++ project.projectRoot) ]
        , viewErrorList project
        ]


viewErrorList : ProjectStatus.Project -> Html Msg
viewErrorList project =
    case project.status of
        ProjectStatus.Success ->
            Html.text ""

        ProjectStatus.NoData ->
            Html.div
                [ Attr.style "color" "#9ca3af"
                , Attr.style "font-size" "12px"
                ]
                [ Html.text "Compilation status unknown." ]

        ProjectStatus.GlobalError ge ->
            Html.div []
                [ Html.div
                    [ Attr.style "font-weight" "600"
                    , Attr.style "margin" "8px 0 4px 0"
                    ]
                    [ Html.text "Errors" ]
                , Html.ul []
                    [ Html.li []
                        [ Html.span [ Attr.style "opacity" "0.7" ]
                            [ Html.text (Maybe.withDefault "(global)" ge.path) ]
                        , Html.span [ Attr.style "margin-left" "8px" ]
                            [ Html.text ge.problem.title ]
                        ]
                    ]
                ]

        ProjectStatus.CompilerError record ->
            let
                items =
                    record.errors
                        |> List.concatMap
                            (\file ->
                                List.map
                                    (\problem ->
                                        Html.li []
                                            [ Html.span [ Attr.style "opacity" "0.7" ]
                                                [ Html.text (relativePath project.projectRoot file.path) ]
                                            , Html.span [ Attr.style "margin-left" "8px" ]
                                                [ Html.text problem.title ]
                                            ]
                                    )
                                    file.problem
                            )
            in
            Html.div []
                [ Html.div
                    [ Attr.style "font-weight" "600"
                    , Attr.style "margin" "8px 0 4px 0"
                    ]
                    [ Html.text "Errors" ]
                , Html.ul [] items
                ]


relativePath : String -> String -> String
relativePath root fullPath =
    let
        withSlash =
            if String.endsWith "/" root then
                root

            else
                root ++ "/"
    in
    if String.startsWith withSlash fullPath then
        String.dropLeft (String.length withSlash) fullPath

    else
        fullPath

