module Page.Home exposing (page, Model, Msg)

{-|

@docs page, Model, Msg

-}

import App.Page
import App.Page.Id
import App.Route
import App.Stores
import App.View
import App.View.Region
import Data.ProjectStatus
import Dict
import Effect exposing (Effect)
import Html
import Html.Attributes as Attr
import Listen exposing (Listen)
import Store.Projects
import String


{-| -}
type alias Model =
    {}


{-| -}
type Msg
    = ReplaceMe


page : App.Page.Page App.Stores.Stores App.Page.Id.Home_Params Msg Model
page =
    App.Page.page
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : App.Page.Id.Id -> App.Page.Id.Home_Params -> App.Stores.Stores -> Maybe Model -> App.Page.Init Msg Model
init pageId params shared maybeCached =
    App.Page.init {}


update : App.Stores.Stores -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    ( model, Effect.none )


subscriptions : App.Stores.Stores -> Model -> Listen Msg
subscriptions shared model =
    Listen.none


view : App.View.Region.Id -> App.Stores.Stores -> Model -> App.View.View Msg
view viewId stores model =
    { title = "Directory"
    , body =
        Html.div
            [ Attr.style "display" "grid"
            , Attr.style "grid-template-columns" "repeat(auto-fill, minmax(280px, 1fr))"
            , Attr.style "gap" "16px"
            ]
            (viewProjects stores.projects)
    }


viewProjects : Store.Projects.Model -> List (Html.Html Msg)
viewProjects project =
    case Dict.values project.projects of
        [] ->
            [ Html.text "No projects" ]

        projects ->
            List.map (viewProjectCard project project.current) projects


viewProjectCard : Store.Projects.Model -> Maybe Int -> Data.ProjectStatus.Project -> Html.Html Msg
viewProjectCard projectsModel current project =
    let
        projectName =
            project.name

        editingCount =
            projectsModel.editorsOpen
                |> Dict.toList
                |> List.filter (\( path, _ ) -> String.startsWith project.projectRoot path)
                |> List.length

        connectedCount =
            projectsModel.sessions
                |> Dict.values
                |> List.filter (\pid -> pid == project.shortId)
                |> List.length

        statusDotColor =
            case project.status of
                Data.ProjectStatus.Success ->
                    "#10b981"

                Data.ProjectStatus.NoData ->
                    "#9ca3af"

                _ ->
                    "#ef4444"

        href =
            App.Route.toString
                (App.Route.Project
                    { projectid = String.fromInt project.shortId }
                )
    in
    Html.a
        [ Attr.href href
        , Attr.style "display" "block"
        , Attr.style "text-decoration" "none"
        , Attr.style "color" "inherit"
        ]
        [ Html.div
            [ Attr.style "position" "relative"
            , Attr.style "border" "1px solid #e5e7eb"
            , Attr.style "border-radius" "8px"
            , Attr.style "padding" "4px 12px"
            ]
            [ Html.div
                [ Attr.style "position" "absolute"
                , Attr.style "top" "12px"
                , Attr.style "right" "12px"
                , Attr.style "width" "10px"
                , Attr.style "height" "10px"
                , Attr.style "border-radius" "50%"
                , Attr.style "background-color" statusDotColor
                ]
                []
            , Html.div
                [ Attr.style "font-weight" "600"
                , Attr.style "font-size" "16px"
                ]
                [ Html.text projectName ]
            , let
                renderCounts =
                    Html.div
                        [ Attr.style "display" "flex"
                        , Attr.style "justify-content" "flex-end"
                        , Attr.style "color" "#9ca3af"
                        , Attr.style "font-size" "12px"
                        ]
              in
              case ( editingCount, connectedCount ) of
                ( 0, 0 ) ->
                    Html.text ""

                ( e, 0 ) ->
                    renderCounts [ Html.text (String.fromInt e ++ " editing") ]

                ( 0, c ) ->
                    renderCounts [ Html.text (String.fromInt c ++ " connected") ]

                ( e, c ) ->
                    renderCounts
                        [ Html.text
                            (String.fromInt e
                                ++ " editing, "
                                ++ String.fromInt c
                                ++ " connected"
                            )
                        ]
            ]
        ]


viewStatus : Data.ProjectStatus.Status -> Html.Html Msg
viewStatus status =
    case status of
        Data.ProjectStatus.Success ->
            Html.span [] [ Html.text "Ok" ]

        Data.ProjectStatus.GlobalError _ ->
            Html.span [] [ Html.text "Error" ]

        Data.ProjectStatus.CompilerError _ ->
            Html.span [] [ Html.text "Error" ]

        Data.ProjectStatus.NoData ->
            Html.span [] [ Html.text "No data" ]


viewTitle : Html.Html Msg
viewTitle =
    Html.h1 [] [ Html.text "Packages" ]


viewBody =
    Html.div []
        [ Html.h1 [] [ Html.text "Application Packages" ]
        , Html.h1 [] [ Html.text "Library Packages" ]
        ]

