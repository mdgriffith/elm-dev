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
        Html.div []
            [ viewTitle
            , viewBody
            , viewProjects stores.projects
            ]
    }


viewProjects : Store.Projects.Model -> Html.Html Msg
viewProjects project =
    case Dict.values project.projects of
        [] ->
            Html.text "No projects"

        projects ->
            Html.div []
                (List.map (viewProject project.current) projects)


viewProject : Maybe String -> Data.ProjectStatus.Project -> Html.Html Msg
viewProject current project =
    Html.div [ Attr.style "display" "flex", Attr.style "gap" "10px" ]
        [ Html.span [] [ Html.text project.root ]
        , viewStatus project.status
        , if current == Just project.root then
            Html.span [] [ Html.text "Current" ]

          else
            Html.text "bg"
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
