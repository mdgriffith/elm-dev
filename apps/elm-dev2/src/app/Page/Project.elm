module Page.Project exposing (page, Model, Msg)

{-|

@docs page, Model, Msg

-}

import App.Page
import App.Page.Id
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
        projectName =
            model.project.name
    in
    { title = projectName
    , body =
        Html.div []
            [ Html.h1 []
                [ Html.text projectName ]
            , Html.div [ Attr.style "opacity" "0.6", Attr.style "font-size" "12px" ]
                [ Html.text ("Root: " ++ model.project.projectRoot) ]
            ]
    }

