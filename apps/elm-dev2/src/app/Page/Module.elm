module Page.Module exposing (page, Model, Msg)

{-|

@docs page, Model, Msg

-}

import App.Page
import App.Page.Id
import App.Route
import App.Stores
import App.View
import App.View.Region
import Broadcast
import Data.ProjectStatus as ProjectStatus
import Docs.Ref.Get
import Effect exposing (Effect)
import Elm.Docs
import Html
import Html.Attributes as Attr
import Listen exposing (Listen)
import Store.Modules
import Store.Projects
import String
import Ui.Module
import Ui.Nav.Top
import WebComponents.Elm


{-| -}
type alias Model =
    { module_ : Store.Modules.Module
    }


{-| -}
type Msg
    = TypeClicked String


page : App.Page.Page App.Stores.Stores App.Page.Id.Module_Params Msg Model
page =
    App.Page.page
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : App.Page.Id.Id -> App.Page.Id.Module_Params -> App.Stores.Stores -> Maybe Model -> App.Page.Init Msg Model
init pageId params stores maybeCached =
    case Store.Modules.lookup params.path_ stores.modules of
        Just module_ ->
            App.Page.init { module_ = module_ }

        Nothing ->
            App.Page.notFound


update : App.Stores.Stores -> Msg -> Model -> ( Model, Effect Msg )
update stores msg model =
    case msg of
        TypeClicked name ->
            ( model
            , case Docs.Ref.Get.lookup name stores of
                Nothing ->
                    Effect.none

                Just ref ->
                    Effect.broadcast (Broadcast.RefPinned ref)
            )


subscriptions : App.Stores.Stores -> Model -> Listen Msg
subscriptions shared model =
    Listen.none


view : App.View.Region.Id -> App.Stores.Stores -> Model -> App.View.View Msg
view viewId shared model =
    { title = model.module_.info.name
    , body =
        let
            maybeProject =
                case shared.projects.current of
                    Just shortId ->
                        Store.Projects.lookup shortId shared.projects

                    Nothing ->
                        Nothing
        in
        Html.div []
            [ Ui.Nav.Top.view
                { title = model.module_.info.name
                , back =
                    case maybeProject of
                        Just project ->
                            App.Route.toString (App.Route.Project { projectid = String.fromInt project.shortId })

                        Nothing ->
                            "/"
                , status = ProjectStatus.NoData
                }
            , Ui.Module.view
                { onClick = Just TypeClicked }
                model.module_.info
            , let
                maybeRoot =
                    case shared.projects.current of
                        Just shortId ->
                            Store.Projects.lookup shortId shared.projects
                                |> Maybe.map .projectRoot

                        Nothing ->
                            Nothing

                moduleFilepath =
                    "src/"
                        ++ (String.split "." model.module_.info.name
                                |> String.join "/"
                           )
                        ++ ".elm"
              in
              case maybeRoot of
                Nothing ->
                    Html.text ""

                Just root ->
                    Html.div
                        [ Attr.style "margin-top" "16px"
                        , Attr.style "height" "480px"
                        , Attr.style "border" "1px solid #e5e7eb"
                        , Attr.style "border-radius" "8px"
                        , Attr.style "overflow" "hidden"
                        ]
                        [ WebComponents.Elm.elm
                            { baseUrl = "http://localhost:1420"
                            , filepath = moduleFilepath
                            , cwd = root
                            }
                        ]
            ]
    }

