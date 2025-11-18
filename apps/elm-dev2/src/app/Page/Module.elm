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
import Docs.Ref
import Docs.Ref.Get
import Effect exposing (Effect)
import Effect.Ask
import Elm.Docs
import Html
import Html.Attributes as Attr
import Listen exposing (Listen)
import Listen.DevServer
import Store.Modules
import Store.Projects
import String
import Ui.Module
import Ui.Nav.Top
import WebComponents.Playground


{-| -}
type alias Model =
    { module_ : Store.Modules.Module
    , interactiveExamples : Maybe (List Listen.DevServer.InteractiveExample)
    }


{-| -}
type Msg
    = TypeClicked String
    | DevServerReceived Listen.DevServer.Event


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
            let
                maybeProject =
                    case stores.projects.current of
                        Just shortId ->
                            Store.Projects.lookup shortId stores.projects

                        Nothing ->
                            Nothing

                maybeEffect =
                    case ( maybeProject, module_.location ) of
                        ( Just project, Docs.Ref.LocalFile path ) ->
                            Just (Effect.Ask.interactiveExamples { dir = project.projectRoot, file = path })

                        _ ->
                            Nothing
            in
            case maybeEffect of
                Just eff ->
                    App.Page.initWith
                        { module_ = module_
                        , interactiveExamples = Nothing
                        }
                        eff

                Nothing ->
                    App.Page.init
                        { module_ = module_
                        , interactiveExamples = Nothing
                        }

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

        DevServerReceived event ->
            case event of
                Listen.DevServer.InteractiveExamplesUpdated { file, examples } ->
                    let
                        matchesThisModule =
                            case model.module_.location of
                                Docs.Ref.LocalFile path ->
                                    path == file

                                Docs.Ref.Package _ ->
                                    False
                    in
                    if Debug.log "PLS" matchesThisModule then
                        ( { model | interactiveExamples = Just examples }, Effect.none )

                    else
                        ( model, Effect.none )

                _ ->
                    ( model, Effect.none )


subscriptions : App.Stores.Stores -> Model -> Listen Msg
subscriptions shared model =
    Listen.DevServer.listen DevServerReceived


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
                    Maybe.map .projectRoot maybeProject
              in
              case ( maybeRoot, shared.devServer.base ) of
                ( Just root, Just baseUrl ) ->
                    case model.interactiveExamples of
                        Just (first :: _) ->
                            Html.div
                                [ Attr.style "margin-top" "16px"
                                , Attr.style "height" "480px"
                                , Attr.style "border" "1px solid #e5e7eb"
                                , Attr.style "border-radius" "8px"
                                , Attr.style "overflow" "hidden"
                                ]
                                [ WebComponents.Playground.playground
                                    { baseUrl = baseUrl
                                    , projectRoot = root
                                    , elmSource = first.elmSource
                                    , filePath = first.path
                                    }
                                ]

                        _ ->
                            Html.text ""

                _ ->
                    Html.text ""
            ]
    }

