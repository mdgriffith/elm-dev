module Store.Projects exposing
    ( store
    , Model, Msg
    , lookup, view
    )

{-|

@docs store

@docs Model, Msg

-}

import App.Store
import Broadcast
import Data.Editor
import Data.ProjectStatus
import Data.Question
import Dict
import Effect
import Effect.Ask
import Elm.Module
import Elm.Project
import Html
import Http
import Json.Decode
import Json.Encode
import Listen
import Listen.DevServer


type alias ShortId =
    Int


type alias Model =
    { current : Maybe ShortId
    , projects : Dict.Dict ShortId Data.ProjectStatus.Project
    , base : Maybe String
    , sessions : Dict.Dict String Int
    , editorsOpen : Dict.Dict String Int
    }


type Msg
    = ProjectReceived Data.ProjectStatus.Project
    | ProjectSelected ShortId
    | ProjectListReceived (Result Http.Error Data.Question.ProjectList)
    | DevServerReceived Listen.DevServer.Event


store : App.Store.Store Msg Model
store =
    App.Store.store
        { init =
            \flags url maybeCachedModel ->
                let
                    model =
                        -- `maybeCachedModel` is the model from localstorage
                        -- If `App.Store.withLocalStorage` is defined
                        -- and it's available
                        maybeCachedModel
                            |> Maybe.withDefault
                                { current = Nothing
                                , projects = Dict.empty
                                , base = Nothing
                                , sessions = Dict.empty
                                , editorsOpen = Dict.empty
                                }
                in
                ( model
                , Effect.none
                )
        , update =
            \msg model ->
                case msg of
                    ProjectListReceived result ->
                        case result of
                            Ok status ->
                                ( { model
                                    | projects =
                                        Dict.fromList
                                            (List.map
                                                (\project ->
                                                    ( project.shortId
                                                    , project
                                                    )
                                                )
                                                status.projects
                                            )
                                    , current =
                                        -- Select the first project as the current one unless one is already selected
                                        List.foldl
                                            (\project found ->
                                                case found of
                                                    Just _ ->
                                                        found

                                                    Nothing ->
                                                        Just project.shortId
                                            )
                                            model.current
                                            status.projects
                                  }
                                , Effect.none
                                )

                            Err _ ->
                                ( model
                                , Effect.none
                                )

                    ProjectReceived project ->
                        ( { model
                            | projects = Dict.insert project.shortId project model.projects
                            , current =
                                case model.current of
                                    Just current ->
                                        model.current

                                    Nothing ->
                                        Just project.shortId
                          }
                        , Effect.none
                        )

                    ProjectSelected path ->
                        ( { model | current = Just path }
                        , Effect.none
                        )

                    DevServerReceived event ->
                        case event of
                            Listen.DevServer.ServerStatusUpdated { status } ->
                                case status of
                                    Listen.DevServer.Connected info ->
                                        ( model
                                        , Effect.Ask.projectList
                                        )

                                    _ ->
                                        ( model, Effect.none )

                            Listen.DevServer.ProjectsStatusUpdated projects ->
                                ( { model
                                    | projects =
                                        Dict.fromList
                                            (List.map
                                                (\project -> ( project.shortId, project ))
                                                (Debug.log "ProjectsStatusUpdated" projects)
                                            )
                                  }
                                , Effect.none
                                )

                            Listen.DevServer.ServiceStatusUpdated { sessions, editorsOpen } ->
                                ( { model
                                    | sessions = sessions
                                    , editorsOpen = editorsOpen
                                  }
                                , Effect.none
                                )

                            _ ->
                                ( model, Effect.none )
        , subscriptions =
            \_ ->
                Listen.batch
                    [ Listen.DevServer.listen DevServerReceived
                    , Listen.onBroadcast
                        (\broadcastMsg ->
                            case broadcastMsg of
                                Broadcast.ProjectSelected shortId ->
                                    Just (ProjectSelected shortId)

                                _ ->
                                    Nothing
                        )
                    ]
        }


lookup : ShortId -> Model -> Maybe Data.ProjectStatus.Project
lookup shortId model =
    Dict.get shortId model.projects


view : Model -> Html.Html msg
view model =
    Html.div []
        [ Html.h2 [] [ Html.text "Projects" ]
        , Html.ul []
            (model.projects
                |> Dict.values
                |> List.map (\project -> Html.li [] [ Html.text project.root ])
            )
        ]

