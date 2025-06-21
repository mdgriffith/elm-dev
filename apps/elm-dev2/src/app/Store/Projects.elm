module Store.Projects exposing
    ( store
    , Model, Msg
    )

{-|

@docs store

@docs Model, Msg

-}

import App.Store
import Data.Editor
import Data.ProjectStatus
import Data.Question
import Dict
import Effect
import Elm.Module
import Elm.Project
import Http
import Json.Decode
import Json.Encode
import Listen
import Listen.DevServer


type alias Path =
    String


type alias Model =
    { current : Maybe Path
    , projects : Dict.Dict Path Data.ProjectStatus.Project
    }



-- type alias Project =
--     { root : Path
--     , projectRoot : String
--     , status : Status
--     , entrypoints : List String
--     , info : Elm.Project.ApplicationInfo
--     , localModules : List Elm.Module.Name
--     }


type Msg
    = ProjectReceived Data.ProjectStatus.Project
    | ProjectSelected Path
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
                                }
                in
                ( model
                , Data.Question.projectList ProjectListReceived
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
                                                    ( project.root
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
                                                        Just project.root
                                            )
                                            model.current
                                            status.projects
                                  }
                                , Effect.none
                                )

                            Err _ ->
                                ( model, Effect.none )

                    ProjectReceived project ->
                        ( { model
                            | projects = Dict.insert project.root project model.projects
                            , current =
                                case model.current of
                                    Just current ->
                                        model.current

                                    Nothing ->
                                        Just project.root
                          }
                        , Effect.none
                        )

                    ProjectSelected path ->
                        ( { model | current = Just path }
                        , Effect.none
                        )

                    DevServerReceived event ->
                        case event of
                            Listen.DevServer.ProjectsStatusUpdated projects ->
                                ( { model
                                    | projects =
                                        Dict.fromList
                                            (List.map
                                                (\project -> ( project.root, project ))
                                                projects
                                            )
                                  }
                                , Effect.none
                                )

                            _ ->
                                ( model, Effect.none )
        , subscriptions =
            \_ ->
                Listen.DevServer.listen DevServerReceived
        }
