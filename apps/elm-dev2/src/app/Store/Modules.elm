module Store.Modules exposing
    ( store
    , Model, Msg(..)
    , Module, lookup, getByName
    , requestMissingForProject
    )

{-|

@docs store

@docs Model, Msg

@docs Module, lookup, getByName
@docs requestMissingForProject

-}

import App.Store
import Data.ProjectStatus as ProjectStatus
import Dict
import Docs.Ref
import Effect
import Effect.Ask
import Elm.Docs
import Elm.Module
import Elm.Package
import Json.Decode
import Json.Encode
import Listen
import Listen.DevServer


type alias Model =
    -- The key is the module name
    { modules : Dict.Dict String Module }


type alias Module =
    { location : Docs.Ref.Location
    , info : Elm.Docs.Module
    }


type Msg
    = ModulesReceived Docs.Ref.Location (List Elm.Docs.Module)
    | IgnoreDevServer String


lookup : List String -> Model -> Maybe Module
lookup path_ model =
    let
        path =
            String.join "." path_
    in
    Dict.get path model.modules


getByName : Elm.Module.Name -> Model -> Maybe Module
getByName name model =
    Dict.get (Elm.Module.toString name) model.modules


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
                                { modules = Dict.empty }
                in
                ( model
                , Effect.none
                )
        , update =
            \msg model ->
                case msg of
                    ModulesReceived location newModules ->
                        ( { model
                            | modules =
                                List.foldl
                                    (\m acc ->
                                        -- Overwrite the module if it already exists
                                        Dict.insert m.name
                                            { info = m
                                            , location = location
                                            }
                                            acc
                                    )
                                    model.modules
                                    newModules
                          }
                        , Effect.none
                        )

                    IgnoreDevServer _ ->
                        ( model, Effect.none )
        , subscriptions =
            \_ ->
                Listen.DevServer.listen
                    (\event ->
                        case event of
                            Listen.DevServer.PackageUpdated pkg ->
                                ModulesReceived (Docs.Ref.Package pkg.name) pkg.modules

                            Listen.DevServer.ModuleLocalUpdated { filepath, modul } ->
                                ModulesReceived (Docs.Ref.LocalFile filepath) [ modul ]

                            _ ->
                                IgnoreDevServer "non-package event"
                    )
        }


{-| For a given project, request any modules that are not already present in the modules store.
-}
requestMissingForProject : ProjectStatus.Project -> Model -> Effect.Effect msg
requestMissingForProject project model =
    let
        missingPaths : List String
        missingPaths =
            project.modules
                |> List.filter
                    (\mi ->
                        Dict.member mi.name model.modules
                            |> not
                    )
                |> List.map .path
    in
    missingPaths
        |> List.map (\file -> Effect.Ask.moduleRequested { dir = project.projectRoot, file = file })
        |> Effect.batch

