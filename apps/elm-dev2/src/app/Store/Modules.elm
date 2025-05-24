module Store.Modules exposing
    ( store
    , Model, Msg(..)
    , Module, lookup, getByName
    )

{-|

@docs store

@docs Model, Msg

@docs Module, lookup, getByName

-}

import App.Store
import Dict
import Effect
import Elm.Docs
import Elm.Module
import Elm.Package
import Elm.Type
import Json.Decode
import Json.Encode
import Listen


type alias Model =
    -- The key is the module name
    { modules : Dict.Dict String Module }


type alias Module =
    { package : Elm.Package.Name
    , info : Elm.Docs.Module
    }


type Msg
    = ModulesReceived Elm.Package.Name (List Elm.Docs.Module)


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
                    ModulesReceived packageName newModules ->
                        ( { model
                            | modules =
                                List.foldl
                                    (\m acc ->
                                        -- Overwrite the module if it already exists
                                        Dict.insert m.name
                                            { info = m
                                            , package = packageName
                                            }
                                            acc
                                    )
                                    model.modules
                                    newModules
                          }
                        , Effect.none
                        )
        , subscriptions = \_ -> Listen.none
        }
