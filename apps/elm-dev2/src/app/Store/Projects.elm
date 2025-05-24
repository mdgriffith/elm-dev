module Store.Projects exposing
    ( store
    , Model, Msg(..)
    )

{-|

@docs store

@docs Model, Msg

-}

import App.Store
import Dict
import Effect
import Elm.Module
import Elm.Project
import Json.Decode
import Json.Encode
import Listen


type alias Path =
    String


type alias Model =
    { current : Maybe Path
    , projects : Dict.Dict Path Project
    }


type alias Project =
    { root : Path
    , info : Elm.Project.ApplicationInfo
    , localModules : List Elm.Module.Name
    }


type Msg
    = ProjectReceived Project
    | ProjectSelected Path


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
                , Effect.none
                )
        , update =
            \msg model ->
                case msg of
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
        , subscriptions = \_ -> Listen.none
        }
        |> App.Store.withLocalStorage
            { decoder = decoder
            , encode = encode
            }


encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "current"
          , case model.current of
                Nothing ->
                    Json.Encode.null

                Just current ->
                    Json.Encode.string current
          )
        , ( "projects", Json.Encode.dict identity encodeProject model.projects )
        ]


encodeProject : Project -> Json.Encode.Value
encodeProject project =
    Json.Encode.object
        [ ( "root", Json.Encode.string project.root )
        , ( "info", Elm.Project.encode (Elm.Project.Application project.info) )
        , ( "localModules", Json.Encode.list Elm.Module.encode project.localModules )
        ]


decoder : Json.Decode.Decoder Model
decoder =
    Json.Decode.map2 Model
        (Json.Decode.field "current" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "projects" (Json.Decode.dict projectDecoder))


projectDecoder : Json.Decode.Decoder Project
projectDecoder =
    Json.Decode.map3 Project
        (Json.Decode.field "root" Json.Decode.string)
        (Json.Decode.field "info" applicationInfoDecoder)
        (Json.Decode.field "localModules" (Json.Decode.list Elm.Module.decoder))


applicationInfoDecoder : Json.Decode.Decoder Elm.Project.ApplicationInfo
applicationInfoDecoder =
    Elm.Project.decoder
        |> Json.Decode.andThen
            (\project ->
                case project of
                    Elm.Project.Application info ->
                        Json.Decode.succeed info

                    _ ->
                        Json.Decode.fail "Expected an application project"
            )
