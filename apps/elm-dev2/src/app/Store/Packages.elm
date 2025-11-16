module Store.Packages exposing
    ( store
    , Model, Msg(..)
    , lookup, getExposed, Package
    , requestMissingForProject
    )

{-|

@docs store

@docs Model, Msg

@docs lookup, getExposed, Package
@docs requestMissingForProject

-}

import App.Store
import Data.ProjectStatus as ProjectStatus
import Dict
import Effect
import Effect.Ask
import Elm.Docs
import Elm.Module
import Elm.Package
import Elm.Project
import Json.Decode
import Json.Encode
import Listen
import Listen.DevServer


type alias Model =
    -- The key is the package name as a string
    { packages : Dict.Dict String Package }


type alias Package =
    { readme : Maybe String
    , name : Elm.Package.Name
    , info : Elm.Project.PackageInfo
    , version : String
    }


type Msg
    = PackagesReceived (List Package)
    | PackageUpdated Listen.DevServer.Package
    | IgnoreDevServer String


getExposed : Package -> List Elm.Module.Name
getExposed pkg =
    case pkg.info.exposed of
        Elm.Project.ExposedList exposed ->
            exposed

        Elm.Project.ExposedDict assocList ->
            List.concatMap Tuple.second assocList


lookup : List String -> Model -> Maybe Package
lookup path_ model =
    let
        path =
            String.join "/" path_
    in
    Dict.get path model.packages


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
                                { packages = Dict.empty }
                in
                ( model
                , Effect.none
                )
        , update =
            \msg model ->
                case msg of
                    PackagesReceived packages ->
                        ( { model
                            | packages =
                                Dict.fromList
                                    (List.map
                                        (\pkg ->
                                            ( Elm.Package.toString pkg.info.name, pkg )
                                        )
                                        packages
                                    )
                          }
                        , Effect.none
                        )

                    PackageUpdated pkg ->
                        case pkg.info of
                            Elm.Project.Package info ->
                                ( { model
                                    | packages =
                                        model.packages
                                            |> Dict.insert (Elm.Package.toString info.name)
                                                { readme = pkg.readme
                                                , name = info.name
                                                , version = pkg.version
                                                , info = info
                                                }
                                  }
                                , Effect.none
                                )

                            _ ->
                                ( model, Effect.none )

                    IgnoreDevServer _ ->
                        ( model, Effect.none )
        , subscriptions =
            \_ ->
                Listen.DevServer.listen
                    (\event ->
                        case event of
                            Listen.DevServer.PackageUpdated pkg ->
                                PackageUpdated pkg

                            _ ->
                                IgnoreDevServer "non-package event"
                    )
        }
        |> App.Store.withLocalStorage
            { decoder = decoder
            , encode = encode
            }


{-| For a given project, request any direct dependencies (including test direct)
that are not already present in the packages store.
-}
requestMissingForProject : ProjectStatus.Project -> Model -> Effect.Effect msg
requestMissingForProject project model =
    let
        deps : List ProjectStatus.PackageInfo
        deps =
            project.dependencies.direct ++ project.testDependencies.direct

        missing : List ProjectStatus.PackageInfo
        missing =
            deps
                |> List.filter
                    (\pkg ->
                        let
                            path_ =
                                String.split "/" pkg.name
                        in
                        lookup path_ model
                            |> (==) Nothing
                    )
    in
    Effect.batch (List.map Effect.Ask.packageRequested missing)


encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "packages"
          , Json.Encode.dict identity
                (\pkg ->
                    Json.Encode.object
                        [ ( "readme"
                          , case pkg.readme of
                                Nothing ->
                                    Json.Encode.null

                                Just readme ->
                                    Json.Encode.string readme
                          )
                        , ( "info", Elm.Project.encode (Elm.Project.Package pkg.info) )
                        ]
                )
                model.packages
          )
        ]


decoder : Json.Decode.Decoder Model
decoder =
    Json.Decode.map Model
        (Json.Decode.field "packages"
            (Json.Decode.dict
                (Json.Decode.map4 Package
                    (Json.Decode.field "readme" (Json.Decode.nullable Json.Decode.string))
                    (Json.Decode.field "name" Elm.Package.decoder)
                    (Json.Decode.field "info" decodePackage)
                    (Json.Decode.field "version" Json.Decode.string)
                )
            )
        )


decodePackage : Json.Decode.Decoder Elm.Project.PackageInfo
decodePackage =
    Elm.Project.decoder
        |> Json.Decode.andThen
            (\proejct ->
                case proejct of
                    Elm.Project.Package pkg ->
                        Json.Decode.succeed pkg

                    _ ->
                        Json.Decode.fail "Expected a package"
            )

