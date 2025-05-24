module Store.Packages exposing
    ( store
    , Model, Msg(..)
    , lookup, getExposed, Package
    )

{-|

@docs store

@docs Model, Msg

@docs lookup, getExposed, Package

-}

import App.Store
import Dict
import Effect
import Elm.Docs
import Elm.Module
import Elm.Package
import Elm.Project
import Json.Decode
import Json.Encode
import Listen
import Store.Modules


type alias Model =
    -- The key is the package name as a string
    { packages : Dict.Dict String Package }


type alias Package =
    { readme : Maybe String
    , info : Elm.Project.PackageInfo
    }


type Msg
    = PackagesReceived (List Package)


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
        , subscriptions = \_ -> Listen.none
        }
        |> App.Store.withLocalStorage
            { decoder = decoder
            , encode = encode
            }


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
                (Json.Decode.map2 Package
                    (Json.Decode.field "readme" (Json.Decode.nullable Json.Decode.string))
                    (Json.Decode.field "info" decodePackage)
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
