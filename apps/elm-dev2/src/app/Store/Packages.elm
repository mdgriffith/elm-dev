module Store.Packages exposing
    ( store
    , Model, Msg(..)
    )

{-|

@docs store

@docs Model, Msg

-}

import App.Store
import Effect
import Elm.Docs
import Elm.Package
import Elm.Project
import Json.Decode
import Json.Encode
import Listen


type alias Model =
    -- The key is the package name as a string
    { packages : Dict String Package }


type alias Package =
    { readme : Maybe String
    , info : Elm.Project.PackageInfo
    }


type Msg
    = PackagesReceived (List Package)


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
                        [ ( "readme", Json.Encode.maybe Json.Encode.string pkg.readme )
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
                    (Json.Decode.field "readme" (Json.Decode.maybe Json.Decode.string))
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
                        Json.Decode.succeed pkg.info

                    _ ->
                        Json.Decode.fail "Expected a package"
            )
