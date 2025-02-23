module Store.{{name}} exposing
    ( store
    , Model, Msg(..)
    )

{-|

@docs store

@docs Model, Msg

-}

import App.Store
import Effect
import Json.Decode
import Json.Encode
import Listen


type alias Model =
    {}


type Msg
    = ReplaceMe


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
                                {}
                in
                ( model
                , Effect.none
                )
        , update =
            \msg model ->
                ( model, Effect.none )
        , subscriptions = \_ -> Listen.none
        }
        |> App.Store.withLocalStorage
            { decoder = decoder
            , encode = encode
            }


encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object []


decoder : Json.Decode.Decoder Model
decoder =
    Json.Decode.succeed {}
