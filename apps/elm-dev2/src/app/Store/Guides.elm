module Store.Guides exposing
    ( store
    , Model, Msg(..)
    , lookup
    )

{-|

@docs store

@docs Model, Msg

@docs lookup

-}

import App.Store
import Dict
import Effect
import Json.Decode
import Json.Encode
import Listen


type alias Model =
    { guides : Dict.Dict String Guide }


type alias Guide =
    { path : String
    , content : String
    }


type Msg
    = GuideReceived Guide


lookup : List String -> Model -> Maybe Guide
lookup path_ model =
    let
        path =
            String.join "/" path_
    in
    Dict.get path model.guides


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
                                { guides = Dict.empty }
                in
                ( model
                , Effect.none
                )
        , update =
            \msg model ->
                case msg of
                    GuideReceived guide ->
                        ( { model
                            | guides =
                                Dict.insert guide.path guide model.guides
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
        [ ( "guides"
          , Json.Encode.dict identity encodeGuide model.guides
          )
        ]


encodeGuide : Guide -> Json.Encode.Value
encodeGuide guide =
    Json.Encode.object
        [ ( "path", Json.Encode.string guide.path )
        , ( "content", Json.Encode.string guide.content )
        ]


decoder : Json.Decode.Decoder Model
decoder =
    Json.Decode.map Model
        (Json.Decode.field "guides" (Json.Decode.dict guideDecoder))


guideDecoder : Json.Decode.Decoder Guide
guideDecoder =
    Json.Decode.map2 Guide
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "content" Json.Decode.string)
