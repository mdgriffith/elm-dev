module Store.Guides exposing
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
    { guides : List Guide }


type alias Guide =
    { path : String
    , content : String
    }


type Msg
    = GuideReceived Guide


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
                                { guides = [] }
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
                                if List.any (\g -> g.path == guide.path) model.guides then
                                    List.map
                                        (\g ->
                                            if g.path == guide.path then
                                                guide

                                            else
                                                g
                                        )
                                        model.guides

                                else
                                    guide
                                        :: model.guides
                                        |> List.sortBy .path
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
          , Json.Encode.list encodeGuide model.guides
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
        (Json.Decode.field "guides" (Json.Decode.list guideDecoder))


guideDecoder : Json.Decode.Decoder Guide
guideDecoder =
    Json.Decode.map2 Guide
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "content" Json.Decode.string)
