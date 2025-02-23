module App.Store exposing
    ( Store, store
    , withLocalStorage
    )

{-|

@docs Store, store

@docs withLocalStorage

-}

import Effect
import Json.Decode as Decode
import Json.Encode as Json
import Listen
import Url


{-| -}
type alias Store msg model =
    { init : Json.Value -> Url.Url -> Maybe model -> ( model, Effect.Effect msg )
    , update : msg -> model -> ( model, Effect.Effect msg )
    , subscriptions : model -> Listen.Listen msg
    , codec :
        Maybe
            { decoder : Decode.Decoder model
            , encode : model -> Json.Value
            }
    }


{-| -}
store :
    { init : Json.Value -> Url.Url -> Maybe model -> ( model, Effect.Effect msg )
    , update : msg -> model -> ( model, Effect.Effect msg )
    , subscriptions : model -> Listen.Listen msg
    }
    -> Store msg model
store options =
    { init = options.init
    , update = options.update
    , subscriptions = options.subscriptions
    , codec = Nothing
    }


{-| -}
withLocalStorage :
    { decoder : Decode.Decoder model
    , encode : model -> Json.Value
    }
    -> Store msg model
    -> Store msg model
withLocalStorage codec res =
    { res | codec = Just codec }
