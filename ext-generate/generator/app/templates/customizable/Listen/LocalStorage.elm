port module Listen.LocalStorage exposing (onUpdated)

{-|

@docs onUpdated

-}

import Json.Decode
import Json.Encode
import Listen
import Platform.Sub


port localStorageUpdated : (Json.Encode.Value -> msg) -> Platform.Sub.Sub msg


onUpdated :
    { key : String
    , decoder : Json.Decode.Decoder msg
    , onError : { value : Json.Decode.Value, error : Json.Decode.Error } -> msg
    }
    -> Listen.Listen msg
onUpdated options =
    Listen.OnFromJs
        { portName = "localStorageUpdated"
        , subscribe = localStorageUpdated
        , receive =
            \value ->
                case Json.Decode.decodeValue options.decoder value of
                    Ok msg ->
                        msg

                    Err error ->
                        options.onError { value = value, error = error }
        }
