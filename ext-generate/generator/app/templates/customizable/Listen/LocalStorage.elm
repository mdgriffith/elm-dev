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
    }
    -> Listen.Listen msg
onUpdated options =
    Listen.OnFromJs
        { portName = "localStorageUpdated"
        , subscription =
            localStorageUpdated
                (Json.Decode.decodeValue options.decoder)
        }
