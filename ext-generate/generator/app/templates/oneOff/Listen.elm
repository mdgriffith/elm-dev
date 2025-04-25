module Listen.{{name}} exposing (..)

{-|



-}

import Json.Decode
import Json.Encode
import Platform.Sub
import Sub


port {{name_decapitalized}}Updated : (Json.Encode.Value -> msg) -> Platform.Sub.Sub msg


listen :
    { key : String
    , decoder : Json.Decode.Decoder msg
    }
    -> Sub.Sub msg
listen options =
    Sub.OnFromJs
        { portName = "{{name_decapitalized}}Updated"
        , subscription =
            {{name_decapitalized}}Updated
                (Json.Decode.decodeValue options.decoder)
        }
