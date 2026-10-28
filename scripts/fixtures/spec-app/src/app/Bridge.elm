port module Bridge exposing (listen, send)

import Effect
import Json.Decode as Decode
import Json.Encode as Encode
import Listen


port bridgeOut : Encode.Value -> Cmd msg


port bridgeIn : (Decode.Value -> msg) -> Sub msg


send : String -> Effect.Effect msg
send name =
    Effect.SendToWorld
        { toPort = bridgeOut
        , portName = "bridgeOut"
        , payload = Encode.object [ ( "name", Encode.string name ), ( "requestId", Encode.string "captured-id" ) ]
        }


listen :
    { decoder : Decode.Decoder msg
    , onError : { value : Decode.Value, error : Decode.Error } -> msg
    }
    -> Listen.Listen msg
listen options =
    Listen.OnFromJs
        { portName = "bridgeIn"
        , subscribe = bridgeIn
        , receive =
            \value ->
                case Decode.decodeValue options.decoder value of
                    Ok msg ->
                        msg

                    Err error ->
                        options.onError { value = value, error = error }
        }
