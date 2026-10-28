module Support.BridgeSpec exposing (exchange)

import Json.Decode as Decode
import Json.Encode as Encode
import Spec


exchange name =
    Spec.outgoingPort
        { name = "bridgeOut"
        , description = "bridge submits " ++ name
        , decoder = Decode.map2 Tuple.pair (Decode.field "name" Decode.string) (Decode.field "requestId" Decode.string)
        , matches = \( submitted, _ ) -> submitted == name
        , then_ =
            \( submitted, requestId ) ->
                Spec.incomingPort "bridgeIn"
                    (Encode.object [ ( "name", Encode.string submitted ), ( "requestId", Encode.string requestId ) ])
        }
