port module Effect.{{name}} exposing (send)

{-| -}

import Effect
import Json.Encode as Json


port {{name}} : Json.Encode.Value -> Cmd msg


send : String -> Json.Value -> Effect.Effect msg
send operation value =
    Effect.SendToWorld
        { toPort = {{name}}
        , portName = "{{name}}"
        , payload =
            Json.object
                [ ( "operation", Json.string operation )
                , ( "details", value )
                ]
        }
