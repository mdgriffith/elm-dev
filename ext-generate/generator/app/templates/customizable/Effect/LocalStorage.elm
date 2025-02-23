port module Effect.LocalStorage exposing (clear, save)

{-|

@docs clear, save

-}

import Effect
import Json.Encode as Json


port localStorage : Json.Value -> Cmd msg


{-| -}
save : String -> Json.Value -> Effect.Effect msg
save key value =
    send "save"
        (Json.object
            [ ( "key", Json.string key )
            , ( "value", value )
            ]
        )


{-| -}
clear : String -> Effect.Effect msg
clear key =
    send "clear"
        (Json.object
            [ ( "key", Json.string key )
            ]
        )


send : String -> Json.Value -> Effect.Effect msg
send operation value =
    Effect.SendToWorld
        { toPort = localStorage
        , portName = "local-storage"
        , payload =
            Json.object
                [ ( "operation", Json.string operation )
                , ( "details", value )
                ]
        }
