port module Effect.Ask exposing (projectList)

{-|

@docs projectList

-}

import Effect
import Json.Encode as Encode


port ask : Encode.Value -> Cmd msg


{-| Ask the host to fetch the project list.

The host (TS/Tauri) is responsible for resolving the base URL and will
return the data via the `devServer` port with a `{"msg":"Status"}` payload.
-}
projectList : Effect.Effect msg
projectList =
    Effect.SendToWorld
        { toPort = ask
        , portName = "ask"
        , payload =
            Encode.object
                [ ( "route", Encode.string "ProjectList" )
                ]
        }
