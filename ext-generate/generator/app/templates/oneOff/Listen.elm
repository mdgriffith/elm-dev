port module Listen.{{name}} exposing (..)

{-| Listen for messages from JS.

Once you've added this module to your app, you can send messages to your Elm App by
adding something like the following to your main.ts


    // Connect to a WebSocket
    const socket = new WebSocket("ws://localhost:8080");
    // When a message comes into our WebSocket, we pass the message along
    // to the {{name_decapitalized}} port.
    socket.addEventListener("message", function(event) {
        app.ports.{{name_decapitalized}}.send(event.data);
    });


-}

import Json.Decode
import Platform.Sub
import Listen


port {{name_decapitalized}} : (Json.Decode.Value -> msg) -> Platform.Sub.Sub msg


listen :
    { decoder : Json.Decode.Decoder msg
    , onError : { value : Json.Decode.Value, error : Json.Decode.Error } -> msg
    }
    -> Listen.Listen msg
listen options =
    Listen.OnFromJs
        { portName = "{{name_decapitalized}}"
        , subscribe = {{name_decapitalized}}
        , receive =
            \value ->
                case Json.Decode.decodeValue options.decoder value of
                    Ok msg ->
                        msg

                    Err error ->
                        options.onError { value = value, error = error }
        }
