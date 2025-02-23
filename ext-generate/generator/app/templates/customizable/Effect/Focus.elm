port module Effect.Focus exposing (focus, blur, select)

{-| Focus a text input and select the text.

@docs focus, blur, select

-}

import Browser.Dom
import Effect
import Json.Encode as Json


{-| Attempt to change the browser focus to the element with a given id.
-}
focus : String -> (Result Browser.Dom.Error () -> msg) -> Effect.Effect msg
focus =
    Effect.Focus


{-| Make a specific element lose focus.
-}
blur : String -> (Result Browser.Dom.Error () -> msg) -> Effect.Effect msg
blur =
    Effect.Blur


port textSelection : Json.Value -> Cmd msg


{-| Give the id of the text element you want to focus and select the contents of.
-}
select : String -> Effect.Effect msg
select id =
    Effect.SendToWorld
        { toPort = textSelection
        , portName = "textSelection"
        , payload =
            Json.object [ ( "id", Json.string id ) ]
        }
