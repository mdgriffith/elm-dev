module Broadcast exposing (Msg(..))

{-| This is a module that is special for elm-prefab.

The `Msg` here can be broadcasted globally to any page that is listening.

To broadcast a message, you'd do this in your `update` function:

    Effect.broadcast Broadcast.LogOut

And to listen for this message, in your subscriptions, you'd have

    Listen.onBroadcast
        (\broadcastMsg ->
            case broadcastMsg of
                Broadcast.LogOut ->
                    -- You can choose which messages you opt in to.
                    Nothing
        )

@docs Msg

-}


type Msg
    = LogOut
