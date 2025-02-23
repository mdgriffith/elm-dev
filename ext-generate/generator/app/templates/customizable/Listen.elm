module Listen exposing
    ( Listen(..)
    , none, batch
    , onBroadcast
    , onKeyPress
    , onEvery
    , onResize
    , map, toSubscription
    , broadcastListeners
    )

{-|


# Listeners

@docs Listen

@docs none, batch

@docs onBroadcast

@docs onKeyPress

@docs onEvery

@docs onResize

@docs map, toSubscription

@docs broadcastListeners

-}

import Broadcast
import Browser.Events
import Json.Decode
import Platform.Sub
import Time


type Listen msg
    = None
    | Batch (List (Listen msg))
      -- Common subscriptions
    | Every Float (Time.Posix -> msg)
    | OnWindowResize (Int -> Int -> msg)
    | OnKeyPress
        { ctrl : Bool
        , shift : Bool
        , key : String
        }
        msg
    | OnBroadcast (Broadcast.Msg -> Maybe msg)
      --
    | OnFromJs
        { portName : String
        , subscription : Platform.Sub.Sub (Result Json.Decode.Error msg)
        }


{-| -}
none : Listen msg
none =
    None


{-| -}
batch : List (Listen msg) -> Listen msg
batch =
    Batch


{-| -}
onKeyPress : { ctrl : Bool, shift : Bool, key : String } -> msg -> Listen msg
onKeyPress options msg =
    OnKeyPress options msg


{-| -}
onEvery : Float -> (Time.Posix -> msg) -> Listen msg
onEvery ms toMsg =
    Every ms toMsg


{-| -}
onResize : (Int -> Int -> msg) -> Listen msg
onResize msg =
    OnWindowResize msg


{-| -}
onBroadcast : (Broadcast.Msg -> Maybe msg) -> Listen msg
onBroadcast toMsg =
    OnBroadcast toMsg


{-| -}
map : (a -> b) -> Listen a -> Listen b
map func sub =
    case sub of
        None ->
            None

        Batch subs ->
            Batch (List.map (map func) subs)

        Every ms toMsg ->
            Every ms (func << toMsg)

        OnKeyPress options msg ->
            OnKeyPress options (func msg)

        OnWindowResize msg ->
            OnWindowResize (\w h -> func <| msg w h)

        OnBroadcast toMsg ->
            OnBroadcast (Maybe.map func << toMsg)

        OnFromJs fromJs ->
            OnFromJs
                { portName = fromJs.portName
                , subscription =
                    Sub.map (Result.map func) fromJs.subscription
                }


{-| -}
toSubscription : { ignore : String -> msg } -> Listen msg -> Platform.Sub.Sub msg
toSubscription options sub =
    case sub of
        None ->
            Platform.Sub.none

        Batch subs ->
            Platform.Sub.batch (List.map (toSubscription options) subs)

        Every ms toMsg ->
            Time.every ms toMsg

        OnWindowResize toMsg ->
            Browser.Events.onResize toMsg

        OnKeyPress keyOptions msg ->
            Browser.Events.onKeyDown
                (Json.Decode.map4
                    (\_ ctrl shift meta ->
                        { ctrl = ctrl
                        , shift = shift
                        , meta = meta
                        }
                    )
                    (Json.Decode.field "key" Json.Decode.string
                        |> Json.Decode.andThen
                            (\key ->
                                if String.toLower key == String.toLower keyOptions.key then
                                    Json.Decode.succeed True

                                else
                                    Json.Decode.fail "Not a match"
                            )
                    )
                    (Json.Decode.field "ctrlKey" Json.Decode.bool)
                    (Json.Decode.field "shiftKey" Json.Decode.bool)
                    (Json.Decode.field "metaKey" Json.Decode.bool)
                    |> Json.Decode.andThen
                        (\event ->
                            -- accept both "meta" (Cmd on macs)
                            --  and "ctrl"
                            if (keyOptions.ctrl == event.ctrl || keyOptions.ctrl == event.meta) && keyOptions.shift == event.shift then
                                Json.Decode.succeed msg

                            else
                                Json.Decode.fail "Not a match"
                        )
                )

        OnBroadcast toMsg ->
            -- This isn't handled like a normal subscription
            -- We use `broadcastListeners` to handle this
            Sub.none

        OnFromJs fromJs ->
            fromJs.subscription
                |> Sub.map
                    (\result ->
                        case result of
                            Ok success ->
                                success

                            Err err ->
                                options.ignore (Json.Decode.errorToString err)
                    )


{-| You shouldn't need to use this directly, it's used by some code that elm-prefab generates.
-}
broadcastListeners : Broadcast.Msg -> Listen msg -> List msg
broadcastListeners broadcastMsg sub =
    case sub of
        None ->
            []

        Batch subs ->
            List.concatMap (broadcastListeners broadcastMsg) subs

        Every _ _ ->
            []

        OnWindowResize _ ->
            []

        OnKeyPress _ _ ->
            []

        OnBroadcast toMsg ->
            case toMsg broadcastMsg of
                Just msg ->
                    [ msg ]

                Nothing ->
                    []

        OnFromJs _ ->
            []
