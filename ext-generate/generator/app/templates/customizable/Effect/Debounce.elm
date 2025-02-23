module Effect.Debounce exposing
    ( init, send
    , update, Model, Msg
    )

{-|

@docs init, send

@docs update, Model, Msg

-}

import Effect exposing (Effect)
import Time


send : msg -> Model msg -> Effect (Msg msg)
send msg (Model model) =
    case model.state of
        NothingToSend ->
            Effect.now (Start msg)

        WaitingToSend _ previouslySent ->
            if msg == previouslySent then
                Effect.none

            else
                Effect.now (Start msg)


type Model msg
    = Model
        { state : State msg
        , delay : Float
        }


type State msg
    = NothingToSend
    | WaitingToSend Time.Posix msg


init : Float -> Model msg
init milliseconds =
    Model
        { state = NothingToSend
        , delay = milliseconds
        }


update : (Msg msg -> msg) -> Msg msg -> Model msg -> ( Model msg, Effect msg )
update toMsg msg (Model model) =
    let
        paddedDelay =
            model.delay + 10
    in
    case msg of
        Start msgToSend sentAt ->
            case model.state of
                NothingToSend ->
                    ( Model { model | state = WaitingToSend sentAt msgToSend }
                    , Effect.map toMsg (waitAndCheck paddedDelay)
                    )

                WaitingToSend _ previousMsg ->
                    if msgToSend == previousMsg then
                        ( Model model, Effect.none )

                    else
                        ( Model { model | state = WaitingToSend sentAt msgToSend }
                        , Effect.map toMsg (waitAndCheck paddedDelay)
                        )

        TimeReceived now ->
            case model.state of
                NothingToSend ->
                    ( Model { model | state = NothingToSend }
                    , Effect.none
                    )

                WaitingToSend startedAt sendableMsg ->
                    let
                        millisSinceStart =
                            Time.posixToMillis now - Time.posixToMillis startedAt
                    in
                    if toFloat millisSinceStart >= model.delay then
                        ( Model { model | state = NothingToSend }
                        , Effect.sendMsg sendableMsg
                        )

                    else
                        ( Model model
                        , Effect.map toMsg (waitAndCheck paddedDelay)
                        )


waitAndCheck : Float -> Effect (Msg msg)
waitAndCheck millis =
    Effect.nowAfter millis TimeReceived


type Msg msg
    = TimeReceived Time.Posix
    | Start msg Time.Posix
