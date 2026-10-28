module App.Spec.Listen exposing (simulate, timers)

import Json.Decode as Decode
import Listen exposing (Listen(..))
import ProgramTest exposing (SimulatedSub)
import SimulatedEffect.Ports as Ports
import SimulatedEffect.Sub as SimSub
import Time


simulate : { callbacks | ignore : String -> msg } -> Listen msg -> SimulatedSub msg
simulate callbacks listener =
    case listener of
        None ->
            SimSub.none

        Batch listeners ->
            SimSub.batch (List.map (simulate callbacks) listeners)

        OnFromJs incoming ->
            Ports.subscribe incoming.portName
                Decode.value
                incoming.receive

        OnWindowResize toMsg ->
            Ports.subscribe "__elm_spec_resize"
                (Decode.map2 toMsg (Decode.field "width" Decode.int) (Decode.field "height" Decode.int))
                identity

        OnKeyPress options msg ->
            Ports.subscribe "__elm_spec_keydown"
                (Decode.map4
                    (\key ctrl meta shift ->
                        if String.toLower key == String.toLower options.key && (options.ctrl == ctrl || options.ctrl == meta) && options.shift == shift then
                            Just msg

                        else
                            Nothing
                    )
                    (Decode.field "key" Decode.string)
                    (Decode.field "ctrlKey" Decode.bool)
                    (Decode.field "metaKey" Decode.bool)
                    (Decode.field "shiftKey" Decode.bool)
                )
                (Maybe.withDefault (callbacks.ignore "Keypress does not match this listener"))

        Every _ _ ->
            SimSub.none

        OnBroadcast _ ->
            -- Generated App.update dispatches broadcasts through real listeners.
            SimSub.none


timers : Listen msg -> List ( Float, Time.Posix -> msg )
timers listener =
    case listener of
        Batch listeners ->
            List.concatMap timers listeners

        Every interval callback ->
            [ ( interval, callback ) ]

        _ ->
            []
