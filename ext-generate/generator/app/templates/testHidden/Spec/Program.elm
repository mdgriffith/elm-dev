module Spec.Program exposing
    ( Config, Model, Msg, start
    , unsupported, validate
    )

{-| The test runtime supplied by elm-dev. Periodic subscriptions share
elm-program-test's virtual clock with delayed effects. Timer callbacks are read
from the latest model; removing and re-adding a subscription invalidates old ticks.

@docs Config, Model, Msg, start
@docs unsupported, validate

-}

import Browser
import Expect
import Html
import Json.Decode
import Json.Encode as Encode
import ProgramTest exposing (ProgramTest, SimulatedEffect, SimulatedSub)
import SimulatedEffect.Cmd as SimCmd
import SimulatedEffect.Ports as Ports
import SimulatedEffect.Process as Process
import SimulatedEffect.Sub as SimSub
import SimulatedEffect.Task as Task
import SimulatedEffect.Time as Clock
import Time
import Url


type alias Config flags model msg effect =
    { init : flags -> Url.Url -> () -> ( model, effect )
    , update : msg -> model -> ( model, effect )
    , view : model -> Browser.Document msg
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url.Url -> msg
    , effects : effect -> SimulatedEffect msg
    , subscriptions : model -> SimulatedSub msg
    , timers : model -> List ( Float, Time.Posix -> msg )
    }


type Model model
    = Model { value : model, timers : List Timer, generation : Int }


type alias Timer =
    { interval : Float, generation : Int }


type Msg msg
    = Application msg
    | Tick Float Int Time.Posix


start : { url : String, flags : flags, time : Time.Posix } -> Config flags model msg effect -> ProgramTest (Model model) (Msg msg) (SimulatedEffect (Msg msg))
start options config =
    let
        sync value previous =
            let
                intervals =
                    config.timers value |> List.map Tuple.first |> unique

                retained =
                    List.filter (\timer -> List.member timer.interval intervals) previous.timers

                added =
                    intervals
                        |> List.filter (\interval -> not (List.any (\timer -> timer.interval == interval) retained))
                        |> List.indexedMap (\index interval -> { interval = interval, generation = previous.generation + index + 1 })
            in
            ( Model { value = value, timers = retained ++ added, generation = previous.generation + List.length added }
            , List.map schedule added
            )

        initialize flags url key =
            let
                ( value, effect ) =
                    config.init flags url key

                ( model, timers ) =
                    sync value { timers = [], generation = 0 }
            in
            ( model, SimCmd.batch (SimCmd.map Application (config.effects effect) :: timers) )

        update message ((Model current) as original) =
            case message of
                Application appMsg ->
                    let
                        ( value, effect ) =
                            config.update appMsg current.value

                        ( model, timers ) =
                            sync value current
                    in
                    ( model, SimCmd.batch (SimCmd.map Application (config.effects effect) :: timers) )

                Tick interval generation timestamp ->
                    if List.any (\timer -> timer.interval == interval && timer.generation == generation) current.timers then
                        let
                            messages =
                                config.timers current.value
                                    |> List.filter (\( ms, _ ) -> ms == interval)
                                    |> List.map (\( _, callback ) -> callback (Time.millisToPosix (Time.posixToMillis options.time + Time.posixToMillis timestamp)))

                            ( value, effects ) =
                                List.foldl
                                    (\msg ( previous, accumulated ) ->
                                        let
                                            ( updatedValue, effect ) =
                                                config.update msg previous
                                        in
                                        ( updatedValue, accumulated ++ [ SimCmd.map Application (config.effects effect) ] )
                                    )
                                    ( current.value, [] )
                                    messages

                            ( (Model next) as model, newTimers ) =
                                sync value current

                            repeat =
                                if List.any (\timer -> timer.interval == interval && timer.generation == generation) next.timers then
                                    [ schedule { interval = interval, generation = generation } ]

                                else
                                    []
                        in
                        ( model, SimCmd.batch (effects ++ newTimers ++ repeat) )

                    else
                        ( original, SimCmd.none )
    in
    ProgramTest.createApplication
        { init = initialize
        , update = update
        , view = \(Model model) -> config.view model.value |> mapDocument Application
        , onUrlRequest = config.onUrlRequest >> Application
        , onUrlChange = config.onUrlChange >> Application
        }
        |> ProgramTest.withBaseUrl options.url
        |> ProgramTest.withSimulatedEffects identity
        |> ProgramTest.withSimulatedSubscriptions (\(Model model) -> config.subscriptions model.value |> SimSub.map Application)
        |> ProgramTest.start options.flags


schedule : Timer -> SimulatedEffect (Msg msg)
schedule timer =
    Process.sleep (max 1 timer.interval)
        |> Task.andThen (\_ -> Clock.now)
        |> Task.perform (Tick timer.interval timer.generation)


unique : List comparable -> List comparable
unique values =
    List.foldl
        (\value result ->
            if List.member value result then
                result

            else
                result ++ [ value ]
        )
        []
        values


mapDocument : (a -> b) -> Browser.Document a -> Browser.Document b
mapDocument transform document =
    { title = document.title, body = List.map (Html.map transform) document.body }


{-| Report an effect that needs an app-owned simulator. Spec.run checks these
diagnostics after startup and each interaction rather than silently dropping it.
-}
unsupported : String -> SimulatedEffect msg
unsupported description =
    Ports.send "__elm_spec_errors" (Encode.string description)


validate : ProgramTest model msg effect -> ProgramTest model msg effect
validate program =
    case ProgramTest.getOutgoingPortValues "__elm_spec_errors" program of
        Ok (_ :: _) ->
            ProgramTest.ensureOutgoingPortValues "__elm_spec_errors"
                Json.Decode.string
                (\errors -> Expect.fail (String.join "\n" errors))
                program

        _ ->
            program
