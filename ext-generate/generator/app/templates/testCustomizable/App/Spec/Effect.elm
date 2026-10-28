module App.Spec.Effect exposing (simulate)

{-| Customize this module to supply simulations for application-specific effects.
Unsupported effects fail specs with a diagnostic instead of being discarded.
-}

import App.Page.Id
import App.View.Region
import Broadcast
import Effect exposing (Effect(..))
import Json.Decode
import ProgramTest exposing (SimulatedEffect)
import SimulatedEffect.Cmd as SimCmd
import SimulatedEffect.Http as Http
import SimulatedEffect.Navigation as Navigation
import SimulatedEffect.Ports as Ports
import SimulatedEffect.Process as Process
import SimulatedEffect.Task as Task
import SimulatedEffect.Time as Clock
import Spec.Program
import Time


simulate :
    { callbacks :
        { preload : App.Page.Id.Id -> msg
        , viewRequested : App.View.Region.Operation App.Page.Id.Id -> msg
        , broadcast : Broadcast.Msg -> msg
        , dropPageCache : msg
        , ignore : String -> msg
        }
    , time : Time.Posix
    , zone : Time.Zone
    , zoneName : Time.ZoneName
    , httpTarget : Effect.UrlBase -> { headers : List Effect.Header, urlBase : String }
    }
    -> Effect msg
    -> SimulatedEffect msg
simulate options effect =
    let
        send msg =
            Task.succeed msg |> Task.perform identity

        later delay msg =
            Process.sleep (max 0 delay) |> Task.perform (\_ -> msg)

        now delay toMsg =
            Process.sleep (max 0 delay)
                |> Task.andThen (\_ -> Clock.now)
                |> Task.perform (\timestamp -> toMsg (Time.millisToPosix (Time.posixToMillis options.time + Time.posixToMillis timestamp)))

        unwrap result =
            case result of
                Ok msg ->
                    msg

                Err msg ->
                    msg
    in
    case effect of
        None ->
            SimCmd.none

        Batch effects ->
            SimCmd.batch (List.map (simulate options) effects)

        SendMsg msg ->
            send msg

        SendMsgAfter delay msg ->
            later (toFloat delay) msg

        Now delay toMsg ->
            now (Maybe.withDefault 0 delay) toMsg

        Here toMsg ->
            send (toMsg options.zone)

        GetZoneName toMsg ->
            send (toMsg options.zoneName)

        PushUrl url ->
            Navigation.pushUrl url

        ReplaceUrl url ->
            Navigation.replaceUrl url

        Back steps ->
            Navigation.back steps

        Load url ->
            Navigation.load url

        Reload ->
            Navigation.reload

        ReloadAndSkipCache ->
            Navigation.reloadAndSkipCache

        ViewUpdated operation ->
            send (options.callbacks.viewRequested operation)

        Preload page ->
            send (options.callbacks.preload page)

        SendBroadcast message ->
            send (options.callbacks.broadcast message)

        SendToWorld outgoing ->
            Ports.send outgoing.portName outgoing.payload

        Focus _ toMsg ->
            later 1 (toMsg (Ok ()))

        Blur _ toMsg ->
            send (toMsg (Ok ()))

        ScrollToBottomOf scroll ->
            later 1 scroll.onScrollFinish

        ScrollToTopOf scroll ->
            send scroll.onScrollFinish

        ScrollTo scroll ->
            send (scroll.onScrollFinish (Ok ()))

        HttpRequest request ->
            let
                target =
                    Effect.resolveHttpTarget options.httpTarget request

                body =
                    case request.body of
                        Effect.EmptyBody ->
                            Just Http.emptyBody

                        Effect.StringBody contentType value ->
                            Just (Http.stringBody contentType value)

                        Effect.JsonBody value ->
                            Just (Http.jsonBody value)

                        _ ->
                            Nothing

                expectation =
                    case request.expect of
                        Effect.ExpectString toMsg ->
                            Just (Http.expectString toMsg)

                        Effect.ExpectStringResponse toMsg ->
                            Just (Http.expectStringResponse unwrap (toMsg >> Ok))

                        Effect.ExpectJson decoder onError ->
                            Just
                                (Http.expectJson
                                    (Result.mapError onError
                                        >> (\result ->
                                                case result of
                                                    Ok msg ->
                                                        msg

                                                    Err msg ->
                                                        msg
                                           )
                                    )
                                    decoder
                                )

                        Effect.ExpectWhatever toMsg ->
                            Just (Http.expectWhatever toMsg)

                        _ ->
                            Nothing
            in
            if request.timeout /= Nothing || request.tracker /= Nothing then
                Spec.Program.unsupported "HTTP timeout/tracker simulation requires a custom App.Spec.Effect simulator."

            else
                case ( body, expectation ) of
                    ( Just requestBody, Just expect ) ->
                        Http.request
                            { method = request.method
                            , headers = target.headers
                            , url = target.url
                            , body = requestBody
                            , expect = expect
                            , timeout = Nothing
                            , tracker = Nothing
                            }

                    _ ->
                        Spec.Program.unsupported "Binary/file/multipart HTTP simulation requires a custom App.Spec.Effect simulator."

        Generate _ ->
            Spec.Program.unsupported "Random generation requires a custom App.Spec.Effect simulator."

        GetBoundingBox _ _ ->
            Spec.Program.unsupported "DOM measurements require a custom App.Spec.Effect simulator."

        GetBoundingBoxList _ _ ->
            Spec.Program.unsupported "DOM measurements require a custom App.Spec.Effect simulator."

        Forward _ ->
            Spec.Program.unsupported "Forward navigation is not supported by elm-program-test."

        CancelHttp _ ->
            Spec.Program.unsupported "HTTP cancellation requires a custom App.Spec.Effect simulator."

        File _ _ ->
            Spec.Program.unsupported "File selection requires a custom App.Spec.Effect simulator."

        Files _ _ ->
            Spec.Program.unsupported "File selection requires a custom App.Spec.Effect simulator."

        FileToUrl _ _ ->
            Spec.Program.unsupported "File conversion requires a custom App.Spec.Effect simulator."

        FileToString _ _ ->
            Spec.Program.unsupported "File conversion requires a custom App.Spec.Effect simulator."

        FileToBytes _ _ ->
            Spec.Program.unsupported "File conversion requires a custom App.Spec.Effect simulator."
