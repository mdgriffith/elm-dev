module Page.Home exposing (Model, Msg, page)

import App.Page
import App.Page.Id
import App.Stores
import App.View
import App.View.Region
import Bridge
import Broadcast
import Effect
import Effect.Http as Http
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import Listen
import Listen.GeneratedBridge
import Listen.LocalStorage
import Random
import Time


type alias Model =
    { name : String, status : String, ticks : Int, listening : Bool, timestamp : Int }


type Msg
    = NameChanged String
    | Save
    | Saved (Result HttpError String)
    | HideNotice
    | Send
    | Received String
    | ReceiveFailed { value : Decode.Value, error : Decode.Error }
    | SubscriptionIgnored
    | Tick Time.Posix
    | Toggle
    | Now
    | GotTime Time.Posix
    | Key
    | Resized Int Int
    | BroadcastNow
    | LoggedOut
    | RandomRequested
    | RandomResult Int


type alias HttpError =
    String


page : App.Page.Page App.Stores.Stores App.Page.Id.Home_Params Msg Model
page =
    App.Page.page
        { init = \_ _ _ _ -> App.Page.init { name = "", status = "ready", ticks = 0, listening = True, timestamp = 0 }
        , update = update
        , subscriptions =
            \_ model ->
                Listen.batch
                    [ Bridge.listen { decoder = Decode.map Ok (Decode.field "name" Decode.string), onError = Err }
                        |> Listen.map
                            (\result ->
                                case result of
                                    Ok name ->
                                        Received name

                                    Err error ->
                                        ReceiveFailed error
                            )
                    , Listen.GeneratedBridge.listen
                        { decoder = Decode.map Received Decode.string
                        , onError = ReceiveFailed
                        }
                    , Listen.LocalStorage.onUpdated
                        { key = "fixture"
                        , decoder = Decode.map Received Decode.string
                        , onError = \_ -> SubscriptionIgnored
                        }
                    , if model.listening then
                        Listen.onEvery 100 Tick

                      else
                        Listen.none
                    , Listen.onKeyPress { ctrl = False, shift = False, key = "x" } Key
                    , Listen.onKeyPress { ctrl = False, shift = False, key = "y" } Key
                    , Listen.onResize Resized
                    , Listen.onBroadcast (\_ -> Just LoggedOut)
                    ]
        , view = view
        }


update : App.Stores.Stores -> Msg -> Model -> ( Model, Effect.Effect Msg )
update _ msg model =
    case msg of
        NameChanged name ->
            ( { model | name = name }, Effect.none )

        Save ->
            ( { model | status = "saving" }
            , Http.request
                { method = "POST"
                , url = "/save"
                , urlBase = Just Effect.UrlApi
                , headers = [ Http.header "X-Test" "yes" ]
                , body = Http.jsonBody (Encode.object [ ( "name", Encode.string model.name ) ])
                , expect = Http.expectString (Result.mapError (\_ -> "error") >> Saved)
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        Saved (Ok _) ->
            ( { model | status = "saved" }, Effect.sendMsgAfter 300 HideNotice )

        Saved (Err error) ->
            ( { model | status = error }, Effect.none )

        HideNotice ->
            ( { model | status = "ready" }, Effect.none )

        Send ->
            ( model, Bridge.send model.name )

        Received name ->
            ( { model | status = "received:" ++ name }, Effect.none )

        ReceiveFailed _ ->
            ( { model | status = "invalid incoming message" }, Effect.none )

        SubscriptionIgnored ->
            ( model, Effect.none )

        Tick timestamp ->
            ( { model | ticks = model.ticks + 1, timestamp = Time.posixToMillis timestamp }, Effect.none )

        Toggle ->
            ( { model | listening = not model.listening }, Effect.none )

        Now ->
            ( model, Effect.now GotTime )

        GotTime timestamp ->
            ( { model | timestamp = Time.posixToMillis timestamp }, Effect.none )

        Key ->
            ( { model | status = "key" }, Effect.none )

        Resized width height ->
            ( { model | status = String.fromInt width ++ "x" ++ String.fromInt height }, Effect.none )

        BroadcastNow ->
            ( model, Effect.broadcast Broadcast.LogOut )

        LoggedOut ->
            ( { model | status = "broadcast received" }, Effect.none )

        RandomRequested ->
            ( model, Effect.Generate (Random.map RandomResult (Random.int 0 10)) )

        RandomResult _ ->
            ( model, Effect.none )


view : App.View.Region.Id -> App.Stores.Stores -> Model -> App.View.View Msg
view _ _ model =
    { title = "Home"
    , body =
        Html.div []
            [ Html.input [ Attr.id "name", Attr.value model.name, Events.onInput NameChanged ] []
            , Html.button [ Events.onClick Save ] [ Html.text "Save" ]
            , Html.button [ Events.onClick Send ] [ Html.text "Send" ]
            , Html.button [ Events.onClick Toggle ] [ Html.text "Toggle" ]
            , Html.button [ Events.onClick Now ] [ Html.text "Now" ]
            , Html.button [ Events.onClick BroadcastNow ] [ Html.text "Broadcast" ]
            , Html.button [ Events.onClick RandomRequested ] [ Html.text "Random" ]
            , Html.text model.status
            , Html.text ("ticks:" ++ String.fromInt model.ticks)
            , Html.text ("time:" ++ String.fromInt model.timestamp)
            ]
    }
