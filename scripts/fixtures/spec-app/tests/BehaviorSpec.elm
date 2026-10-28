module BehaviorSpec exposing (tests)

import App.Spec
import Json.Decode as Decode
import Json.Encode as Encode
import Spec
import Support.BridgeSpec
import Support.TestApp
import Test exposing (Test)
import Test.Http
import Time


saveRequest =
    { method = "POST"
    , url = "https://api.example.test/save"
    , description = "save Ada with the shared API header"
    , matches =
        \request ->
            Decode.decodeString (Decode.field "name" Decode.string) request.body
                == Ok "Ada"
                && List.member ( "Authorization", "test-token" ) request.headers
                && List.member ( "X-Test", "yes" ) request.headers
    }


tests : Test
tests =
    let
        defaults =
            Support.TestApp.defaults

        options =
            { defaults
                | time = Time.millisToPosix 1000
                , httpTarget = \_ -> { urlBase = "https://api.example.test", headers = [ ( "Authorization", "test-token" ) ] }
            }
    in
    Spec.onPage
        { start = \_ -> App.Spec.start options
        , init = Spec.initStatic ()
        , tests =
            \_ ->
                [ Spec.test "route, page, HTTP exchange, and delayed notice"
                    [ Spec.expect.textIsRendered "ready"
                    , Spec.user.inputsText { fieldId = "name", input = "Ada" }
                    , Spec.user.clicksButton "Save"
                    , Spec.expect.textIsRendered "saving"
                    , Spec.httpResponse saveRequest (Test.Http.httpResponse { statusCode = 200, headers = [], body = "ok" })
                    , Spec.expect.textIsRendered "saved"
                    , Spec.advanceTime 299
                    , Spec.expect.textIsRendered "saved"
                    , Spec.advanceTime 1
                    , Spec.expect.textIsRendered "ready"
                    ]
                , Spec.test "app-owned adapter captures an outgoing exchange and uses the mapped receive callback"
                    [ Spec.user.inputsText { fieldId = "name", input = "Ada" }
                    , Spec.user.clicksButton "Send"
                    , Support.BridgeSpec.exchange "Ada"
                    , Spec.expect.textIsRendered "received:Ada"
                    , Spec.user.inputsText { fieldId = "name", input = "Grace" }
                    , Spec.user.clicksButton "Send"
                    , Support.BridgeSpec.exchange "Grace"
                    , Spec.expect.textIsRendered "received:Grace"
                    ]
                , Spec.test "malformed incoming JSON reaches the mapped app-owned error callback"
                    [ Spec.incomingPort "bridgeIn" (Encode.object [ ( "name", Encode.int 42 ) ])
                    , Spec.expect.textIsRendered "invalid incoming message"
                    , Spec.incomingPort "bridgeIn" (Encode.object [ ( "name", Encode.string "Ada" ) ])
                    , Spec.expect.textIsRendered "received:Ada"
                    ]
                , Spec.test "generated listener helper delivers valid JSON and routes malformed input to onError"
                    [ Spec.incomingPort "generatedBridge" (Encode.string "Ada")
                    , Spec.expect.textIsRendered "received:Ada"
                    , Spec.incomingPort "generatedBridge" (Encode.int 42)
                    , Spec.expect.textIsRendered "invalid incoming message"
                    ]
                , Spec.test "local storage helper can explicitly ignore malformed input"
                    [ Spec.incomingPort "localStorageUpdated" (Encode.int 42)
                    , Spec.expect.textIsRendered "ready"
                    , Spec.incomingPort "localStorageUpdated" (Encode.string "Ada")
                    , Spec.expect.textIsRendered "received:Ada"
                    , Spec.incomingPort "localStorageUpdated" Encode.null
                    , Spec.expect.textIsRendered "received:Ada"
                    ]
                , Spec.test "page timers and Time.now share a configured virtual clock"
                    [ Spec.advanceTime 100
                    , Spec.expect.textIsRendered "ticks:1"
                    , Spec.expect.textIsRendered "time:1100"
                    , Spec.user.clicksButton "Toggle"
                    , Spec.advanceTime 250
                    , Spec.expect.textIsRendered "ticks:1"
                    , Spec.user.clicksButton "Now"
                    , Spec.expect.textIsRendered "time:1350"
                    ]
                , Spec.test "global keyboard listeners ignore nonmatches and resize is delivered"
                    [ Spec.incomingPort "__elm_spec_keydown"
                        (Encode.object [ ( "key", Encode.string "x" ), ( "ctrlKey", Encode.bool False ), ( "metaKey", Encode.bool False ), ( "shiftKey", Encode.bool False ) ])
                    , Spec.expect.textIsRendered "key"
                    , Spec.incomingPort "__elm_spec_resize" (Encode.object [ ( "width", Encode.int 800 ), ( "height", Encode.int 600 ) ])
                    , Spec.expect.textIsRendered "800x600"
                    ]
                , Spec.test "internal broadcasts reach the real page listeners"
                    [ Spec.user.clicksButton "Broadcast"
                    , Spec.expect.textIsRendered "broadcast received"
                    ]
                ]
        }
        |> Spec.run
