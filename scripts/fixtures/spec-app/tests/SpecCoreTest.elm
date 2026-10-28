module SpecCoreTest exposing (tests)

import App.Spec
import Html
import Json.Decode as Decode
import Json.Encode as Encode
import ProgramTest
import SimulatedEffect.Cmd as SimCmd
import SimulatedEffect.Http as Http
import SimulatedEffect.Ports as Ports
import Spec
import Support.TestApp
import Test exposing (Test)
import Test.Http


httpStart first second () =
    ProgramTest.createElement
        { init = \_ -> ( "empty", SimCmd.batch [ request "first" first, request "second" second ] )
        , update = \message model -> ( model ++ "," ++ message, SimCmd.none )
        , view = Html.text
        }
        |> ProgramTest.withSimulatedEffects identity
        |> ProgramTest.start ()


request label variable =
    Http.request
        { method = "POST"
        , url = "/api"
        , headers = []
        , body = Http.jsonBody (Encode.object [ ( "variable", Encode.string variable ) ])
        , expect = Http.expectString (\_ -> label)
        , timeout = Nothing
        , tracker = Nothing
        }


matching variable =
    { method = "POST"
    , url = "/api"
    , description = "request with variable " ++ variable
    , matches = \request_ -> Decode.decodeString (Decode.field "variable" Decode.string) request_.body == Ok variable
    }


respond variable =
    Spec.httpResponse (matching variable) (Test.Http.httpResponse { statusCode = 200, headers = [], body = "ok" })


portStart () =
    ProgramTest.createElement
        { init = \_ -> ( "", SimCmd.batch [ Ports.send "out" (Encode.int 1), Ports.send "out" (Encode.int 2) ] )
        , update = \message model -> ( model ++ String.fromInt message, SimCmd.none )
        , view = Html.text
        }
        |> ProgramTest.withSimulatedEffects identity
        |> ProgramTest.withSimulatedSubscriptions (\_ -> Ports.subscribe "reply" Decode.int identity)
        |> ProgramTest.start ()


receiveOutgoing number =
    Spec.outgoingPort
        { name = "out"
        , description = "consume outbound " ++ String.fromInt number
        , decoder = Decode.int
        , matches = (==) number
        , then_ = \captured -> Spec.incomingPort "reply" (Encode.int captured)
        }


tests : Test
tests =
    Test.describe "Bundled Spec runtime"
        [ Spec.onPage
            { start = httpStart "A" "B"
            , init = Spec.initStatic ()
            , tests =
                \_ ->
                    [ Spec.test "concurrent requests match actual bodies and respond to the exact request"
                        [ respond "B"
                        , Spec.expect.textIsRendered "empty,second"
                        , Spec.noHttpRequest (matching "B")
                        , Spec.httpPending (matching "A")
                        , respond "A"
                        , Spec.expect.textIsRendered "empty,second,first"
                        ]
                    ]
            }
            |> Spec.run
        , Spec.onPage
            { start = httpStart "same" "same"
            , init = Spec.initStatic ()
            , tests =
                \_ ->
                    [ Spec.test "identical requests are consumed oldest first"
                        [ respond "same"
                        , Spec.expect.textIsRendered "empty,first"
                        , respond "same"
                        , Spec.expect.textIsRendered "empty,first,second"
                        ]
                    ]
            }
            |> Spec.run
        , Spec.onPage
            { start = \_ -> App.Spec.start Support.TestApp.defaults
            , init = Spec.initStatic ()
            , tests =
                \_ ->
                    [ Spec.test "removing and re-adding a timer invalidates stale ticks"
                        [ Spec.advanceTime 50
                        , Spec.user.clicksButton "Toggle"
                        , Spec.advanceTime 10
                        , Spec.user.clicksButton "Toggle"
                        , Spec.advanceTime 40
                        , Spec.expect.textIsRendered "ticks:0"
                        , Spec.advanceTime 60
                        , Spec.expect.textIsRendered "ticks:1"
                        , Spec.expect.textIsRendered "time:160"
                        , Spec.advanceTime 100
                        , Spec.expect.textIsRendered "ticks:2"
                        ]
                    ]
            }
            |> Spec.run
        , Spec.onPage
            { start = portStart
            , init = Spec.initWith { cases = [ 1, 2 ], setup = \number -> [ receiveOutgoing number ] }
            , tests =
                \number ->
                    [ Spec.test "fixture setup is fresh and captures typed port identities"
                        [ Spec.expect.textIsRendered (String.fromInt number)
                        , receiveOutgoing (3 - number)
                        , Spec.expect.textIsRendered (String.fromInt number ++ String.fromInt (3 - number))
                        ]
                    ]
            }
            |> Spec.run
        ]
