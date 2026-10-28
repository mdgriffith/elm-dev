module ListenerErrorTest exposing (tests)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Listen
import Listen.GeneratedBridge
import Listen.LocalStorage
import Test exposing (Test)


tests : Test
tests =
    let
        decoder =
            Decode.map Ok (Decode.field "name" Decode.string)

        payload =
            Encode.object
                [ ( "requestId", Encode.string "request-42" )
                , ( "name", Encode.int 42 )
                ]
    in
    [ ( "generated listener", Listen.GeneratedBridge.listen { decoder = decoder, onError = Err } )
    , ( "local storage listener", Listen.LocalStorage.onUpdated { key = "fixture", decoder = decoder, onError = Err } )
    ]
        |> List.map
            (\( label, listener ) ->
                Test.test (label ++ " preserves the full payload and structured decoding error through map") <|
                    \_ ->
                        case Listen.map (Result.mapError (\failure -> ( failure.value, failure.error ))) listener of
                            Listen.OnFromJs incoming ->
                                case incoming.receive payload of
                                    Err ( value, Decode.Field "name" (Decode.Failure _ invalidValue) ) ->
                                        Expect.equal
                                            ( Encode.encode 0 payload, "42" )
                                            ( Encode.encode 0 value, Encode.encode 0 invalidValue )

                                    _ ->
                                        Expect.fail "Expected the original payload and a structured field decoding error"

                            _ ->
                                Expect.fail "Expected an incoming JSON listener"
            )
        |> Test.describe "Listener error context"
