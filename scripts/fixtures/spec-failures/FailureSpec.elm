module FailureSpec exposing (tests)

import App.Spec
import Spec
import Support.BridgeSpec
import Support.TestApp
import Test exposing (Test)


tests : Test
tests =
    Spec.onPage
        { start = \_ -> App.Spec.start Support.TestApp.defaults
        , init = Spec.initStatic ()
        , tests =
            \_ ->
                [ Spec.test "an outbound exchange cannot be consumed twice"
                    [ Spec.user.inputsText { fieldId = "name", input = "Ada" }
                    , Spec.user.clicksButton "Send"
                    , Support.BridgeSpec.exchange "Ada"
                    , Support.BridgeSpec.exchange "Ada"
                    , Spec.expect.textIsRendered "this later assertion must not replace the original failure"
                    ]
                , Spec.test "unsupported effects fail rather than being silently ignored"
                    [ Spec.user.clicksButton "Random" ]
                ]
        }
        |> Spec.run
