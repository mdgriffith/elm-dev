port module Runner exposing (main)

import Json.Encode as Encode
import Platform
import Test exposing (Test)
import Test.Reporter.TestResults as TR exposing (Outcome(..))
import Test.Runner
import Random
import List
import Everything


port results : String -> Cmd msg


main : Program () () msg
main =
    Platform.worker
        { init = \_ -> ( (), runTests )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


runTests : Cmd msg
runTests =
    let
        runners =
            Test.Runner.fromTest 1 (Random.initialSeed 0) Everything.tests

        expectations =
            case runners of
                Test.Runner.Invalid _ -> []
                Test.Runner.Only rs -> List.map (.run >> (\f -> f ())) rs
                Test.Runner.Skipping rs -> List.map (.run >> (\f -> f ())) rs
                Test.Runner.Plain rs -> List.map (.run >> (\f -> f ())) rs

        outcomes =
            TR.outcomesFromExpectations expectations

        summarize out (p,f,fs) =
            case out of
                TR.Passed _ -> ( p + 1, f, fs )
                TR.Failed _ -> ( p, f + 1, "failure" :: fs )
                TR.Todo _ -> ( p, f, fs )

        ( passed, failed, failures ) =
            List.foldl summarize ( 0, 0, [] ) outcomes

        total = passed + failed

        json =
            Encode.object
                [ ( "total", Encode.int total )
                , ( "passed", Encode.int passed )
                , ( "failed", Encode.int failed )
                , ( "failures", Encode.list Encode.string failures )
                ]
                |> Encode.encode 0
    in
    results json


