port module Runner exposing (main)

import Json.Encode as Encode
import Platform
import Test exposing (Test)
import Test.Runner
import Test.Runner.Failure
import Random
import List
import Everything




type alias TestId = String

 

type alias Flags =
    { seed : Int
    , runs : Int
    }

type alias Model =
    { seed : Int
    , runs : Int
    }

main : Program Flags () msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = \_ -> runTest Run
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { seed = flags.seed,
      , runs = flags.runs
      }
    , Cmd.none
    )


type Msg
    = Run TestId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run testId ->
            case runTest model testId of
                Ok report ->
                    ( model, sendReport report )
                Err error ->
                    ( model, error error )




runTest : Model -> TestId -> Result String Report
runTest flags testId =
    let
        maybeRunner =
            Everything.tests
                |> List.filter (\(testId, test) -> test.id == testId)
                |> List.head
       
    in
    case maybeRunner of
        Nothing ->
            Err "Test not found"
            
        Just seededRunner ->
            case Test.Runner.fromTest flags.runs (Random.initialSeed flags.seed) seededRunner of
                Test.Runner.Invalid message -> Err message
                Test.Runner.Plain runners ->
                    Ok
                        { id = testId,
                        , runs =
                            runners 
                                |> List.map 
                                    (\runner -> 
                                        { label = runner.labels,
                                        , result =
                                            runner.run ()
                                                |> List.map runRunner
                                        }
                                    )
                        , isOnly = False
                        }

                Test.Runner.Only runners ->
                    Ok
                        { id = testId,
                        , runs =
                            runners 
                                |> List.map 
                                    (\runner -> 
                                        { label = runner.labels,
                                        , result =
                                            runner.run ()
                                                |> List.map runRunner
                                        }
                                    )
                        , isOnly = True
                        }

                Test.Runner.Skipping runners ->
                    Ok
                        { id = testId,
                        , runs =
                            runners 
                                |> List.map 
                                    (\runner -> 
                                        { label = runner.labels,
                                        , result = [ Skipped ]
                                        }
                                    )
                        , isOnly = False
                        }
               



runRunner : Test.Runner.Runner -> TestRun
runRunner runner =
    { label = runner.labels,
    , result =
        runner.run ()
            |> List.map 
                (\expectation -> 
                    case Test.Runner.getFailureReason expectation of
                        Just failure ->
                            Failed
                                { given = failure.given,
                                , description = failure.description
                                , reason = failure.reason
                                }

                        Nothing ->
                            Passed
                )
    }
                                    

type alias Report =
    { id : TestId
    , runs : List TestRun
    , isOnly : Bool
    }

type alias TestRun =
    { label : List String
    , result : List TestResult
    }


type TestResult
    = Passed
    | Skipped
    | Failed
        { given : Maybe String
        , description : String
        , reason : Test.Runner.Failure.Reason
        }

encodeTestRun : TestRun -> Encode.Value
encodeTestRun run =
    Encode.object
        [ ( "label", Encode.list Encode.string run.label )
        , ( "result", Encode.list encodeTestResult run.result )
        ]


encodeTestResult : TestResult -> Encode.Value
encodeTestResult result =
    case result of
        Passed -> Encode.object [ ( "status", Encode.string "pass" ) ]
        Skipped -> Encode.object [ ( "status", Encode.string "skip" ) ]
        Failed { given, description, reason } ->
            Encode.object
                [ ( "status", Encode.string "fail" )
                , ( "given", Maybe.withDefault Encode.null (Maybe.map Encode.string given) )
                , ( "message", Encode.string description )
                , ( "reason", encodeReason reason )
                ]

encodeReason : Test.Runner.Failure.Reason -> Encode.Value
encodeReason reason =
    case reason of
        Test.Runner.Failure.Custom ->
            Encode.string "Custom"

        Test.Runner.Failure.Equality expected actual ->
            Encode.object 
                [ ( "type", Encode.string "Equality" )
                , ( "expected", Encode.string expected )
                , ( "actual", Encode.string actual )
                ]

        Test.Runner.Failure.Comparison first second ->
            Encode.object 
                [ ( "type", Encode.string "Comparison" )
                , ( "first", Encode.string first )
                , ( "second", Encode.string second )
                ]

        Test.Runner.Failure.TODO ->
            Encode.string "TODO"

        Test.Runner.Failure.Invalid BadDescription ->
            Encode.string "Invalid, Bad description"

        Test.Runner.Failure.Invalid _ ->
            Encode.string "Invalid"

        Test.Runner.Failure.ListDiff expected actual ->
            Encode.object 
                [ ( "type", Encode.string "ListDiff" )
                , ( "expected", Encode.list Encode.string expected )
                , ( "actual", Encode.list Encode.string actual )
                ]

        Test.Runner.Failure.CollectionDiff { expected, actual, extra, missing } ->
            Encode.object 
                [ ( "type", Encode.string "CollectionDiff" )
                , ( "expected", Encode.string expected )
                , ( "actual", Encode.string actual )
                , ( "extra", Encode.list Encode.string extra )
                , ( "missing", Encode.list Encode.string missing )
                ]



-- Incoming
port runTest : (TestId -> msg) -> Sub msg


-- Outgoing
port error : String -> Cmd msg
port reportSent : { id : TestId, runs : Encode.Value, isOnly : Bool } -> Cmd msg


sendReport : Report -> Cmd msg
sendReport { id, runs, isOnly } =
    reportSent
        { id = id
        , runs = encodeTestRun result
        , isOnly = isOnly
        }
