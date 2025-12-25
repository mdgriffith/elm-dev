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


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = \_ -> runTest Run
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { seed = flags.seed
      , runs = flags.runs
      }
    , Cmd.none
    )


type Msg
    = Run TestId


type RunResult
    = Error String
    | Finished Report
    | SkippedBecauseNotATest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run testId ->
            case run model testId of
                Finished report ->
                    ( model, sendReport report )
                Error message ->
                    ( model, error message )
                SkippedBecauseNotATest ->
                    ( model, sendSkippedReport testId )




run : Model -> TestId -> RunResult
run flags testId =
    let
        maybeRunner =
            Everything.tests
                |> List.filterMap (\(id, maybeTest) -> if id == testId then maybeTest else Nothing)
                |> List.head
       
    in
    case maybeRunner of
        Nothing ->
            -- Test ID not found or value is not a test - skip it
            SkippedBecauseNotATest
            
        Just seededRunner ->
            case Test.Runner.fromTest flags.runs (Random.initialSeed flags.seed) seededRunner of
                Test.Runner.Invalid message -> Error message
                Test.Runner.Plain runners ->
                    Finished
                        { id = testId
                        , runs = List.map runRunner runners
                        , isOnly = False
                        }

                Test.Runner.Only runners ->
                    Finished
                        { id = testId
                        , runs = List.map runRunner runners
                        , isOnly = True
                        }

                Test.Runner.Skipping runners ->
                    Finished
                        { id = testId
                        , runs =
                            runners 
                                |> List.map 
                                    (\runner -> 
                                        { label = List.reverse runner.labels
                                        , result = [ Skipped ]
                                        }
                                    )
                        , isOnly = False
                        }
               



runRunner : Test.Runner.Runner -> TestRun
runRunner runner =
    { label = List.reverse runner.labels
    , result =
        runner.run ()
            |> List.map 
                (\expectation -> 
                    case Test.Runner.getFailureReason expectation of
                        Just failure ->
                            Failed
                                { given = failure.given
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
encodeTestRun testRun =
    Encode.object
        [ ( "label", Encode.list Encode.string testRun.label )
        , ( "result", Encode.list encodeTestResult testRun.result )
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
            Encode.object 
                [ ( "type", Encode.string "Custom"
                  )
                ]

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
            Encode.object 
                [ ( "type", Encode.string "TODO"
                  )
                ]

        Test.Runner.Failure.Invalid invalidReason ->
            Encode.object 
                [ ( "type", Encode.string "Invalid")
                , ( "data", Encode.string (invalidReasonToString invalidReason) )
                ]

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


invalidReasonToString : Test.Runner.Failure.InvalidReason -> String
invalidReasonToString invalidReason =
    case invalidReason of
        Test.Runner.Failure.BadDescription -> "Bad description"
        Test.Runner.Failure.InvalidFuzzer -> "Invalid fuzzer"
        Test.Runner.Failure.NonpositiveFuzzCount -> "Nonpositive fuzz count"
        Test.Runner.Failure.EmptyList -> "Empty list"
        Test.Runner.Failure.DuplicatedName -> "Duplicated name"
        Test.Runner.Failure.DistributionInsufficient -> "Distribution insufficient"
        Test.Runner.Failure.DistributionBug -> "Distribution bug"


-- Incoming
port runTest : (TestId -> msg) -> Sub msg


-- Outgoing
port error : String -> Cmd msg
port reportSent : { id : TestId, runs : Encode.Value, isOnly : Bool, skippedBecauseNotATest : Bool } -> Cmd msg


sendReport : Report -> Cmd msg
sendReport { id, runs, isOnly } =
    reportSent
        { id = id
        , runs = Encode.list encodeTestRun runs
        , isOnly = isOnly
        , skippedBecauseNotATest = False
        }


sendSkippedReport : TestId -> Cmd msg
sendSkippedReport testId =
    reportSent
        { id = testId
        , runs = Encode.list encodeTestRun []
        , isOnly = False
        , skippedBecauseNotATest = True
        }
