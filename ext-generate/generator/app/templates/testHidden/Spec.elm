module Spec exposing
    ( Spec, Test, Interaction, Session, Init
    , onPage, initStatic, initWith, test, run, batch, custom, fromProgramTest, mapProgram
    , user, expect, advanceTime, incomingPort, outgoingPort
    , HttpRequest, HttpMatch, httpResponse, httpPending, noHttpRequest
    )

{-| Behavioral interaction lists supplied by elm-dev, backed by elm-program-test.
Adapters produce the same Interaction values as built-in actions and assertions.

@docs Spec, Test, Interaction, Session, Init
@docs onPage, initStatic, initWith, test, run, batch, custom, fromProgramTest, mapProgram
@docs user, expect, advanceTime, incomingPort, outgoingPort
@docs HttpRequest, HttpMatch, httpResponse, httpPending, noHttpRequest

-}

import Dict exposing (Dict)
import Expect
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import ProgramTest exposing (ProgramTest)
import Spec.Program
import Test as ElmTest
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Runner


type Spec model msg effect
    = Spec
        { start : () -> ProgramTest model msg effect
        , tests : List (Test model msg effect)
        }


type Test model msg effect
    = Test String (List (Interaction model msg effect))


type Interaction model msg effect
    = Interaction String (Session model msg effect -> Session model msg effect)


{-| A fresh session is allocated for each test. Port consumption is tracked here,
not globally, so adapters cannot accidentally reuse a previous exchange.
-}
type Session model msg effect
    = Session
        { program : ProgramTest model msg effect
        , consumed : Dict String (List Int)
        }


type Init data model msg effect
    = Init (List ( data, List (Interaction model msg effect) ))


initStatic : data -> Init data model msg effect
initStatic value =
    Init [ ( value, [] ) ]


{-| Each fixture starts a fresh application for every test. Adapters can provide
setup without adding technology-specific query types to Spec.
-}
initWith : { cases : List data, setup : data -> List (Interaction model msg effect) } -> Init data model msg effect
initWith options =
    Init (List.map (\value -> ( value, options.setup value )) options.cases)


onPage :
    { start : () -> ProgramTest model msg effect
    , init : Init data model msg effect
    , tests : data -> List (Test model msg effect)
    }
    -> Spec model msg effect
onPage options =
    let
        (Init cases) =
            options.init
    in
    Spec
        { start = options.start
        , tests =
            List.indexedMap
                (\index ( value, setup ) ->
                    options.tests value
                        |> List.map
                            (\(Test label interactions) ->
                                Test
                                    (if List.length cases > 1 then
                                        "Setup " ++ String.fromInt (index + 1) ++ ": " ++ label

                                     else
                                        label
                                    )
                                    (setup ++ interactions)
                            )
                )
                cases
                |> List.concat
        }


test : String -> List (Interaction model msg effect) -> Test model msg effect
test =
    Test


run : Spec model msg effect -> ElmTest.Test
run (Spec spec) =
    ElmTest.concat
        (List.map
            (\(Test label interactions) ->
                ElmTest.test label
                    (\_ ->
                        let
                            (Session session) =
                                List.indexedMap Tuple.pair interactions
                                    |> List.foldl
                                        (\( index, Interaction description action ) ((Session previous) as current) ->
                                            let
                                                (Session next) =
                                                    if Test.Runner.getFailureReason (ProgramTest.done previous.program) == Nothing then
                                                        case action current of
                                                            Session result ->
                                                                Session { result | program = Spec.Program.validate result.program }

                                                    else
                                                        current
                                            in
                                            case ( Test.Runner.getFailureReason (ProgramTest.done previous.program), Test.Runner.getFailureReason (ProgramTest.done next.program) ) of
                                                ( _, Nothing ) ->
                                                    Session next

                                                ( Nothing, Just failure ) ->
                                                    Session
                                                        { next
                                                            | program =
                                                                ProgramTest.createFailed
                                                                    ("Interaction " ++ String.fromInt (index + 1) ++ ": " ++ description)
                                                                    failure.description
                                                        }

                                                _ ->
                                                    current
                                        )
                                        (Session { program = Spec.Program.validate (spec.start ()), consumed = Dict.empty })
                        in
                        ProgramTest.done session.program
                    )
            )
            spec.tests
        )


custom : { description : String, run : Session model msg effect -> Session model msg effect } -> Interaction model msg effect
custom options =
    Interaction options.description options.run


{-| Low-level escape hatch for adapters. Prefer the transport primitives when
possible, as they preserve request matching and port consumption semantics.
-}
fromProgramTest : String -> (ProgramTest model msg effect -> ProgramTest model msg effect) -> Interaction model msg effect
fromProgramTest description transform =
    Interaction description (mapProgram transform)


mapProgram : (ProgramTest model msg effect -> ProgramTest model msg effect) -> Session model msg effect -> Session model msg effect
mapProgram transform (Session session) =
    case Test.Runner.getFailureReason (ProgramTest.done session.program) of
        Just _ ->
            Session session

        Nothing ->
            Session { session | program = transform session.program }


batch : List (Interaction model msg effect) -> Interaction model msg effect
batch interactions =
    Interaction "batch"
        (\session ->
            List.foldl (\(Interaction _ action) -> action) session interactions
        )


user :
    { clicksButton : String -> Interaction model msg effect
    , clicksLink : String -> String -> Interaction model msg effect
    , inputsText : { fieldId : String, input : String } -> Interaction model msg effect
    }
user =
    { clicksButton = \label -> fromProgramTest ("click button " ++ label) (ProgramTest.clickButton label)
    , clicksLink = \label href -> fromProgramTest ("click link " ++ label) (ProgramTest.clickLink label href)
    , inputsText =
        \options ->
            fromProgramTest ("input text into " ++ options.fieldId)
                (ProgramTest.simulateDomEvent
                    (Query.find [ Selector.id options.fieldId ])
                    ( "input", Encode.object [ ( "target", Encode.object [ ( "value", Encode.string options.input ) ] ) ] )
                )
    }


expect :
    { textIsRendered : String -> Interaction model msg effect
    , textIsNotRendered : String -> Interaction model msg effect
    , elementIsRendered : String -> Interaction model msg effect
    , isOnUrl : String -> Interaction model msg effect
    }
expect =
    { textIsRendered = \text -> fromProgramTest ("see text " ++ text) (ProgramTest.ensureViewHas [ Selector.text text ])
    , textIsNotRendered = \text -> fromProgramTest ("do not see text " ++ text) (ProgramTest.ensureViewHasNot [ Selector.text text ])
    , elementIsRendered = \id -> fromProgramTest ("see element " ++ id) (ProgramTest.ensureViewHas [ Selector.id id ])
    , isOnUrl = \url -> fromProgramTest ("be on URL " ++ url) (ProgramTest.ensureBrowserUrl (Expect.equal url))
    }


advanceTime : Int -> Interaction model msg effect
advanceTime milliseconds =
    fromProgramTest ("advance time by " ++ String.fromInt milliseconds ++ "ms")
        (if milliseconds < 0 then
            ProgramTest.fail "advanceTime" "Time cannot move backwards."

         else
            ProgramTest.advanceTime milliseconds
        )


incomingPort : String -> Encode.Value -> Interaction model msg effect
incomingPort name payload =
    fromProgramTest ("receive on port " ++ name) (ProgramTest.simulateIncomingPort name payload)


{-| Consume the oldest unconsumed matching outbound message. The continuation
can use captured query/request IDs to construct subsequent interactions.
Unrelated messages remain available to other expectations.
-}
outgoingPort :
    { name : String
    , description : String
    , decoder : Decode.Decoder value
    , matches : value -> Bool
    , then_ : value -> Interaction model msg effect
    }
    -> Interaction model msg effect
outgoingPort options =
    Interaction options.description
        (\((Session session) as original) ->
            case ProgramTest.getOutgoingPortValues options.name session.program of
                Err failed ->
                    mapProgram (\_ -> failed) original

                Ok values ->
                    let
                        consumed =
                            Dict.get options.name session.consumed |> Maybe.withDefault []

                        matched =
                            List.reverse values
                                |> List.indexedMap Tuple.pair
                                |> List.filterMap
                                    (\( index, wire ) ->
                                        if List.member index consumed then
                                            Nothing

                                        else
                                            Decode.decodeValue options.decoder wire
                                                |> Result.toMaybe
                                                |> Maybe.andThen
                                                    (\value ->
                                                        if options.matches value then
                                                            Just ( index, value )

                                                        else
                                                            Nothing
                                                    )
                                    )
                                |> List.head
                    in
                    case matched of
                        Nothing ->
                            mapProgram
                                (ProgramTest.fail options.description
                                    ("No unconsumed matching message on " ++ options.name ++ ". Outgoing values: " ++ Encode.encode 0 (Encode.list identity values))
                                )
                                original

                        Just ( index, value ) ->
                            let
                                (Interaction _ action) =
                                    options.then_ value
                            in
                            action (Session { session | consumed = Dict.insert options.name (index :: consumed) session.consumed })
        )


type alias HttpRequest =
    { headers : List ( String, String ), body : String }


type alias HttpMatch =
    { method : String
    , url : String
    , description : String
    , matches : HttpRequest -> Bool
    }


{-| Match real pending request bodies/headers and respond to the oldest match.
-}
httpResponse : HttpMatch -> Http.Response String -> Interaction model msg effect
httpResponse matcher response =
    fromProgramTest matcher.description
        (\program ->
            case findHttp matcher program 0 of
                Nothing ->
                    missingHttp matcher program

                Just index ->
                    -- elm-program-test's response indices are ONE based.
                    ProgramTest.simulateHttpResponseAdvanced matcher.method matcher.url (index + 1) response program
        )


httpPending : HttpMatch -> Interaction model msg effect
httpPending matcher =
    fromProgramTest matcher.description
        (\program ->
            case findHttp matcher program 0 of
                Nothing ->
                    missingHttp matcher program

                Just _ ->
                    program
        )


noHttpRequest : HttpMatch -> Interaction model msg effect
noHttpRequest matcher =
    fromProgramTest ("no request: " ++ matcher.description)
        (ProgramTest.ensureHttpRequests matcher.method
            matcher.url
            (\requests ->
                Expect.equal False (List.any (\request -> matcher.matches { headers = request.headers, body = request.body }) requests)
            )
        )


findHttp : HttpMatch -> ProgramTest model msg effect -> Int -> Maybe Int
findHttp matcher program index =
    let
        probe check =
            ProgramTest.expectHttpRequests matcher.method matcher.url check program
                |> Test.Runner.getFailureReason
                |> (==) Nothing

        atIndex requests =
            List.drop index requests |> List.head
    in
    if not (probe (\requests -> Expect.equal True (List.length requests > index))) then
        Nothing

    else if
        probe
            (\requests ->
                Expect.equal True
                    (atIndex requests
                        |> Maybe.map (\request -> matcher.matches { headers = request.headers, body = request.body })
                        |> Maybe.withDefault False
                    )
            )
    then
        -- elm-program-test exposes pending requests newest first.
        Just (findHttp matcher program (index + 1) |> Maybe.withDefault index)

    else
        findHttp matcher program (index + 1)


missingHttp : HttpMatch -> ProgramTest model msg effect -> ProgramTest model msg effect
missingHttp matcher =
    ProgramTest.ensureHttpRequests matcher.method
        matcher.url
        (\requests ->
            Expect.fail
                ("No matching pending request: " ++ matcher.description ++ ". Pending bodies: " ++ String.join ", " (List.map .body requests))
        )
