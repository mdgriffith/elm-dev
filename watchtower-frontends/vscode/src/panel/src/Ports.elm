port module Ports exposing
    ( Incoming(..), Outgoing(..), incoming, outgoing, Warning(..)
    , CallGraphNode, Call, CallType(..)
    , Fact, Module
    )

{-|

@docs Incoming, Outgoing, incoming, outgoing, Warning

@docs CallGraphNode, Call, CallType

@docs Fact, Module

-}

import Dict exposing (Dict)
import Editor
import Elm
import Json.Decode as Decode
import Json.Encode


port toElm : (Json.Encode.Value -> msg) -> Sub msg


port toWorld : Json.Encode.Value -> Cmd msg


incoming : (Result Decode.Error Incoming -> msg) -> Sub msg
incoming toMsg =
    toElm (toMsg << Decode.decodeValue incomingDecoder)


outgoing : Outgoing -> Cmd msg
outgoing out =
    toWorld (encodeOutgoing out)


type Incoming
    = VisibleEditorsUpdated
        { active : Maybe Editor.Editor
        , visible : List Editor.Editor
        }
    | ProjectsStatusUpdated (List Elm.Status)
    | WarningsUpdated
        { filepath : String
        , warnings : List Warning
        }
    | CallGraphReceived
        { filepath : String
        , callgraph : List CallGraphNode
        }
    | ExplanationReceived
        { filepath : String
        , facts : List Fact
        }
    | ServerConnection { connected : Bool }


type alias Fact =
    { module_ : Module
    , name : String
    , type_ : String
    }


type alias Module =
    { pkg : String
    , name : String
    }


decodeFact : Decode.Decoder Fact
decodeFact =
    Decode.map3 Fact
        (Decode.field "module" decodeModule)
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)


decodeModule : Decode.Decoder Module
decodeModule =
    Decode.map2 Module
        (Decode.field "pkg" Decode.string)
        (Decode.field "module" Decode.string)


decodeCallGraphNode : Decode.Decoder CallGraphNode
decodeCallGraphNode =
    Decode.map4 CallGraphNode
        (Decode.field "id" Decode.string)
        (Decode.field "recursive" Decode.bool)
        (Decode.field "calls" (Decode.list decodeCall))
        (Decode.field "callers" (Decode.list decodeCall))


decodeCall : Decode.Decoder Call
decodeCall =
    Decode.map2 Call
        (Decode.field "id" Decode.string)
        (Decode.field "callType" decodeCallType)


decodeCallType : Decode.Decoder CallType
decodeCallType =
    enum
        (Dict.fromList
            [ Tuple.pair "local" Local
            , Tuple.pair "top-level" TopLevel
            , Tuple.pair "foreign" Foreign
            , Tuple.pair "constructor" Constructor
            , Tuple.pair "debug" Debug
            , Tuple.pair "operator" Operator
            ]
        )


enum : Dict String val -> Decode.Decoder val
enum vals =
    Decode.string
        |> Decode.andThen
            (\str ->
                case Dict.get str vals of
                    Nothing ->
                        Decode.fail ("Don't recognize " ++ str)

                    Just val ->
                        Decode.succeed val
            )


type alias CallGraphNode =
    { id : String
    , recursive : Bool
    , calls : List Call
    , callers : List Call
    }


type alias Call =
    { id : String
    , callType : CallType
    }


type CallType
    = Local
    | TopLevel
    | Foreign
    | Constructor
    | Debug
    | Operator


type Warning
    = UnusedVariable
        { region : Editor.Region
        , context : String
        , name : String
        }
    | MissingAnnotation
        { region : Editor.Region
        , name : String
        , signature : String
        }
    | UnusedImport
        { region : Editor.Region
        , name : String
        }


type Outgoing
    = Goto Editor.Location
    | FillTypeSignatures String


encodeOutgoing : Outgoing -> Json.Encode.Value
encodeOutgoing out =
    case out of
        Goto location ->
            Json.Encode.object
                [ ( "msg", Json.Encode.string "Jump" )
                , ( "details"
                  , Json.Encode.object
                        [ ( "path", Json.Encode.string location.file )
                        , ( "region", Editor.encodeRegion location.region )
                        ]
                  )
                ]

        FillTypeSignatures path ->
            Json.Encode.object
                [ ( "msg", Json.Encode.string "InsertMissingTypeSignatures" )
                , ( "details"
                  , Json.Encode.object
                        [ ( "path", Json.Encode.string path )
                        ]
                  )
                ]


incomingDecoder : Decode.Decoder Incoming
incomingDecoder =
    Decode.field "msg" Decode.string
        |> Decode.andThen
            (\msg ->
                case msg of
                    "Status" ->
                        Decode.map ProjectsStatusUpdated
                            (Decode.field "details" (Decode.list (Decode.map .status Elm.decodeProject)))

                    "EditorVisibilityChanged" ->
                        Decode.field "details"
                            (Decode.map2
                                (\active vis ->
                                    VisibleEditorsUpdated
                                        { active = active
                                        , visible = vis
                                        }
                                )
                                (Decode.field "active"
                                    (Decode.nullable Editor.decodeEditor)
                                )
                                (Decode.field "visible"
                                    (Decode.list Editor.decodeEditor)
                                )
                            )

                    "Warnings" ->
                        Decode.field "details"
                            (Decode.map2
                                (\filepath warnings ->
                                    WarningsUpdated
                                        { filepath = filepath
                                        , warnings = warnings
                                        }
                                )
                                (Decode.field "filepath"
                                    Decode.string
                                )
                                (Decode.field "warnings"
                                    (Decode.list decodeWarning)
                                )
                            )

                    "CallGraph" ->
                        Decode.field "details"
                            (Decode.map2
                                (\filepath callgraph ->
                                    CallGraphReceived
                                        { filepath = filepath
                                        , callgraph = callgraph
                                        }
                                )
                                (Decode.field "filepath"
                                    Decode.string
                                )
                                (Decode.field "callgraph"
                                    (Decode.list decodeCallGraphNode)
                                )
                            )

                    "Explanation" ->
                        Decode.field "details"
                            (Decode.map2
                                (\filepath facts ->
                                    ExplanationReceived
                                        { filepath = filepath
                                        , facts = facts
                                        }
                                )
                                (Decode.field "filepath"
                                    Decode.string
                                )
                                (Decode.field "facts"
                                    (Decode.list decodeFact)
                                )
                            )

                    _ ->
                        let
                            _ =
                                Debug.log "UNRECOGNIZED INCOMING MSG" msg
                        in
                        Decode.fail "UNRECOGNIZED INCOMING MSG"
            )


decodeWarning : Decode.Decoder Warning
decodeWarning =
    Decode.field "warning" Decode.string
        |> Decode.andThen
            (\warning ->
                case warning of
                    "UnusedVariable" ->
                        Decode.map3
                            (\region context name ->
                                UnusedVariable
                                    { region = region
                                    , context = context
                                    , name = name
                                    }
                            )
                            (Decode.field "region"
                                Editor.decodeRegion
                            )
                            (Decode.field "context"
                                Decode.string
                            )
                            (Decode.field "name"
                                Decode.string
                            )

                    "MissingAnnotation" ->
                        Decode.map3
                            (\region signature name ->
                                MissingAnnotation
                                    { region = region
                                    , signature = signature
                                    , name = name
                                    }
                            )
                            (Decode.field "region"
                                Editor.decodeRegion
                            )
                            (Decode.field "signature"
                                Decode.string
                            )
                            (Decode.field "name"
                                Decode.string
                            )

                    "UnusedImport" ->
                        Decode.map2
                            (\region name ->
                                UnusedImport
                                    { region = region
                                    , name = name
                                    }
                            )
                            (Decode.field "region"
                                Editor.decodeRegion
                            )
                            (Decode.field "name"
                                Decode.string
                            )

                    _ ->
                        Decode.fail "Unknown warning"
            )
