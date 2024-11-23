port module Ports exposing
    ( Incoming(..), Outgoing(..), incoming, outgoing, Warning(..)
    , Fact(..), FactDetails(..), Module
    , Server, ServerStatus(..), Source(..), Type, UnionDetails
    )

{-|

@docs Incoming, Outgoing, incoming, outgoing, Warning

@docs Fact, FactDetails, Module

-}

import Editor
import Elm.CallGraph
import Elm.ProjectStatus
import Elm.Type
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
        { visible : List Editor.Editor
        }
    | ProjectsStatusUpdated (List Elm.ProjectStatus.Project)
    | WarningsUpdated
        { filepath : String
        , warnings : List Warning
        }
    | CallGraphReceived Elm.CallGraph.CallGraph
    | ExplanationReceived
        { modulename : String
        , filepath : String
        , definition : ExplainedDefinition
        , facts : List Fact
        }
    | ServerStatusUpdated Server


type alias Server =
    { status : ServerStatus }


type ServerStatus
    = Connected ServerInfo
    | Connecting
    | Disconnected


type alias ServerInfo =
    { version : String
    , port_ : String
    , host : String
    }


type alias ExplainedDefinition =
    { name : String
    , type_ : Maybe Type
    , recursive : Bool
    , range : Editor.Region
    }


type Fact
    = Fact
        { source : Source
        , name : String
        , details : FactDetails
        }


type FactDetails
    = Value ValueDetails
    | Union UnionDetails
    | Alias AliasDetails
    | Def DefDetails


type alias ValueDetails =
    { type_ : Type
    }


type alias Type =
    { string : String
    , value : Elm.Type.Type
    }


type alias UnionDetails =
    { name : String
    , args : List String
    , comment : Maybe String
    , cases : List Variant
    }


type alias Variant =
    { name : String
    , types_ : List Type
    }


type alias AliasDetails =
    { name : String
    , comment : Maybe String
    , type_ : Type
    }


type alias DefDetails =
    { type_ : Maybe Type
    , comment : String
    }


type Source
    = SourceLet
    | SourceDeclaration
    | External Module


type alias Module =
    { pkg : String
    , name : String
    }


decodeFact : Decode.Decoder Fact
decodeFact =
    Decode.map3
        (\source name details ->
            Fact
                { source = source
                , name = convertSpecialName source name
                , details = details
                }
        )
        (Decode.field "source" decodeSource)
        (Decode.field "name" Decode.string)
        decodeFactDetails


convertSpecialName : Source -> String -> String
convertSpecialName source name =
    case source of
        External mod ->
            if mod.pkg == "elm/core" then
                case name of
                    "apL" ->
                        "<|"

                    "apR" ->
                        "|>"

                    "composeR" ->
                        ">>"

                    "composeL" ->
                        "<<"

                    "cons" ->
                        "::"

                    "fdiv" ->
                        "/"

                    "idiv" ->
                        "//"

                    "mul" ->
                        "*"

                    "sub" ->
                        "-"

                    "pos" ->
                        "^"

                    "lt" ->
                        "<"

                    "lte" ->
                        "<="

                    "gt" ->
                        ">"

                    "gte" ->
                        ">="

                    "eq" ->
                        "=="

                    _ ->
                        name

            else
                name

        _ ->
            name


decodeFactDetails : Decode.Decoder FactDetails
decodeFactDetails =
    Decode.oneOf
        [ Decode.map Union
            (Decode.field "union"
                (Decode.map4 UnionDetails
                    (Decode.field "name" Decode.string)
                    (Decode.field "args" (Decode.succeed []))
                    (Decode.field "comment" (Decode.nullable Decode.string))
                    (Decode.field "cases"
                        (Decode.list decodeCase)
                    )
                )
            )
        , Decode.map Alias
            (Decode.field "alias"
                (Decode.map3 AliasDetails
                    (Decode.field "name" Decode.string)
                    (Decode.field "comment" (Decode.nullable Decode.string))
                    (Decode.field "type" decodeType)
                )
            )
        , Decode.map Def
            (Decode.field "definition"
                (Decode.map2 DefDetails
                    (Decode.field "type" (Decode.nullable decodeType))
                    (Decode.field "comment" Decode.string)
                )
            )
        , Decode.map ValueDetails
            (Decode.field "type" decodeType)
            |> Decode.map Value
        ]


decodeCase : Decode.Decoder Variant
decodeCase =
    Decode.map2 Variant
        (Decode.index 0 Decode.string)
        (Decode.index 1 (Decode.list decodeType))


decodeType : Decode.Decoder Type
decodeType =
    Decode.string
        |> Decode.andThen
            (\str ->
                Decode.map
                    (\typeValue ->
                        { string = str
                        , value = typeValue
                        }
                    )
                    Elm.Type.decoder
            )


decodeSource : Decode.Decoder Source
decodeSource =
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                (\str ->
                    case str of
                        "let" ->
                            Decode.succeed SourceLet

                        "declaration" ->
                            Decode.succeed SourceDeclaration

                        _ ->
                            Decode.fail "Data source is not local"
                )
        , Decode.map External decodeModule
        ]


decodeModule : Decode.Decoder Module
decodeModule =
    Decode.map2 Module
        (Decode.field "pkg" Decode.string)
        (Decode.field "module" Decode.string)


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
    = ConnectToServer
    | RequestProjectList
    | Goto Editor.Location
    | FillTypeSignatures String
    | WindowMinimize
    | WindowMaximize
    | WindowClose


encodeOutgoing : Outgoing -> Json.Encode.Value
encodeOutgoing out =
    case out of
        ConnectToServer ->
            Json.Encode.object
                [ ( "msg", Json.Encode.string "ConnectToServer" )
                ]

        RequestProjectList ->
            Json.Encode.object
                [ ( "msg", Json.Encode.string "RequestProjectList" )
                ]

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

        WindowMinimize ->
            Json.Encode.object [ ( "msg", Json.Encode.string "WindowMinimize" ) ]

        WindowMaximize ->
            Json.Encode.object [ ( "msg", Json.Encode.string "WindowMaximize" ) ]

        WindowClose ->
            Json.Encode.object [ ( "msg", Json.Encode.string "WindowClose" ) ]


incomingDecoder : Decode.Decoder Incoming
incomingDecoder =
    Decode.field "msg" Decode.string
        |> Decode.andThen
            (\msg ->
                case msg of
                    "Server" ->
                        Decode.field "details"
                            (Decode.field "status" Decode.string
                                |> Decode.andThen
                                    (\status ->
                                        case status of
                                            "Connected" ->
                                                Decode.map (\info -> ServerStatusUpdated { status = Connected info })
                                                    (Decode.map3 ServerInfo
                                                        (Decode.field "version" Decode.string)
                                                        (Decode.field "port" Decode.string)
                                                        (Decode.field "host" Decode.string)
                                                    )

                                            "Disconnected" ->
                                                Decode.succeed (ServerStatusUpdated { status = Disconnected })

                                            _ ->
                                                Decode.fail ("Unknown server status: " ++ status)
                                    )
                            )

                    "Status" ->
                        Decode.map ProjectsStatusUpdated
                            (Decode.field "details"
                                (Decode.list
                                    Elm.ProjectStatus.decodeProject
                                )
                            )

                    "EditorVisibilityChanged" ->
                        Decode.field "details"
                            (Decode.map
                                (\vis ->
                                    VisibleEditorsUpdated { visible = vis }
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
                            (Decode.map
                                CallGraphReceived
                                (Decode.field "filepath" Decode.string
                                    |> Decode.andThen
                                        (\filepath ->
                                            Decode.field "callgraph" (Elm.CallGraph.decode filepath)
                                        )
                                )
                            )

                    "Explanation" ->
                        Decode.field "details"
                            (Decode.map4
                                (\modName filepath definition facts ->
                                    ExplanationReceived
                                        { modulename = modName
                                        , filepath = filepath
                                        , definition = definition
                                        , facts = facts
                                        }
                                )
                                (Decode.field "explanation"
                                    (Decode.field "moduleName" Decode.string)
                                )
                                (Decode.field "filepath"
                                    Decode.string
                                )
                                (Decode.field "explanation"
                                    (Decode.field "definition"
                                        decodeExplanationDefinition
                                    )
                                )
                                (Decode.field "explanation"
                                    (Decode.field "facts"
                                        (Decode.list decodeFact)
                                    )
                                )
                            )

                    _ ->
                        Decode.value
                            |> Decode.andThen
                                (\val ->
                                    Decode.fail "UNRECOGNIZED INCOMING MSG"
                                )
            )


decodeExplanationDefinition : Decode.Decoder ExplainedDefinition
decodeExplanationDefinition =
    Decode.map4 ExplainedDefinition
        (Decode.field "name" Decode.string)
        (Decode.field "type"
            (Decode.oneOf
                [ Decode.null Nothing
                , Decode.map Just decodeType
                ]
            )
        )
        (Decode.field "recursive" Decode.bool)
        (Decode.field "region" Editor.decodeRegion)


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
