module Question exposing (Answer(..), TypeSignature, ask)

{-| -}

import Editor
import Elm.CallGraph
import Elm.ProjectStatus
import Http
import Json.Decode as Decode
import Url.Builder


watchtower : List String -> List Url.Builder.QueryParameter -> String
watchtower =
    Url.Builder.crossOrigin "http://localhost:51213"


ask :
    { missingTypesignatures : String -> Cmd (Result Http.Error Answer)
    , health : Cmd (Result Http.Error Answer)
    , status : Cmd (Result Http.Error Answer)
    , callgraph : String -> Cmd (Result Http.Error Answer)
    }
ask =
    { missingTypesignatures =
        \path ->
            Http.get
                { url =
                    watchtower
                        [ "list-missing-signatures"
                        ]
                        [ Url.Builder.string "file" path
                        ]
                , expect =
                    Http.expectJson
                        (Result.map (MissingTypeSignatures path))
                        (Decode.list
                            decodeMissingTypesignature
                        )
                }
    , status =
        Http.get
            { url =
                watchtower
                    [ "status"
                    ]
                    []
            , expect =
                Http.expectJson
                    (Result.map Status)
                    (Decode.field "details"
                        (Decode.list
                            Elm.ProjectStatus.decodeProject
                        )
                    )
            }
    , health =
        Http.get
            { url =
                watchtower
                    [ "status"
                    ]
                    []
            , expect =
                Http.expectJson
                    (Result.map Health)
                    Decode.string
            }
    , callgraph =
        \filepath ->
            Http.get
                { url =
                    watchtower
                        [ "callgraph"
                        ]
                        [ Url.Builder.string "file" filepath ]
                , expect =
                    Http.expectJson
                        (Result.map CallGraph)
                        (Elm.CallGraph.decode filepath)
                }
    }


type Answer
    = MissingTypeSignatures String (List TypeSignature)
    | Health String
    | Status (List Elm.ProjectStatus.Project)
    | CallGraph Elm.CallGraph.CallGraph


type alias TypeSignature =
    { name : String
    , region : Editor.Region
    , signature : String
    }


decodeMissingTypesignature : Decode.Decoder TypeSignature
decodeMissingTypesignature =
    Decode.map3 TypeSignature
        (Decode.field "name" Decode.string)
        (Decode.field "region" Editor.decodeRegion)
        (Decode.field "signature" Decode.string)
