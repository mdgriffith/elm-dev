module Data.Question exposing
    ( health, status
    , callgraph, missingTypesignatures, TypeSignature
    )

{-|

@docs health, status

@docs callgraph, missingTypesignatures, TypeSignature

-}

import Data.CallGraph
import Data.Editor
import Http
import Json.Decode as Decode
import Store.Projects
import Url.Builder


watchtower : List String -> List Url.Builder.QueryParameter -> String
watchtower =
    Url.Builder.crossOrigin "http://localhost:51213"


type alias MissingTypeSignatures =
    { path : String
    , signatures : List TypeSignature
    }


missingTypesignatures : String -> Cmd (Result Http.Error MissingTypeSignatures)
missingTypesignatures path =
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


type alias Status =
    { projects : List Store.Projects.Project }


status : Cmd (Result Http.Error Status)
status =
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


health : Cmd (Result Http.Error String)
health =
    Http.get
        { url =
            watchtower
                [ "health"
                ]
                []
        , expect =
            Http.expectJson
                (Result.map Health)
                Decode.string
        }


callgraph : String -> Cmd (Result Http.Error Data.CallGraph.CallGraph)
callgraph filepath =
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


type alias TypeSignature =
    { name : String
    , region : Data.Editor.Region
    , signature : String
    }


decodeMissingTypesignature : Decode.Decoder TypeSignature
decodeMissingTypesignature =
    Decode.map3 TypeSignature
        (Decode.field "name" Decode.string)
        (Decode.field "region" Editor.decodeRegion)
        (Decode.field "signature" Decode.string)
