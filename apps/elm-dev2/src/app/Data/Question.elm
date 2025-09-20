module Data.Question exposing (projectList, ProjectList)

{-|

@docs projectList, ProjectList

-}

import Data.CallGraph
import Data.Editor
import Data.ProjectStatus
import Effect exposing (Effect)
import Effect.Http
import Http
import Json.Decode as Decode
import Url.Builder


watchtower : String -> List String -> List Url.Builder.QueryParameter -> String
watchtower base =
    Url.Builder.crossOrigin base



-- type alias MissingTypeSignatures =
--     { path : String
--     , signatures : List TypeSignature
--     }
-- missingTypesignatures : String -> Cmd (Result Http.Error MissingTypeSignatures)
-- missingTypesignatures path =
--     Http.get
--         { url =
--             watchtower
--                 [ "list-missing-signatures"
--                 ]
--                 [ Url.Builder.string "file" path
--                 ]
--         , expect =
--             Http.expectJson
--                 (Result.map (MissingTypeSignatures path))
--                 (Decode.list
--                     decodeMissingTypesignature
--                 )
--         }


type alias ProjectList =
    { projects : List Data.ProjectStatus.Project }


projectList : String -> (Result Http.Error ProjectList -> msg) -> Effect msg
projectList base toMsg =
    -- Requests are now done via Tauri HTTP plugin in TS and pushed via devServer port.
    -- Keep a no-op effect so call sites remain unchanged.
    Effect.none



-- health : Cmd (Result Http.Error String)
-- health =
--     Effect.Http.get
--         { url =
--             watchtower
--                 [ "health"
--                 ]
--                 []
--         , expect =
--             Http.expectJson
--                 (Result.map Health)
--                 Decode.string
--         }
-- callgraph : String -> Cmd (Result Http.Error Data.CallGraph.CallGraph)
-- callgraph filepath =
--     Http.get
--         { url =
--             watchtower
--                 [ "callgraph"
--                 ]
--                 [ Url.Builder.string "file" filepath ]
--         , expect =
--             Http.expectJson
--                 (Result.map CallGraph)
--                 (Elm.CallGraph.decode filepath)
--         }
-- type alias TypeSignature =
--     { name : String
--     , region : Data.Editor.Region
--     , signature : String
--     }
-- decodeMissingTypesignature : Decode.Decoder TypeSignature
-- decodeMissingTypesignature =
--     Decode.map3 TypeSignature
--         (Decode.field "name" Decode.string)
--         (Decode.field "region" Editor.decodeRegion)
--         (Decode.field "signature" Decode.string)
