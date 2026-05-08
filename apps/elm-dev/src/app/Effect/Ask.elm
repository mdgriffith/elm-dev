port module Effect.Ask exposing
    ( projectList
    , interactiveExamples, moduleRequested, packageRequested
    )

{-|

@docs projectList

-}

import Data.ProjectStatus as ProjectStatus
import Effect
import Json.Encode as Encode


port ask : Encode.Value -> Cmd msg


{-| Ask the host to fetch the project list.

The host (TS/Tauri) is responsible for resolving the base URL and will
return the data via the `devServer` port with a `{"msg":"Status"}` payload.

-}
projectList : Effect.Effect msg
projectList =
    Effect.SendToWorld
        { toPort = ask
        , portName = "ask"
        , payload =
            Encode.object
                [ ( "route", Encode.string "ProjectList" )
                ]
        }


{-| Request loading a package by full name: "owner/project@version" or "owner/project".
-}
packageRequested : ProjectStatus.PackageInfo -> Effect.Effect msg
packageRequested { name, version } =
    Effect.SendToWorld
        { toPort = ask
        , portName = "ask"
        , payload =
            Encode.object
                [ ( "route", Encode.string "PackageRequested" )
                , ( "name", Encode.string name )
                , ( "version", Encode.string version )
                ]
        }


{-| Request loading docs for a single project module file.
-}
moduleRequested : { dir : String, file : String } -> Effect.Effect msg
moduleRequested { dir, file } =
    Effect.SendToWorld
        { toPort = ask
        , portName = "ask"
        , payload =
            Encode.object
                [ ( "route", Encode.string "ModuleRequested" )
                , ( "dir", Encode.string dir )
                , ( "file", Encode.string file )
                ]
        }


{-| Request interactive examples for a given local module file.
-}
interactiveExamples : { dir : String, file : String } -> Effect.Effect msg
interactiveExamples { dir, file } =
    Effect.SendToWorld
        { toPort = ask
        , portName = "ask"
        , payload =
            Encode.object
                [ ( "route", Encode.string "InteractiveExamples" )
                , ( "dir", Encode.string dir )
                , ( "file", Encode.string file )
                ]
        }
