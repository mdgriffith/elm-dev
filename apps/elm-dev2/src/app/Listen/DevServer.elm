port module Listen.DevServer exposing
    ( listen, Event(..)
    , ServerStatus(..), ServerInfo
    , Package
    )

{-| Listen for updates from the dev server.

@docs listen, Event

@docs ServerStatus, ServerInfo

-}

import Data.Editor as Editor
import Data.ProjectStatus as ProjectStatus
import Dict
import Elm.Docs
import Elm.Package
import Elm.Project
import Json.Decode as Decode
import Listen
import Platform.Sub


port devServer : (Decode.Value -> msg) -> Platform.Sub.Sub msg


listen : (Event -> msg) -> Listen.Listen msg
listen toMsg =
    Listen.OnFromJs
        { portName = "devServer"
        , subscription =
            devServer
                (Decode.decodeValue (Decode.map toMsg eventDecoder))
        }


type Event
    = VisibleEditorsUpdated
        { visible : List Editor.Editor
        }
    | ProjectsStatusUpdated (List ProjectStatus.Project)
    | ServiceStatusUpdated
        { sessions : Dict.Dict String Int
        , editorsOpen : Dict.Dict String Int
        }
    | WarningsUpdated
        { filepath : String
        , warnings : List ProjectStatus.Warning
        }
    | ServerStatusUpdated Server
    | PackageUpdated Package


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


type alias Package =
    { readme : Maybe String
    , name : Elm.Package.Name
    , info : Elm.Project.Project
    , version : String
    , modules : List Elm.Docs.Module
    }


eventDecoder : Decode.Decoder Event
eventDecoder =
    Decode.field "msg" Decode.string
        |> Decode.andThen
            (\msg ->
                case Debug.log "EVENT DECODER" msg of
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
                                    ProjectStatus.decodeProject
                                )
                            )

                    "ServiceStatus" ->
                        Decode.field "details"
                            (Decode.map2
                                (\sessions editorsOpen ->
                                    ServiceStatusUpdated
                                        { sessions = sessions
                                        , editorsOpen = editorsOpen
                                        }
                                )
                                (Decode.field "sessions" (Decode.dict Decode.int))
                                (Decode.field "editorsOpen" (Decode.dict Decode.int))
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
                                    (Decode.list ProjectStatus.decodeWarning)
                                )
                            )

                    "PackageUpdated" ->
                        Decode.map PackageUpdated
                            (Decode.field "details"
                                (Decode.map5 Package
                                    (Decode.field "readme" (Decode.maybe Decode.string))
                                    (Decode.field "name"
                                        Elm.Package.decoder
                                    )
                                    (Decode.field "elmJson"
                                        Elm.Project.decoder
                                    )
                                    (Decode.field "version" Decode.string)
                                    (Decode.field "docs" (Decode.list Elm.Docs.decoder))
                                )
                            )

                    _ ->
                        Decode.value
                            |> Decode.andThen
                                (\val ->
                                    let
                                        _ =
                                            Debug.log "UNRECOGNIZED INCOMING MSG" val
                                    in
                                    Decode.fail "UNRECOGNIZED INCOMING MSG"
                                )
            )



-- placeholderProject : Elm.Project.Project
-- placeholderProject =
--     Elm.Project.Package
--         { name = Elm.Package.Name "placeholder"
--         , version = "0.0.0"
--         , exposed = Elm.Project.ExposedList []
--         , deps = Elm.Project.Deps [] []
--         , elm = Elm.Constraint.Constraint "0.0.0"
--         , license = Elm.License.License "placeholder"
--         , summary = "placeholder"
--         , testDeps = Elm.Project.Deps [] []
--         }

