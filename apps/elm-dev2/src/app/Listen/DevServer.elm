port module Listen.DevServer exposing
    ( listen, Event(..)
    , ServerStatus(..), ServerInfo
    )

{-| Listen for updates from the dev server.

@docs listen, Event

@docs ServerStatus, ServerInfo

-}

import Data.Editor as Editor
import Data.ProjectStatus as ProjectStatus
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
    | WarningsUpdated
        { filepath : String
        , warnings : List ProjectStatus.Warning
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


eventDecoder : Decode.Decoder Event
eventDecoder =
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
                                    ProjectStatus.decodeProject
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
                                    (Decode.list ProjectStatus.decodeWarning)
                                )
                            )

                    _ ->
                        Decode.value
                            |> Decode.andThen
                                (\val ->
                                    Decode.fail "UNRECOGNIZED INCOMING MSG"
                                )
            )
