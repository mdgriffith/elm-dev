port module Ports exposing (Incoming(..), Outgoing(..), incoming, outgoing, Warning(..))

{-|

@docs Incoming, Outgoing, incoming, outgoing, Warning

-}

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


type Warning
    = UnusedVaraible
        { region : Editor.Region
        , context : String
        , name : String
        }
    | MissingAnnotation
        { region : Editor.Region
        , name : String
        , signature : String
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
                                UnusedVaraible
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

                    _ ->
                        Decode.fail "Unknown warning"
            )
