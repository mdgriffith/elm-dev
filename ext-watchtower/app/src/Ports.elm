port module Ports exposing (Incoming(..), Outgoing(..), incoming, outgoing)

{-|

@docs Incoming, Outgoing, incoming, outgoing

-}

import Editor
import Elm
import Json.Decode as Decode
import Json.Encode


port editorChange : (Json.Encode.Value -> msg) -> Sub msg


port notify : Json.Encode.Value -> Cmd msg


incoming : (Result Decode.Error Incoming -> msg) -> Sub msg
incoming toMsg =
    editorChange (toMsg << Decode.decodeValue incomingDecoder)


outgoing : Outgoing -> Cmd msg
outgoing out =
    notify (encodeOutgoing out)


type Incoming
    = VisibleRangeUpdated
        { fileName : String
        , ranges : List Editor.Range
        }
    | SelectionUpdated
        { fileName : String
        , selections : List Editor.Selection
        }
    | ActiveEditorUpdated
        { fileName : String
        , ranges : List Editor.Range
        , selections : List Editor.Selection
        }
    | VisiblEditorsUpdated
        (List
            { fileName : String
            , ranges : List Editor.Range
            , selections : List Editor.Selection
            }
        )
    | Errors Elm.Error
    | WorkspaceFolders (List Editor.Workspace)


type Outgoing
    = Goto Editor.Location


encodeOutgoing : Outgoing -> Json.Encode.Value
encodeOutgoing out =
    case out of
        Goto location ->
            Json.Encode.object
                [ ( "command", Json.Encode.string "Goto" )
                , ( "path", Json.Encode.string location.file )
                , ( "region", Editor.encodeRegion location.region )
                ]


incomingDecoder : Decode.Decoder Incoming
incomingDecoder =
    Decode.field "command" Decode.string
        |> Decode.andThen
            (\command ->
                case Debug.log "found command" command of
                    "VisibleEditorsUpdated" ->
                        Decode.map VisiblEditorsUpdated
                            (Decode.field "visible"
                                (Decode.list Editor.decodeEditor)
                            )

                    "VisibleRangeUpdated" ->
                        Decode.map2
                            (\fileName ranges ->
                                VisibleRangeUpdated
                                    { fileName = fileName
                                    , ranges = ranges
                                    }
                            )
                            (Decode.field "fileName" Decode.string)
                            (Decode.field "ranges" (Decode.list Editor.decodeRegion))

                    "SelectionUpdated" ->
                        Decode.map2
                            (\fileName selections ->
                                SelectionUpdated
                                    { fileName = fileName
                                    , selections = selections
                                    }
                            )
                            (Decode.field "fileName" Decode.string)
                            (Decode.field "selections" (Decode.list Editor.selection))

                    "ActiveEditorUpdated" ->
                        Decode.map ActiveEditorUpdated
                            Editor.decodeEditor

                    "Errors" ->
                        Decode.map Errors
                            (Decode.field "json" Elm.decodeError)

                    "WorkspaceFolders" ->
                        Decode.map WorkspaceFolders
                            (Decode.field "folders" (Decode.list Editor.decodeWorkspaceFolder))

                    _ ->
                        let
                            _ =
                                Debug.log "UNRECOGNIZED COMMAND" command
                        in
                        Decode.fail "UNRECOGNIZED COMMAND"
            )
