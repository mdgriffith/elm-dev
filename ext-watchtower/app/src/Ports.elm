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
    = VisibleEditorsUpdated
        { active : Maybe Editor.Editor
        , visible : List Editor.Editor
        }
    | ProjectsStatusUpdated (List Elm.Status)


type Outgoing
    = Goto Editor.Location


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


incomingDecoder : Decode.Decoder Incoming
incomingDecoder =
    Decode.field "msg" Decode.string
        |> Decode.andThen
            (\msg ->
                case Debug.log "found msg" msg of
                    "Status" ->
                        Decode.map ProjectsStatusUpdated
                            (Decode.field "details" (Decode.list Elm.decodeStatus))

                    "Visible" ->
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

                    _ ->
                        let
                            _ =
                                Debug.log "UNRECOGNIZED INCOMING MSG" msg
                        in
                        Decode.fail "UNRECOGNIZED INCOMING MSG"
            )
