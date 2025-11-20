port module Ui.Interactive.Controls exposing
    ( listen
    , propertyUpdated
    , view
    )

{-| Interactive controls UI and ports (listen/send).
-}

import Data.Controls as Controls
import Dict
import Effect
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import Listen


port onControlsUpdated : (Decode.Value -> msg) -> Sub msg


listen :
    ({ filepath : String, controls : Controls.Controls } -> msg)
    -> Listen.Listen msg
listen toMsg =
    let
        decoder =
            Decode.map2
                (\filepath controls -> toMsg { filepath = filepath, controls = controls })
                (Decode.field "filepath" Decode.string)
                (Decode.field "controls" Controls.decode)
    in
    Listen.OnFromJs
        { portName = "onControlsUpdated"
        , subscription =
            onControlsUpdated (Decode.decodeValue decoder)
        }


port interactivePropertyUpdated : Encode.Value -> Cmd msg


propertyUpdated :
    { filepath : String, key : String, value : Controls.Value }
    -> Effect.Effect msg
propertyUpdated { filepath, key, value } =
    Effect.SendToWorld
        { toPort = interactivePropertyUpdated
        , portName = "interactivePropertyUpdated"
        , payload =
            Encode.object
                [ ( "filepath", Encode.string filepath )
                , ( "key", Encode.string key )
                , ( "value", Controls.encodeValue value )
                ]
        }


{-| Render a simple UI for controls. This is a minimal first pass.
-}
view :
    (String -> Controls.Value -> msg)
    -> Controls.Controls
    -> Html.Html msg
view onChange controls =
    controls.data
        |> Dict.values
        |> List.map (viewInput onChange)
        |> Html.div [ Attr.style "display" "flex", Attr.style "flexDirection" "column", Attr.style "gap" "8px" ]


viewInput :
    (String -> Controls.Value -> msg)
    -> Controls.Input
    -> Html.Html msg
viewInput onChange input =
    case input.input of
        Controls.Str str ->
            Html.div []
                [ Html.label [] [ Html.text input.name ]
                , Html.input
                    [ Attr.type_ "text"
                    , Attr.value str
                    , Events.onInput (\s -> onChange input.path (Controls.StringValue s))
                    ]
                    []
                ]

        Controls.Boolean bool ->
            Html.label []
                [ Html.input
                    [ Attr.type_ "checkbox"
                    , Attr.checked bool
                    , Events.onCheck (\b -> onChange input.path (Controls.BoolValue b))
                    ]
                    []
                , Html.text (" " ++ input.name)
                ]

        Controls.Number details ->
            let
                asString =
                    details.value |> Maybe.map String.fromFloat |> Maybe.withDefault ""
            in
            Html.div []
                [ Html.label [] [ Html.text input.name ]
                , Html.input
                    [ Attr.type_ "number"
                    , Attr.value asString
                    , Events.onInput
                        (\s ->
                            case String.toFloat s of
                                Just f ->
                                    onChange input.path (Controls.FloatValue f)

                                Nothing ->
                                    onChange input.path (Controls.FloatValue 0)
                        )
                    ]
                    []
                ]

        Controls.OneOf details ->
            Html.div []
                [ Html.label [] [ Html.text input.name ]
                , Html.select
                    [ Events.onInput (\s -> onChange input.path (Controls.StringValue s))
                    ]
                    (details.options
                        |> List.map
                            (\opt ->
                                Html.option
                                    [ Attr.value opt
                                    , Attr.selected (details.value == Just opt)
                                    ]
                                    [ Html.text opt ]
                            )
                    )
                ]

        Controls.ManyOf items ->
            Html.div []
                (Html.div [] [ Html.text input.name ]
                    :: List.map
                        (\it ->
                            Html.label []
                                [ Html.input
                                    [ Attr.type_ "checkbox"
                                    , Attr.checked it.selected
                                    , Events.onCheck
                                        (\_ ->
                                            onChange input.path (Controls.StringValue it.value)
                                        )
                                    ]
                                    []
                                , Html.text (" " ++ it.value)
                                ]
                        )
                        items
                )

