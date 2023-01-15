module Ui.Type exposing (view)

import Element as Ui
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import Elm.Type
import Ui


view : Elm.Type.Type -> Ui.Element msg
view tipe =
    if linearWidth tipe > 50 then
        viewMultiline 4 tipe

    else
        viewSingleLine tipe


linearWidth : Elm.Type.Type -> Int
linearWidth tipe =
    case tipe of
        Elm.Type.Var var ->
            String.length var

        Elm.Type.Lambda one two ->
            linearWidth one + linearWidth two + 4

        Elm.Type.Tuple vals ->
            4
                + sumWith linearWidth vals
                + spacingWidth 2 vals

        Elm.Type.Type typename varTypes ->
            String.length typename
                + sumWith linearWidth varTypes
                + spacingWidth 1 varTypes

        Elm.Type.Record fields Nothing ->
            recordWidth
                + sumWith
                    (\( fieldName, fieldType ) ->
                        4 + String.length fieldName + linearWidth fieldType
                    )
                    fields
                + spacingWidth 2 fields

        Elm.Type.Record fields (Just extensibleName) ->
            String.length extensibleName
                + recordWidth
                + sumWith
                    (\( fieldName, fieldType ) ->
                        3 + String.length fieldName + linearWidth fieldType
                    )
                    fields
                + spacingWidth 2 fields


sumWith : (a -> Int) -> List a -> Int
sumWith toSize items =
    List.foldl (\v sum -> sum + toSize v) 0 items


spacingWidth : Int -> List a -> Int
spacingWidth spacing items =
    case items of
        [] ->
            0

        [ _ ] ->
            0

        [ _, _ ] ->
            spacing

        [ _, _, _ ] ->
            spacing + spacing

        _ ->
            (List.length items - 1) * spacing


{-| curly braces and a space on each side
-}
recordWidth : Int
recordWidth =
    4


viewSingleLine : Elm.Type.Type -> Ui.Element msg
viewSingleLine tipe =
    case tipe of
        Elm.Type.Var var ->
            Ui.text var

        Elm.Type.Lambda one two ->
            Ui.row []
                [ viewSingleLine one
                , Ui.text " -> "
                , viewSingleLine two
                ]

        Elm.Type.Tuple vals ->
            Ui.row []
                [ Ui.text "( "
                , Ui.row []
                    (List.map viewSingleLine vals
                        |> List.intersperse (Ui.text ", ")
                    )
                , Ui.text " )"
                ]

        Elm.Type.Type typename [] ->
            Ui.text typename

        Elm.Type.Type typename varTypes ->
            Ui.row []
                [ Ui.text typename
                , Ui.text " "
                , Ui.row []
                    (List.map viewSingleLine varTypes
                        |> List.intersperse (Ui.text " ")
                    )
                ]

        Elm.Type.Record fields Nothing ->
            Ui.row []
                [ Ui.text "{ "
                , Ui.row []
                    (List.map viewSingleLineField fields
                        |> List.intersperse (Ui.text ", ")
                    )
                , Ui.text " }"
                ]

        Elm.Type.Record fields (Just extensibleName) ->
            Ui.row []
                [ Ui.text ("{ " ++ extensibleName ++ " | ")
                , Ui.row []
                    (List.map viewSingleLineField fields
                        |> List.intersperse (Ui.text ", ")
                    )
                , Ui.text " }"
                ]


viewSingleLineField ( name, type_ ) =
    Ui.row []
        [ Ui.text name
        , Ui.text " : "
        , viewSingleLine type_
        ]


viewMultiline : Int -> Elm.Type.Type -> Ui.Element msg
viewMultiline indent tipe =
    case tipe of
        Elm.Type.Var var ->
            Ui.text var

        Elm.Type.Lambda one two ->
            Ui.column []
                [ viewMultiline indent one
                , Ui.row [] [ Ui.text " -> ", viewMultiline indent two ]
                ]

        Elm.Type.Tuple vals ->
            Ui.row []
                [ Ui.text "( "
                , Ui.row []
                    (List.map (viewMultiline indent) vals
                        |> List.intersperse (Ui.text ", ")
                    )
                , Ui.text " )"
                ]

        Elm.Type.Type typename [] ->
            Ui.text typename

        Elm.Type.Type typename varTypes ->
            Ui.row []
                [ Ui.text typename
                , Ui.text " "
                , Ui.row []
                    (List.map (viewMultiline indent) varTypes
                        |> List.intersperse (Ui.text " ")
                    )
                ]

        Elm.Type.Record fields Nothing ->
            Ui.column [ indentPadding indent, Ui.space.sm ]
                (List.foldl
                    (\field ( isFirst, gathered ) ->
                        ( False, viewField isFirst indent field :: gathered )
                    )
                    ( True, [] )
                    fields
                    |> Tuple.second
                    |> (::) (Ui.text "}")
                    |> List.reverse
                )

        Elm.Type.Record fields (Just extensibleName) ->
            Ui.column [ indentPadding indent, Ui.space.sm ]
                (List.foldl
                    (\field ( isFirst, gathered ) ->
                        ( False, viewField isFirst indent field :: gathered )
                    )
                    ( True, [] )
                    fields
                    |> Tuple.second
                    |> (::) (Ui.text "}")
                    |> List.reverse
                )


indentPadding indent =
    Ui.paddingXY
        (indent * 8)
        0


viewField isFirst indent ( name, type_ ) =
    Ui.row []
        [ Ui.text
            (if isFirst then
                "{ "

             else
                ", "
            )
        , Ui.text name
        , Ui.text " : "
        , viewSingleLine type_
        ]
