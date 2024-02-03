module Ui.Type exposing (view, viewUnion)

import Element as Ui
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import Elm.Type
import Ports
import Ui
import VSCode.SyntaxColors as Syntax


viewUnion : Ports.UnionDetails -> Ui.Element msg
viewUnion union =
    Ui.column [ Ui.space.sm ]
        [ Ui.row [ Ui.alignTop ]
            [ Ui.el [ Syntax.keyword ] (Ui.text "type ")
            , Ui.el [ Syntax.type_ ] (Ui.text union.name)
            ]
        , Ui.row []
            [ Ui.text "   "
            , Ui.column
                [ Ui.space.sm
                ]
                (List.foldl
                    (\variant ( isFirst, children ) ->
                        let
                            payload =
                                List.foldl
                                    (\varType cursor ->
                                        let
                                            item =
                                                viewNew (shouldBeMultiline varType.value) 0 varType.value
                                        in
                                        { items = item.content :: cursor.items
                                        , multiline = cursor.multiline || item.multiline
                                        }
                                    )
                                    { items = []
                                    , multiline = False
                                    }
                                    variant.types_
                        in
                        ( False
                        , Ui.row []
                            [ Ui.el [ Syntax.keyword, Ui.alignTop ] <|
                                if isFirst then
                                    Ui.text "= "

                                else
                                    Ui.text "| "
                            , if payload.multiline then
                                Ui.column [ Ui.space.sm ]
                                    [ Ui.el [ Syntax.variant, Ui.alignTop ] (Ui.text variant.name)
                                    , Ui.column
                                        [ Ui.space.sm, indentPadding 2 ]
                                        (List.reverse payload.items)
                                    ]

                              else
                                Ui.row []
                                    [ Ui.el [ Syntax.variant, Ui.alignTop ] (Ui.text variant.name)
                                    , Ui.text " "
                                    , Ui.row
                                        [ Ui.space.sm ]
                                        (List.reverse payload.items)
                                    ]
                            ]
                            :: children
                        )
                    )
                    ( True, [] )
                    union.cases
                    |> Tuple.second
                    |> List.reverse
                )
            ]
        ]


shouldBeMultiline : Elm.Type.Type -> Bool
shouldBeMultiline tipe =
    linearWidth tipe > 50


{-| View a type definition
-}
view : Elm.Type.Type -> Ui.Element msg
view tipe =
    viewNew (shouldBeMultiline tipe) 0 tipe
        |> .content


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
                    (\( name, fieldType ) ->
                        4 + String.length name + linearWidth fieldType
                    )
                    fields
                + spacingWidth 2 fields

        Elm.Type.Record fields (Just extensibleName) ->
            String.length extensibleName
                + recordWidth
                + sumWith
                    (\( name, fieldType ) ->
                        3 + String.length name + linearWidth fieldType
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


indentPadding : Int -> Ui.Attribute msg
indentPadding indent =
    Ui.paddingXY
        (indent * 8)
        0


parens : Ui.Element msg -> Ui.Element msg
parens content =
    Ui.row []
        [ punctuation "("
        , content
        , punctuation ")"
        ]


verticalParens : Ui.Element msg -> Ui.Element msg
verticalParens content =
    Ui.column []
        [ Ui.row []
            [ Ui.el
                [ Ui.alignTop
                , Syntax.punctuation
                ]
                (Ui.text "(")
            , content
            ]
        , punctuation ")"
        ]


addParens : Elm.Type.Type -> Ui.Element msg -> Ui.Element msg
addParens tipe elem =
    case tipe of
        Elm.Type.Lambda _ _ ->
            parens elem

        Elm.Type.Type _ [] ->
            elem

        Elm.Type.Type _ _ ->
            parens elem

        _ ->
            elem


addParensInFunction :
    Elm.Type.Type
    ->
        { content : Ui.Element msg
        , multiline : Bool
        }
    -> Ui.Element msg
addParensInFunction tipe elem =
    case tipe of
        Elm.Type.Lambda _ _ ->
            if elem.multiline then
                verticalParens elem.content

            else
                parens elem.content

        _ ->
            elem.content


viewNew :
    Bool
    -> Int
    -> Elm.Type.Type
    ->
        { content : Ui.Element msg
        , multiline : Bool
        }
viewNew forceMultiline indent tipe =
    case tipe of
        Elm.Type.Var var ->
            { multiline = forceMultiline
            , content = Ui.el [ Syntax.typevar, Ui.alignTop ] (Ui.text var)
            }

        Elm.Type.Lambda one two ->
            let
                oneRendered =
                    viewNew forceMultiline indent one

                twoRendered =
                    viewFnArgs forceMultiline indent two

                multiline =
                    forceMultiline || oneRendered.multiline || twoRendered.multiline

                realMultiline =
                    if multiline then
                        multiline

                    else
                        linearWidth tipe > 50
            in
            { multiline = realMultiline
            , content =
                columnIf realMultiline
                    []
                    (addParensInFunction one oneRendered
                        :: twoRendered.items
                    )
            }

        Elm.Type.Tuple vals ->
            let
                renderedItems =
                    viewList forceMultiline
                        indent
                        (viewNew forceMultiline (indent + 4))
                        vals
                        { rowSpacer = Ui.el [ Syntax.punctuation ] (Ui.text ", ")
                        , columnSpacer = Ui.el [ Syntax.punctuation ] (Ui.text ", ")
                        }
            in
            { multiline = forceMultiline || renderedItems.multiline
            , content =
                renderedItems.content
                    |> parens
            }

        Elm.Type.Type typename [] ->
            { multiline = forceMultiline
            , content = Ui.el [ Syntax.type_, Ui.alignTop ] (Ui.text typename)
            }

        Elm.Type.Type typename varTypes ->
            let
                renderedItems =
                    viewList forceMultiline
                        indent
                        (\var ->
                            let
                                rendered =
                                    viewNew forceMultiline (indent + 4) var
                            in
                            { content =
                                addParens
                                    var
                                    rendered.content
                            , multiline = rendered.multiline
                            }
                        )
                        varTypes
                        { rowSpacer = Ui.text " "
                        , columnSpacer = Ui.none
                        }
            in
            { multiline = forceMultiline || renderedItems.multiline
            , content =
                columnIf (forceMultiline || renderedItems.multiline)
                    []
                    [ Ui.row [ Ui.alignTop ]
                        [ Ui.el [ Ui.alignTop, Syntax.type_ ] (Ui.text typename)
                        , Ui.text " "
                        ]
                    , if forceMultiline || renderedItems.multiline then
                        Ui.row []
                            [ Ui.text "    "
                            , renderedItems.content
                            ]

                      else
                        renderedItems.content
                    ]
            }

        Elm.Type.Record fields maybeExtensibleName ->
            List.foldl
                (\( name, type_ ) cursor ->
                    let
                        fieldContent =
                            viewNew False (indent + 4) type_
                    in
                    { isFirst = False
                    , content =
                        columnIf fieldContent.multiline
                            []
                            [ Ui.row [ Ui.alignTop, Syntax.field ]
                                [ if cursor.isFirst then
                                    Ui.row []
                                        [ punctuation "{ "
                                        , case maybeExtensibleName of
                                            Nothing ->
                                                Ui.none

                                            Just recordName ->
                                                fieldName recordName
                                        , case maybeExtensibleName of
                                            Nothing ->
                                                Ui.none

                                            Just recordName ->
                                                punctuation " | "
                                        ]

                                  else
                                    punctuation ", "
                                , fieldName name
                                , keyword " : "
                                ]
                            , if fieldContent.multiline then
                                Ui.row []
                                    [ Ui.text "    "
                                    , fieldContent.content
                                    ]

                              else
                                fieldContent.content
                            ]
                            :: cursor.content
                    , multiline = cursor.multiline || fieldContent.multiline
                    }
                )
                { isFirst = True
                , content = []
                , multiline = True --List.length fields > 1
                }
                fields
                |> (\result ->
                        { multiline = result.multiline
                        , content =
                            Ui.column [ Ui.space.sm ]
                                ((punctuation "}" :: result.content)
                                    |> List.reverse
                                )
                        }
                   )


keyword str =
    Ui.el [ Syntax.keyword ] (Ui.text str)


fieldName str =
    Ui.el [ Syntax.field ] (Ui.text str)


punctuation str =
    Ui.el [ Syntax.punctuation ] (Ui.text str)


viewList :
    Bool
    -> Int
    ->
        (Elm.Type.Type
         ->
            { content : Ui.Element msg
            , multiline : Bool
            }
        )
    -> List Elm.Type.Type
    ->
        { rowSpacer : Ui.Element msg
        , columnSpacer : Ui.Element msg
        }
    ->
        { content : Ui.Element msg
        , multiline : Bool
        }
viewList forceMultiline indent viewItem items spacer =
    List.foldl
        (\type_ cursor ->
            let
                fieldContent =
                    viewItem type_
            in
            { isFirst = False
            , content =
                columnIf (forceMultiline || fieldContent.multiline)
                    []
                    [ Ui.row []
                        [ if cursor.isFirst then
                            Ui.none

                          else if forceMultiline || fieldContent.multiline then
                            spacer.columnSpacer

                          else
                            spacer.rowSpacer
                        , fieldContent.content
                        ]
                    ]
                    :: cursor.content
            , multiline = cursor.multiline || fieldContent.multiline
            }
        )
        { isFirst = True
        , content = []
        , multiline = False
        }
        items
        |> (\result ->
                { multiline = result.multiline
                , content =
                    columnIf result.multiline
                        []
                        (result.content
                            |> List.reverse
                        )
                }
           )


viewFnArgs :
    Bool
    -> Int
    -> Elm.Type.Type
    ->
        { items : List (Ui.Element msg)
        , multiline : Bool
        }
viewFnArgs forceMultiline indent tipe =
    case tipe of
        Elm.Type.Lambda one two ->
            let
                new =
                    viewNew False indent one

                args =
                    viewFnArgs forceMultiline indent two
            in
            { multiline = args.multiline
            , items =
                Ui.row [ Ui.alignTop ]
                    [ arrowRight (forceMultiline || args.multiline)
                    , new.content
                    ]
                    :: args.items
            }

        everythingElse ->
            let
                new =
                    viewNew False indent everythingElse
            in
            { multiline = new.multiline
            , items =
                [ Ui.row [ Ui.alignTop ]
                    [ arrowRight (forceMultiline || new.multiline)
                    , new.content
                    ]
                ]
            }


columnIf : Bool -> List (Ui.Attribute msg) -> List (Ui.Element msg) -> Ui.Element msg
columnIf on attrs children =
    if on then
        Ui.column (Ui.space.sm :: attrs) children

    else
        Ui.row attrs children


arrowRight : Bool -> Ui.Element msg
arrowRight multiline =
    if multiline then
        Ui.el [ Ui.alignTop, Syntax.keyword ] (Ui.text "-> ")

    else
        Ui.el [ Ui.alignTop, Syntax.keyword ] (Ui.text " -> ")
