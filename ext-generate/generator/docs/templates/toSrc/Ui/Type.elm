module Ui.Type exposing
    ( needsParens
    , shouldBeMultiline
    , view
    , viewWithIndent
    )

import Elm.Type
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Theme
import Theme.Color
import Ui.Attr
import Ui.Syntax as Syntax


styles =
    { link = [ Theme.Color.textPrimaryInteractive ]
    }


needsParens : Elm.Type.Type -> Bool
needsParens tipe =
    case tipe of
        Elm.Type.Var _ ->
            False

        Elm.Type.Lambda _ _ ->
            True

        Elm.Type.Type _ [] ->
            False

        Elm.Type.Type _ _ ->
            True

        Elm.Type.Tuple _ ->
            False

        Elm.Type.Record _ _ ->
            False


isFunc : Elm.Type.Type -> Bool
isFunc tipe =
    case tipe of
        Elm.Type.Lambda _ _ ->
            True

        _ ->
            False


shouldBeMultiline : Elm.Type.Type -> Bool
shouldBeMultiline tipe =
    linearWidth tipe > 50


type alias Options msg =
    { currentModule : Maybe String
    , onClick : Maybe (String -> msg)
    }


{-| View a type definition
-}
view : Options msg -> Elm.Type.Type -> List (Html msg)
view options tipe =
    viewWithIndent options 0 tipe


viewWithIndent : Options msg -> Int -> Elm.Type.Type -> List (Html msg)
viewWithIndent options indent tipe =
    let
        isMultiline =
            shouldBeMultiline tipe && not (startsWithRecord tipe)
    in
    viewType options isMultiline indent tipe


startsWithRecord : Elm.Type.Type -> Bool
startsWithRecord tipe =
    case tipe of
        Elm.Type.Record _ _ ->
            True

        Elm.Type.Lambda (Elm.Type.Record _ _) _ ->
            True

        _ ->
            False


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


indentPadding : Int -> Html.Attribute msg
indentPadding indent =
    Attr.style "padding-left" (String.fromInt (indent * 8) ++ "px")


indentSpace : Int -> String
indentSpace indent =
    String.repeat indent " "


parens : Html msg -> Html msg
parens content =
    Html.span []
        [ punctuation "("
        , content
        , punctuation ")"
        ]


parenList : List (Html msg) -> List (Html msg)
parenList items =
    case items of
        [] ->
            [ punctuation "( )" ]

        _ ->
            punctuation "( "
                :: items
                ++ [ punctuation " )" ]


spacedParens : Html msg -> Html msg
spacedParens content =
    Html.span []
        [ punctuation "( "
        , content
        , punctuation " )"
        ]


verticalParens : Html msg -> Html msg
verticalParens content =
    Html.span []
        [ Html.span []
            [ span
                [ Ui.Attr.alignTop
                , Syntax.punctuation
                ]
                (Html.text "(")
            , content
            ]
        , punctuation ")"
        ]


addParens : Elm.Type.Type -> Html msg -> Html msg
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
    -> Html msg
    -> Html msg
addParensInFunction tipe elem =
    case tipe of
        Elm.Type.Lambda _ _ ->
            parens elem

        _ ->
            elem


span : List (Html.Attribute msg) -> Html msg -> Html msg
span attrs content =
    Html.span attrs [ content ]


isBuiltIn : String -> Bool
isBuiltIn typename =
    (typename == "Int")
        || (typename == "Basics.Int")
        || (typename == "Basics.Float")
        || (typename == "Float")
        || (typename == "String")
        || (typename == "String.String")
        || (typename == "Char")
        || (typename == "Basics.Char")
        || (typename == "Bool")
        || (typename == "Basics.Bool")
        || (typename == "List")
        || (typename == "List.List")
        || (typename == "Maybe")
        || (typename == "Maybe.Maybe")
        || (typename == "Result")
        || (typename == "Result.Result")
        || (typename == "Cmd")
        || (typename == "Sub")
        || (typename == "Array")
        || (typename == "Dict")
        || (typename == "Set")
        || (typename == "Platform.Cmd.Cmd")
        || (typename == "Platform.Sub.Sub")


toName : Options msg -> String -> String
toName options typename =
    let
        toBuiltInName name =
            case name of
                "String.String" ->
                    "String"

                "List.List" ->
                    "List"

                "Basics.Bool" ->
                    "Bool"

                "Basics.Float" ->
                    "Float"

                "Basics.Int" ->
                    "Int"

                "Result.Result" ->
                    "Result"

                "Maybe.Maybe" ->
                    "Maybe"

                "Platform.Cmd.Cmd" ->
                    "Cmd"

                "Platform.Sub.Sub" ->
                    "Sub"

                _ ->
                    typename
    in
    case options.currentModule of
        Just current ->
            if String.startsWith current typename then
                String.dropLeft (String.length current + 1) typename

            else
                toBuiltInName typename

        Nothing ->
            toBuiltInName typename


typeLink : Options msg -> String -> List (Html.Attribute msg) -> Html msg
typeLink options typename attrs =
    case options.onClick of
        Nothing ->
            span attrs (Html.text (toName options typename))

        Just onClick ->
            let
                fullRefName =
                    case options.currentModule of
                        Nothing ->
                            typename

                        Just currentModule ->
                            if String.contains "." typename then
                                typename

                            else
                                currentModule ++ "." ++ typename
            in
            --Attr.href ("https://package.elm-lang.org/packages/elm/core/latest/" ++ typename ++ "#")
            Html.span
                (Events.onClick (onClick fullRefName)
                    :: Attr.style "cursor" "pointer"
                    :: styles.link
                    ++ attrs
                )
                [ Html.text (toName options typename) ]


viewType :
    Options msg
    -> Bool
    -> Int
    -> Elm.Type.Type
    -> List (Html msg)
viewType options forceMultiline indent tipe =
    case tipe of
        Elm.Type.Var var ->
            [ span [ Syntax.typevar ] (Html.text var) ]

        Elm.Type.Lambda one two ->
            let
                oneRendered =
                    viewType options (shouldBeMultiline one) indent one

                twoRendered =
                    viewFnArgs options (shouldBeMultiline tipe) indent two

                multiline =
                    forceMultiline

                realMultiline =
                    if multiline then
                        multiline

                    else
                        linearWidth tipe > 50
            in
            addParensInFunction one (Html.span [] oneRendered)
                :: twoRendered

        Elm.Type.Tuple [] ->
            [ Html.text "()" ]

        Elm.Type.Tuple vals ->
            let
                renderedItems =
                    viewList forceMultiline
                        indent
                        (\t -> Html.span [] (viewType options forceMultiline (indent + 4) t))
                        vals
                        { rowSpacer = span [ Syntax.punctuation ] (Html.text ", ")
                        , columnSpacer = span [ Syntax.punctuation ] (Html.text ", ")
                        }
            in
            [ renderedItems.content
                |> spacedParens
            ]

        Elm.Type.Type typename [] ->
            [ typeLink options
                typename
                [ Syntax.type_
                ]
            ]

        Elm.Type.Type typename varTypes ->
            let
                renderedItems =
                    viewList forceMultiline
                        indent
                        (\var ->
                            let
                                rendered =
                                    viewType options forceMultiline (indent + 4) var
                            in
                            addParens var (Html.span [] rendered)
                        )
                        varTypes
                        { rowSpacer = Html.text " "
                        , columnSpacer = Html.text " "
                        }
            in
            [ typeLink options typename [ Syntax.type_ ]
            , Html.text " "
            , renderedItems.content
            ]

        Elm.Type.Record fields maybeExtensibleName ->
            case fields of
                [] ->
                    [ punctuation "{}" ]

                _ ->
                    let
                        spacer =
                            String.repeat indent " "

                        isMultiline =
                            List.length fields > 1
                    in
                    List.foldl
                        (\( name, fieldType ) cursor ->
                            let
                                fieldContentIsMultiline =
                                    shouldBeMultiline fieldType

                                fieldContent =
                                    viewType options fieldContentIsMultiline (indent + 4) fieldType

                                extName =
                                    case maybeExtensibleName of
                                        Nothing ->
                                            Html.text ""

                                        Just recordName ->
                                            fieldName recordName

                                extNameDivider =
                                    case maybeExtensibleName of
                                        Nothing ->
                                            Html.text ""

                                        Just recordName ->
                                            punctuation " | "
                            in
                            { isFirst = False
                            , content =
                                List.concat
                                    [ [ if isMultiline then
                                            Html.text "\n"

                                        else
                                            Html.text ""
                                      ]
                                    , List.reverse fieldContent
                                    , [ indentIf fieldContentIsMultiline (indent + 4)
                                      , newlineIf fieldContentIsMultiline
                                      ]
                                    , [ keyword " : "
                                      , fieldName name
                                      ]
                                    , if cursor.isFirst then
                                        [ extName
                                        , extNameDivider
                                        , punctuation "{ "
                                        ]

                                      else
                                        [ punctuation ", " ]
                                    , if isMultiline then
                                        [ Html.text spacer ]

                                      else
                                        []
                                    ]
                                    ++ cursor.content
                            }
                        )
                        { isFirst = True
                        , content =
                            [ if isMultiline then
                                Html.text "\n"

                              else
                                Html.text ""
                            ]
                        }
                        fields
                        |> (\result ->
                                punctuation
                                    (if isMultiline then
                                        spacer ++ "}"

                                     else
                                        " }"
                                    )
                                    :: result.content
                                    |> List.reverse
                           )


indentIf : Bool -> Int -> Html msg
indentIf isMultiline indent =
    if isMultiline then
        Html.text (indentSpace indent)

    else
        Html.text ""


newlineIf : Bool -> Html msg
newlineIf isMultiline =
    if isMultiline then
        Html.text "\n"

    else
        Html.text ""


keyword str =
    span [ Syntax.keyword ] (Html.text str)


fieldName str =
    span [ Syntax.field ] (Html.text str)


punctuation str =
    span [ Syntax.punctuation ] (Html.text str)


viewList :
    Bool
    -> Int
    -> (Elm.Type.Type -> Html msg)
    -> List Elm.Type.Type
    ->
        { rowSpacer : Html msg
        , columnSpacer : Html msg
        }
    ->
        { content : Html msg
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
                Html.span []
                    [ Html.span []
                        [ if cursor.isFirst then
                            Html.text ""

                          else if forceMultiline then
                            spacer.columnSpacer

                          else
                            spacer.rowSpacer
                        , fieldContent
                        ]
                    ]
                    :: cursor.content
            , multiline = cursor.multiline
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
                    Html.span []
                        (result.content
                            |> List.reverse
                        )
                }
           )


attrIf : Bool -> Html.Attribute msg -> Html.Attribute msg
attrIf condition attr =
    if condition then
        attr

    else
        Attr.class ""


viewFnArgs :
    Options msg
    -> Bool
    -> Int
    -> Elm.Type.Type
    -> List (Html msg)
viewFnArgs options forceMultiline indent tipe =
    let
        node children =
            Html.text
                (if forceMultiline then
                    "\n" ++ indentSpace indent

                 else
                    ""
                )
                :: children
    in
    case tipe of
        Elm.Type.Lambda one two ->
            let
                args =
                    viewFnArgs options forceMultiline indent two

                argType =
                    if isFunc one then
                        parenList (viewType options False indent one)

                    else
                        viewType options False indent one
            in
            node
                (arrowRight forceMultiline :: argType ++ args)

        everythingElse ->
            node
                (arrowRight forceMultiline
                    :: viewType options False indent everythingElse
                )


arrowRight : Bool -> Html msg
arrowRight multiline =
    if multiline then
        span [ Syntax.keyword ] (Html.text "-> ")

    else
        span [ Syntax.keyword ] (Html.text " -> ")
