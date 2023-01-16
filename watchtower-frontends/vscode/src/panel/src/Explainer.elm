module Explainer exposing (..)

{-| -}

import Char
import Element as Ui
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import Html.Attributes
import Ports
import Ui
import Ui.Card
import Ui.Type
import VSCode.Colors
import VSCode.SyntaxColors as Syntax


view : List Ports.Fact -> Ui.Element msg
view facts =
    Ui.column
        [ Ui.width Ui.fill
        , Ui.space.lg

        --, Ui.height (Ui.px 1000)
        -- , Ui.height Ui.fill
        , Ui.htmlAttribute (Html.Attributes.style "height" "100vh")
        , Ui.htmlAttribute (Html.Attributes.style "overflow" "auto")
        , Ui.htmlAttribute (Html.Attributes.style "padding-bottom" "200px")
        ]
        (facts
            |> List.sortBy moduleIdentity
            |> groupWhile (\one two -> moduleIdentity one == moduleIdentity two)
            |> List.filterMap viewFactGroup
        )


viewFactGroup : ( Ports.Fact, List Ports.Fact ) -> Maybe (Ui.Element msg)
viewFactGroup ( (Ports.Fact topFact) as fact, remainingFacts ) =
    let
        renderedFacts =
            List.filterMap viewFact (fact :: remainingFacts)
    in
    case renderedFacts of
        [] ->
            Nothing

        _ ->
            Just <|
                Ui.Card.view
                    { title =
                        case topFact.source of
                            Ports.SourceLet ->
                                "Let"

                            Ports.SourceDeclaration ->
                                "Declaration"

                            Ports.External mod ->
                                mod.name
                    , hint =
                        case topFact.source of
                            Ports.SourceLet ->
                                Nothing

                            Ports.SourceDeclaration ->
                                Nothing

                            Ports.External mod ->
                                if mod.pkg == "author/project" then
                                    Nothing

                                else
                                    Just mod.pkg
                    , highlight = True
                    , onClick =
                        -- if expanded then
                        Nothing

                    -- else
                    --     Just (EditorGoTo file.path issue.region)
                    }
                    renderedFacts


isCapitalized : String -> Bool
isCapitalized str =
    case String.uncons str of
        Nothing ->
            False

        Just ( char, remain ) ->
            Char.isUpper char


{-| -}
viewFact : Ports.Fact -> Maybe (Ui.Element msg)
viewFact (Ports.Fact fact) =
    case fact.details of
        Ports.Value value ->
            if isCapitalized fact.name then
                Nothing

            else
                Just <|
                    Ui.row []
                        [ Ui.row [ Ui.alignTop ]
                            [ Ui.el [ Syntax.declaration ] (Ui.text fact.name)
                            , Ui.el [ Syntax.keyword ] (Ui.text " : ")
                            ]
                        , viewType value.type_
                        ]

        Ports.Union union ->
            Just <|
                Ui.column
                    [ Ui.space.sm ]
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
                                    ( False
                                    , Ui.row
                                        []
                                        [ Ui.el [ Syntax.keyword, Ui.alignTop ] <|
                                            if isFirst then
                                                Ui.text "= "

                                            else
                                                Ui.text "| "
                                        , Ui.el [ Syntax.variant, Ui.alignTop ] (Ui.text variant.name)
                                        , Ui.text " "
                                        , Ui.row [ Ui.space.md ]
                                            (List.map viewType variant.types_)
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
                    , Ui.whenJust union.comment <|
                        \comment ->
                            Ui.el [ Ui.pad.md ] (Ui.text comment)
                    ]

        Ports.Alias alias_ ->
            Just <|
                Ui.column
                    [ Ui.space.sm ]
                    [ Ui.row []
                        [ Ui.el [ Syntax.keyword ] (Ui.text "type alias ")
                        , Ui.el [ Syntax.type_ ] (Ui.text alias_.name)
                        , Ui.el [ Syntax.keyword ] (Ui.text " =")
                        ]
                    , Ui.row []
                        [ Ui.text "    "
                        , viewType alias_.type_
                        ]
                    , Ui.whenJust alias_.comment <|
                        \comment ->
                            Ui.el [ Ui.pad.md ] (Ui.text comment)
                    ]

        Ports.Def def ->
            Just <|
                Ui.column
                    []
                    [ Ui.el [ Ui.alignTop, Syntax.declaration ] (Ui.text fact.name)
                    , Ui.whenJust def.type_ <|
                        viewType
                    , Ui.el [ Ui.pad.md ] (Ui.text def.comment)
                    ]


viewType : Ports.Type -> Ui.Element msg
viewType type_ =
    Ui.Type.view type_.value


viewUnionCase : { a | name : String, types_ : List Ports.Type } -> Ui.Element msg
viewUnionCase variant =
    Ui.row
        []
        [ Ui.text "| "
        , Ui.text variant.name
        , Ui.text " "
        , Ui.row [ Ui.space.md ]
            (List.map viewType variant.types_)
        ]


prefixModule : Ports.Source -> String -> String
prefixModule source name =
    case source of
        Ports.SourceDeclaration ->
            name

        Ports.SourceLet ->
            name

        Ports.External mod ->
            mod.name ++ "." ++ name


viewModuleName : Ports.Source -> Ui.Element msg
viewModuleName source =
    case source of
        Ports.SourceDeclaration ->
            Ui.none

        Ports.SourceLet ->
            Ui.none

        Ports.External mod ->
            case mod.pkg of
                "author/project" ->
                    Ui.none

                _ ->
                    Ui.el [] (Ui.text mod.pkg)



{- HELP -}


{-| -}
groupWhile : (a -> a -> Bool) -> List a -> List ( a, List a )
groupWhile isSameGroup items =
    List.foldr
        (\x acc ->
            case acc of
                [] ->
                    [ ( x, [] ) ]

                ( y, restOfGroup ) :: groups ->
                    if isSameGroup x y then
                        ( x, y :: restOfGroup ) :: groups

                    else
                        ( x, [] ) :: acc
        )
        []
        items


moduleIdentity : Ports.Fact -> String
moduleIdentity (Ports.Fact fact) =
    case fact.source of
        Ports.SourceDeclaration ->
            "00000"

        Ports.SourceLet ->
            "00000"

        Ports.External mod ->
            mod.pkg ++ "$" ++ mod.name
