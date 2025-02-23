module Ui.Input.Text exposing (..)

{-|

@docs text, search

@docs with

@docs view

-}

import Ui
import Ui.Dropdown
import Ui.Input
import Ui.Theme


type TextInput msg
    = TextInput (Details msg)


type alias Details msg =
    { value : String
    , onChange : String -> msg
    , onSubmit : Maybe msg
    , placeholder : String
    }


text :
    { onChange : String -> msg
    , text : String
    }
    -> Ui.Element msg
text options =
    TextInput
        { value = options.text
        , onChange = options.onChange
        }


search :
    { onSubmit : msg
    , onChange : String -> msg
    , text : String
    , results : List SearchResult
    }
    -> Ui.Element msg
search options =
    TextInput
        { value = options.text
        , onChange = options.onChange
        }


view : TextInput msg -> Ui.Element msg
view (TextInput details) =
    Ui.Input.search
        [ Ui.Theme.border.small
        , Ui.below
            (viewResults options.results)
        ]
        { onChange = options.onChange
        , text = options.text
        , placeholder =
            Just (Ui.Input.placeholder [] (Ui.text details.placeholder))
        , label =
            Ui.Input.labelHidden options.label
        }


type alias SearchResult =
    { name : String
    , group : String
    , url : String
    }


viewResults : List SearchResult -> Ui.Element msg
viewResults results =
    case results of
        [] ->
            Ui.none

        _ ->
            let
                groups =
                    groupWhile
                        (\one two ->
                            one.group == two.group
                        )
                        results
            in
            Ui.column
                [ Ui.Theme.border.small
                , Ui.Theme.padding.sm
                , Ui.width Ui.fill
                , Ui.Theme.spacing.sm
                ]
                (List.map
                    (\( top, others ) ->
                        Ui.column [ Ui.width Ui.fill, Ui.Theme.spacing.sm3 ]
                            [ Ui.el
                                [ Ui.ellipsis
                                , Ui.width Ui.fill
                                , Ui.Theme.font.small
                                ]
                                (Ui.text top.group)
                            , Ui.column [ Ui.width Ui.fill, Ui.Theme.spacing.sm2 ]
                                (List.map
                                    (\item ->
                                        Ui.el
                                            [ Ui.link item.url
                                            , Ui.ellipsis
                                            , Ui.width Ui.fill
                                            ]
                                            (Ui.text item.name)
                                    )
                                    (top :: others)
                                )
                            ]
                    )
                    groups
                )


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
