module Ui.Input.Text exposing
    ( TextInput, SearchResult
    , text, search
    , view
    )

{-|

@docs TextInput, SearchResult
@docs text, search
@docs view

-}

import Theme
import Theme.Color
import Ui
import Ui.Input


type TextInput msg
    = TextInput (Details msg)


type alias Details msg =
    { value : String
    , onChange : String -> msg
    , onSubmit : Maybe msg
    , placeholder : String
    , label : String
    , results : List SearchResult
    }


type alias SearchResult =
    { name : String
    , group : String
    , url : String
    }


text :
    { onChange : String -> msg
    , text : String
    }
    -> TextInput msg
text options =
    TextInput
        { value = options.text
        , onChange = options.onChange
        , onSubmit = Nothing
        , placeholder = ""
        , label = "Text"
        , results = []
        }


search :
    { onSubmit : msg
    , onChange : String -> msg
    , text : String
    , results : List SearchResult
    }
    -> TextInput msg
search options =
    TextInput
        { value = options.text
        , onChange = options.onChange
        , onSubmit = Just options.onSubmit
        , placeholder = "Search"
        , label = "Search"
        , results = options.results
        }


view : TextInput msg -> Ui.Element msg
view (TextInput details) =
    Ui.Input.search
        [ Theme.Color.borderDefault
        , Theme.borderWidth 1
        , Theme.borderRadius.sm
        , Ui.below (viewResults details.results)
        ]
        { onChange = details.onChange
        , text = details.value
        , placeholder =
            Just (Ui.Input.placeholder [] (Ui.text details.placeholder))
        , label =
            Ui.Input.labelHidden details.label
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
                        (\one two -> one.group == two.group)
                        results
            in
            Ui.column
                [ Theme.Color.backgroundSurface
                , Theme.Color.borderDefault
                , Theme.borderWidth 1
                , Theme.borderRadius.sm
                , Theme.pad 1
                , Ui.width Ui.fill
                , Theme.gap 1
                ]
                (List.map
                    (\( top, others ) ->
                        Ui.column [ Ui.width Ui.fill, Theme.gap 2 ]
                            [ Ui.el
                                [ Ui.ellipsis
                                , Ui.width Ui.fill
                                , Theme.font.body
                                ]
                                (Ui.text top.group)
                            , Ui.column [ Ui.width Ui.fill, Theme.gap 1 ]
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
