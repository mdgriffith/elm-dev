module Viewer exposing (toApp)

import Elm
import Elm.Annotation
import Elm.Case
import Elm.Docs
import Elm.Type
import Gen.Browser
import Gen.Element
import Gen.Html
import Gen.String
import Gen.Ui
import Interactive


{-| -}
toApp : List Elm.Docs.Module -> Maybe Elm.File
toApp mods =
    mods
        |> List.map
            (\mod ->
                { name = mod.name
                , examples =
                    List.filterMap (buildApp mod) mod.values
                }
            )
        |> Interactive.generate [ "Docs" ]


buildApp : Elm.Docs.Module -> Elm.Docs.Value -> Maybe Interactive.Interactive
buildApp mod val =
    let
        ( args, result ) =
            getArgs val
    in
    if isViewable result && List.all isCreatable args then
        Just
            { name = String.concat (List.map capitalize (String.split "." mod.name)) ++ capitalize val.name
            , fields =
                -- case args of
                --     [] ->
                --         [ Interactive.field "state"
                --             { init = Elm.unit
                --             , type_ = Elm.Annotation.unit
                --             }
                --         ]
                --     [ single ] ->
                --         [ Interactive.field "state"
                --             { init = toInit single
                --             , type_ = toAnnotation single
                --             }
                --         ]
                --     _ ->
                --         List.indexedMap
                --             (\i arg ->
                --                 Interactive.field ("arg" ++ String.fromInt i)
                --                     { init = toInit arg
                --                     , type_ = toAnnotation arg
                --                     }
                --             )
                --             args
                []
            , view =
                \model ->
                    view mod val args result model
            }

    else
        Nothing


getArgs : Elm.Docs.Value -> ( List Elm.Type.Type, Elm.Type.Type )
getArgs value =
    getArgsHelper value.tipe []


getArgsHelper : Elm.Type.Type -> List Elm.Type.Type -> ( List Elm.Type.Type, Elm.Type.Type )
getArgsHelper value args =
    case value of
        Elm.Type.Var var ->
            ( List.reverse args, value )

        Elm.Type.Lambda one two ->
            getArgsHelper two (one :: args)

        Elm.Type.Tuple types ->
            ( List.reverse args, value )

        Elm.Type.Type name typeVars ->
            ( List.reverse args, value )

        Elm.Type.Record fields Nothing ->
            ( List.reverse args, value )

        Elm.Type.Record fields (Just baseName) ->
            ( List.reverse args, value )


getName : Elm.Docs.Value -> Int -> String
getName val i =
    case i of
        0 ->
            val.name

        _ ->
            val.name ++ "_" ++ String.fromInt i



-- toAnnotation : Elm.Type.Type -> { type_ : Elm.Annotation.Annotation, init : Elm.Expression }
-- toAnnotation elmType =
--     case elmType of
--         Elm.Type.Var string ->
--             Elm.Annotation.var string
--         Elm.Type.Lambda one two ->
--             Elm.Annotation.function
--                 [ toAnnotation one
--                 ]
--                 (toAnnotation two)
--         Elm.Type.Tuple types ->
--             case types of
--                 [] ->
--                     Elm.Annotation.unit
--                 [ one, two ] ->
--                     Elm.Annotation.tuple (toAnnotation one) (toAnnotation two)
--                 [ one, two, three ] ->
--                     Elm.Annotation.triple
--                         (toAnnotation one)
--                         (toAnnotation two)
--                         (toAnnotation three)
--                 _ ->
--                     Elm.Annotation.unit
--         Elm.Type.Type "Basics.Bool" [] ->
--             Elm.Annotation.bool
--         Elm.Type.Type "Basics.Int" [] ->
--             Elm.Annotation.int
--         Elm.Type.Type "Basics.Float" [] ->
--             Elm.Annotation.float
--         Elm.Type.Type "String.String" [] ->
--             Elm.Annotation.string
--         Elm.Type.Type "List.List" [ inner ] ->
--             Elm.Annotation.list (toAnnotation inner)
--         Elm.Type.Type "Maybe.Maybe" [ inner ] ->
--             Elm.Annotation.maybe (toAnnotation inner)
--         Elm.Type.Type name types ->
--             case List.reverse (String.split "." name) of
--                 [] ->
--                     Elm.Annotation.namedWith [] name (List.map toAnnotation types)
--                 valName :: mod ->
--                     Elm.Annotation.namedWith (List.reverse mod) valName (List.map toAnnotation types)
--         Elm.Type.Record fields maybeExtensible ->
--             case maybeExtensible of
--                 Nothing ->
--                     Elm.Annotation.record (List.map (Tuple.mapSecond toAnnotation) fields)
--                 Just base ->
--                     Elm.Annotation.extensible base (List.map (Tuple.mapSecond toAnnotation) fields)


toInit : Elm.Type.Type -> Elm.Expression
toInit elmType =
    case elmType of
        Elm.Type.Var string ->
            Elm.unit

        Elm.Type.Lambda one two ->
            Elm.unit

        Elm.Type.Tuple types ->
            case types of
                [] ->
                    Elm.unit

                [ one, two ] ->
                    Elm.tuple (toInit one) (toInit two)

                [ one, two, three ] ->
                    Elm.triple
                        (toInit one)
                        (toInit two)
                        (toInit three)

                _ ->
                    Elm.unit

        Elm.Type.Type "List.List" [ inner ] ->
            Elm.list [ toInit inner ]

        Elm.Type.Type "Maybe.Maybe" [ inner ] ->
            Elm.just (toInit inner)

        Elm.Type.Type "Basics.Bool" [] ->
            Elm.bool True

        Elm.Type.Type "Basics.Int" [] ->
            Elm.int 1

        Elm.Type.Type "Basics.Float" [] ->
            Elm.float 1

        Elm.Type.Type "String.String" [] ->
            Elm.string "Hello"

        Elm.Type.Type name types ->
            -- case List.reverse (String.split "." name) of
            --     [] ->
            --         Elm.namedWith [] name (List.map toInit types)
            --     valName :: mod ->
            --         Elm.namedWith (List.reverse mod) valName (List.map toInit types)
            Elm.unit

        Elm.Type.Record fields maybeExtensible ->
            Elm.record (List.map (\( name, val ) -> Tuple.pair name (toInit val)) fields)


{-| This is a value that can be rendered to the screen.
-}
isViewable : Elm.Type.Type -> Bool
isViewable type_ =
    case type_ of
        Elm.Type.Var string ->
            False

        Elm.Type.Lambda one two ->
            False

        Elm.Type.Tuple types ->
            case types of
                [] ->
                    True

                [ one, two ] ->
                    isViewable one && isViewable two

                [ one, two, three ] ->
                    isViewable one && isViewable two && isViewable three

                _ ->
                    False

        Elm.Type.Type "List.List" [ inner ] ->
            isViewable inner

        Elm.Type.Type "Maybe.Maybe" [ inner ] ->
            isViewable inner

        Elm.Type.Type "Basics.Bool" [] ->
            True

        Elm.Type.Type "Basics.Int" [] ->
            True

        Elm.Type.Type "Basics.Float" [] ->
            True

        Elm.Type.Type "String.String" [] ->
            True

        Elm.Type.Type "Html.Html" [] ->
            True

        Elm.Type.Type "Svg.Svg" [] ->
            True

        Elm.Type.Type name types ->
            False

        Elm.Type.Record fields maybeExtensible ->
            False


isCreatable : Elm.Type.Type -> Bool
isCreatable type_ =
    case type_ of
        Elm.Type.Var string ->
            False

        Elm.Type.Lambda one two ->
            False

        Elm.Type.Tuple types ->
            case types of
                [] ->
                    True

                [ one, two ] ->
                    isCreatable one && isCreatable two

                [ one, two, three ] ->
                    isCreatable one && isCreatable two && isCreatable three

                _ ->
                    False

        Elm.Type.Type "List.List" [ inner ] ->
            isCreatable inner

        Elm.Type.Type "Maybe.Maybe" [ inner ] ->
            isCreatable inner

        Elm.Type.Type "Basics.Bool" [] ->
            True

        Elm.Type.Type "Basics.Int" [] ->
            True

        Elm.Type.Type "Basics.Float" [] ->
            True

        Elm.Type.Type "String.String" [] ->
            True

        Elm.Type.Type "Html.Html" [] ->
            False

        Elm.Type.Type "Svg.Svg" [] ->
            False

        Elm.Type.Type name types ->
            False

        Elm.Type.Record fields maybeExtensible ->
            False


view :
    Elm.Docs.Module
    -> Elm.Docs.Value
    -> List Elm.Type.Type
    -> Elm.Type.Type
    -> Interactive.ViewReferences
    -> Elm.Expression
view mod val args result options =
    -- viewInput mod val args result model
    Gen.Element.row [ Gen.Element.padding 25, Gen.Element.spacing 25 ]
        [ viewInput mod val args result options
        , viewValue mod val args result options.model
        ]


viewInput :
    Elm.Docs.Module
    -> Elm.Docs.Value
    -> List Elm.Type.Type
    -> Elm.Type.Type
    -> Interactive.ViewReferences
    -> Elm.Expression
viewInput mod val args result options =
    case args of
        [] ->
            Elm.value
                { importFrom = String.split "." mod.name
                , name = val.name
                , annotation = Nothing --Just (toAnnotation val.tipe)
                }
                |> viewWrapper result

        [ single ] ->
            viewSingleInput mod single 0 val options

        _ ->
            Elm.apply
                (Elm.value
                    { importFrom = String.split "." mod.name
                    , name = val.name
                    , annotation = Nothing -- Just (toAnnotation val.tipe)
                    }
                )
                (List.indexedMap
                    (getArg options.model val)
                    args
                )
                |> viewWrapper result


viewSingleInput mod tipe index val options =
    case tipe of
        Elm.Type.Var string ->
            Gen.Element.none

        Elm.Type.Lambda one two ->
            Gen.Element.none

        Elm.Type.Tuple types ->
            case types of
                [] ->
                    Gen.Element.none

                [ one, two ] ->
                    Gen.Element.none

                -- isViewable one && isViewable two
                [ one, two, three ] ->
                    Gen.Element.none

                -- isViewable one && isViewable two && isViewable three
                _ ->
                    Gen.Element.none

        Elm.Type.Type "List.List" [ inner ] ->
            Gen.Element.none

        Elm.Type.Type "Maybe.Maybe" [ inner ] ->
            Gen.Element.none

        Elm.Type.Type "Basics.Bool" [] ->
            Gen.Element.none

        Elm.Type.Type "Basics.Int" [] ->
            Gen.Element.none

        Elm.Type.Type "Basics.Float" [] ->
            Gen.Element.none

        Elm.Type.Type "String.String" [] ->
            -- Gen.Ui.call_.inputString
            --     (Elm.string val.name)
            --     (Elm.fn ( "new", Just Elm.Annotation.string ) <|
            --         \new ->
            --             Elm.apply
            --                 options.onChange
            --                 [ Elm.record
            --                     [ ( "state", new )
            --                     ]
            --                 ]
            --     )
            --     (Elm.get "state" options.model)
            Gen.Element.none

        Elm.Type.Type "Html.Html" [] ->
            Gen.Element.none

        Elm.Type.Type "Svg.Svg" [] ->
            Gen.Element.none

        Elm.Type.Type name types ->
            Gen.Element.none

        Elm.Type.Record fields maybeExtensible ->
            Gen.Element.none


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain


viewValue mod val args result model =
    case args of
        [] ->
            Elm.value
                { importFrom = String.split "." mod.name
                , name = val.name
                , annotation = Nothing -- Just (toAnnotation val.tipe)
                }
                |> viewWrapper result

        [ single ] ->
            Elm.apply
                (Elm.value
                    { importFrom = String.split "." mod.name
                    , name = val.name
                    , annotation = Nothing -- Just (toAnnotation val.tipe)
                    }
                )
                [ Elm.get "state" model
                ]
                |> viewWrapper result

        _ ->
            Elm.apply
                (Elm.value
                    { importFrom = String.split "." mod.name
                    , name = val.name
                    , annotation = Nothing -- Just (toAnnotation val.tipe)
                    }
                )
                (List.indexedMap
                    (getArg model val)
                    args
                )
                |> viewWrapper result


getArg model val i arg =
    model
        |> Elm.get ("arg" ++ String.fromInt i)


viewWrapper result exp =
    case result of
        Elm.Type.Var string ->
            Gen.Element.text ""

        Elm.Type.Lambda one two ->
            Gen.Element.text ""

        Elm.Type.Tuple types ->
            case types of
                [] ->
                    Gen.Element.text "()"

                [ one, two ] ->
                    Gen.Element.text ""

                -- isViewable one && isViewable two
                [ one, two, three ] ->
                    Gen.Element.text ""

                -- isViewable one && isViewable two && isViewable three
                _ ->
                    Gen.Element.text ""

        Elm.Type.Type "List.List" [ inner ] ->
            Gen.Element.text ""

        Elm.Type.Type "Maybe.Maybe" [ inner ] ->
            Gen.Element.text ""

        Elm.Type.Type "Basics.Bool" [] ->
            Gen.Element.text ""

        Elm.Type.Type "Basics.Int" [] ->
            Gen.Element.call_.text (Gen.String.call_.fromInt exp)

        Elm.Type.Type "Basics.Float" [] ->
            Gen.Element.call_.text (Gen.String.call_.fromFloat exp)

        Elm.Type.Type "String.String" [] ->
            Gen.Element.call_.text exp

        Elm.Type.Type "Html.Html" [] ->
            Gen.Element.html exp

        Elm.Type.Type "Svg.Svg" [] ->
            Gen.Element.html exp

        Elm.Type.Type name types ->
            Gen.Element.text ""

        Elm.Type.Record fields maybeExtensible ->
            Gen.Element.text ""
