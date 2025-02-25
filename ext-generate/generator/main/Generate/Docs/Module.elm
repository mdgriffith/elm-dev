module Generate.Docs.Module exposing (..)

import Elm
import Elm.Annotation as Type
import Elm.Docs
import Elm.Type


comment : String -> Elm.Expression
comment str =
    str
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""
        |> Elm.string


{-|

    { name = String
    , comment = String
    , unions = List Union
    , aliases = List Alias
    , values = List Value
    , binops = List Binop
    }

-}
generate : Elm.Docs.Module -> Elm.Expression
generate mod =
    Elm.record
        [ ( "name", Elm.string mod.name )
        , ( "comment", comment mod.comment )
        , ( "unions", Elm.list (List.map generateUnion mod.unions) )
        , ( "aliases", Elm.list (List.map generateAlias mod.aliases) )
        , ( "values", Elm.list (List.map generateValue mod.values) )
        , ( "binops", Elm.list (List.map generateBinop mod.binops) )
        ]
        |> Elm.withType (Type.named [ "Elm", "Docs" ] "Module")


generateUnion : Elm.Docs.Union -> Elm.Expression
generateUnion union =
    Elm.record
        [ ( "name", Elm.string union.name )
        , ( "comment", comment union.comment )
        , ( "args", Elm.list (List.map Elm.string union.args) )
        , ( "tags", Elm.list (List.map generateTag union.tags) )
        ]


generateTag : ( String, List Elm.Type.Type ) -> Elm.Expression
generateTag ( name, args ) =
    Elm.tuple
        (Elm.string name)
        (Elm.list (List.map generateType args))


generateAlias : Elm.Docs.Alias -> Elm.Expression
generateAlias alias_ =
    Elm.record
        [ ( "name", Elm.string alias_.name )
        , ( "comment", comment alias_.comment )
        , ( "args", Elm.list (List.map Elm.string alias_.args) )
        , ( "tipe", generateType alias_.tipe )
        ]


generateValue : Elm.Docs.Value -> Elm.Expression
generateValue value =
    Elm.record
        [ ( "name", Elm.string value.name )
        , ( "comment", comment value.comment )
        , ( "tipe", generateType value.tipe )
        ]


generateBinop : Elm.Docs.Binop -> Elm.Expression
generateBinop binop =
    Elm.record
        [ ( "name", Elm.string binop.name )
        , ( "comment", comment binop.comment )
        , ( "associativity", generateAssociativity binop.associativity )
        , ( "precedence", Elm.int binop.precedence )
        , ( "tipe", generateType binop.tipe )
        ]


generateAssociativity : Elm.Docs.Associativity -> Elm.Expression
generateAssociativity assoc =
    case assoc of
        Elm.Docs.Left ->
            Elm.value
                { importFrom = [ "Elm", "Docs" ]
                , name = "Left"
                , annotation = Nothing
                }

        Elm.Docs.None ->
            Elm.value
                { importFrom = [ "Elm", "Docs" ]
                , name = "None"
                , annotation = Nothing
                }

        Elm.Docs.Right ->
            Elm.value
                { importFrom = [ "Elm", "Docs" ]
                , name = "Right"
                , annotation = Nothing
                }


toTipe val args =
    Elm.apply
        (Elm.value
            { importFrom = [ "Elm", "Type" ]
            , name = val
            , annotation = Nothing
            }
        )
        args


generateType : Elm.Type.Type -> Elm.Expression
generateType tipe =
    case tipe of
        Elm.Type.Var name ->
            toTipe "Var"
                [ Elm.string name ]

        Elm.Type.Lambda arg ret ->
            toTipe "Lambda"
                [ generateType arg
                , generateType ret
                ]

        Elm.Type.Tuple types ->
            toTipe "Tuple"
                [ Elm.list (List.map generateType types)
                ]

        Elm.Type.Type name args ->
            toTipe "Type"
                [ Elm.string name
                , Elm.list (List.map generateType args)
                ]

        Elm.Type.Record fields ext ->
            toTipe "Record"
                [ Elm.list (List.map generateField fields)
                , Elm.maybe (Maybe.map Elm.string ext)
                ]


generateField : ( String, Elm.Type.Type ) -> Elm.Expression
generateField ( name, tipe ) =
    Elm.tuple
        (Elm.string name)
        (generateType tipe)
