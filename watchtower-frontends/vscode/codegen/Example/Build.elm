module Example.Build exposing (build, getValueNamed)

{-| -}

import Elm
import Elm.Docs
import Elm.Op
import Elm.Type
import Example.Type


getValueNamed : String -> List Elm.Docs.Value -> Maybe Elm.Docs.Value
getValueNamed name values =
    case values of
        [] ->
            Nothing

        top :: remain ->
            if top.name == name then
                Just top

            else
                getValueNamed name remain


build : List Elm.Docs.Value -> Elm.Docs.Value -> Result String Elm.Expression
build inScope value =
    buildExampleCall { allowOptionalIntermediates = True } inScope value value.tipe []


buildExampleCall :
    { allowOptionalIntermediates : Bool }
    -> List Elm.Docs.Value
    -> Elm.Docs.Value
    -> Elm.Type.Type
    -> List Elm.Expression
    -> Result String Elm.Expression
buildExampleCall options inScope originalValue targetType args =
    case targetType of
        Elm.Type.Lambda arg result ->
            case buildArg options inScope arg of
                Ok argBuilt ->
                    case result of
                        Elm.Type.Lambda _ _ ->
                            buildExampleCall options inScope originalValue result (argBuilt :: args)

                        _ ->
                            Ok
                                (Elm.apply
                                    (Elm.value
                                        { importFrom = []
                                        , name = originalValue.name
                                        , annotation = Nothing
                                        }
                                    )
                                    (List.reverse (argBuilt :: args))
                                )

                Err err ->
                    Err err

        _ ->
            buildArg options inScope targetType


buildBuilder :
    { allowOptionalIntermediates : Bool }
    -> List Elm.Docs.Value
    -> Elm.Docs.Value
    -> Elm.Type.Type
    -> List Elm.Expression
    -> Result String Elm.Expression
buildBuilder options inScope originalValue targetType args =
    case targetType of
        Elm.Type.Lambda arg ((Elm.Type.Lambda _ _) as result) ->
            case buildArg options inScope arg of
                Ok argBuilt ->
                    buildBuilder options inScope originalValue result (argBuilt :: args)

                Err err ->
                    Err err

        Elm.Type.Lambda arg result ->
            -- `arg` is the last arg
            -- but because we're building a pipeline, we're skipping it
            Ok
                (Elm.apply
                    (Elm.value
                        { importFrom = []
                        , name = originalValue.name
                        , annotation = Nothing
                        }
                    )
                    (List.reverse args)
                )

        _ ->
            buildArg options inScope targetType


buildArg : { allowOptionalIntermediates : Bool } -> List Elm.Docs.Value -> Elm.Type.Type -> Result String Elm.Expression
buildArg options inScope target =
    case target of
        Elm.Type.Var var ->
            Err ("I don't know how to build a " ++ var)

        Elm.Type.Lambda arg result ->
            Err "Nested lambdas"

        Elm.Type.Tuple [] ->
            Ok Elm.unit

        Elm.Type.Tuple [ one, two ] ->
            case ( buildArg options inScope one, buildArg options inScope two ) of
                ( Ok oneBuilt, Ok twoBuilt ) ->
                    Ok (Elm.tuple oneBuilt twoBuilt)

                ( Err errOne, Ok _ ) ->
                    Err errOne

                ( _, Err errTwo ) ->
                    Err errTwo

        Elm.Type.Tuple [ one, two, three ] ->
            case ( buildArg options inScope one, buildArg options inScope two, buildArg options inScope three ) of
                ( Ok oneBuilt, Ok twoBuilt, Ok threeBuilt ) ->
                    Ok (Elm.triple oneBuilt twoBuilt threeBuilt)

                ( Err errOne, Ok _, Ok _ ) ->
                    Err errOne

                ( _, Err errTwo, Ok _ ) ->
                    Err errTwo

                ( _, _, Err errThree ) ->
                    Err errThree

        Elm.Type.Tuple _ ->
            Err "I don't know how to build a tuple with values other than a 0, 2, and three."

        Elm.Type.Type "String.String" [] ->
            Ok (Elm.string "Here is my string")

        Elm.Type.Type "Basics.Boolean" [] ->
            Ok (Elm.bool True)

        Elm.Type.Type "Basics.Int" [] ->
            Ok (Elm.int 5)

        Elm.Type.Type "Basics.Float" [] ->
            Ok (Elm.float 0.5)

        Elm.Type.Type "Basics.Bool" [] ->
            Ok (Elm.bool True)

        Elm.Type.Type "Maybe.Maybe" [ inner ] ->
            case buildArg options inScope inner of
                Err err ->
                    Err err

                Ok innerExample ->
                    Ok (Elm.just innerExample)

        Elm.Type.Type "List.List" [ inner ] ->
            case buildArg options inScope inner of
                Err err ->
                    Err err

                Ok innerExample ->
                    Ok (Elm.list [ innerExample ])

        Elm.Type.Type name vars ->
            -- Let's find a way to build this type
            List.foldl
                (\decl buildResult ->
                    case buildResult of
                        Ok _ ->
                            buildResult

                        Err _ ->
                            if decl.tipe |> Example.Type.isCreatorOf name then
                                if options.allowOptionalIntermediates then
                                    let
                                        builders =
                                            inScope
                                                |> List.filterMap
                                                    (\doc ->
                                                        case Example.Type.getBuilderOf name doc of
                                                            Nothing ->
                                                                Nothing

                                                            Just builder ->
                                                                buildBuilder { allowOptionalIntermediates = False } inScope builder builder.tipe []
                                                                    |> Result.toMaybe
                                                    )
                                    in
                                    case buildExampleCall { allowOptionalIntermediates = False } inScope decl decl.tipe [] of
                                        Ok builtValue ->
                                            Ok
                                                (List.foldl applyBuilder builtValue builders)

                                        Err err ->
                                            Err err

                                else
                                    buildExampleCall { allowOptionalIntermediates = False } inScope decl decl.tipe []

                            else
                                buildResult
                )
                (Err ("I don't know how to build a " ++ name))
                inScope

        Elm.Type.Record fields maybeName ->
            let
                renderedResult =
                    List.foldl
                        (\( fieldName, fieldType ) gathered ->
                            case gathered of
                                Err err ->
                                    gathered

                                Ok renderedFields ->
                                    case buildArg options inScope fieldType of
                                        Ok fieldExample ->
                                            Ok (( fieldName, fieldExample ) :: renderedFields)

                                        Err err ->
                                            Err err
                        )
                        (Ok [])
                        fields
            in
            case renderedResult of
                Ok rendered ->
                    Ok (Elm.record rendered)

                Err err ->
                    Err err


applyBuilder builder value =
    value |> Elm.Op.pipe builder
