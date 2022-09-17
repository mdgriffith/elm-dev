module Example.Interactive.Build exposing (Created, build)

import Elm
import Elm.Annotation
import Elm.Case
import Elm.Docs
import Elm.Op
import Elm.Type
import Example.CallStack
import Example.Type
import Gen.Element
import Gen.String
import Gen.Ui
import Gen.Ui.Input
import Interactive


type alias Context =
    { state : List Interactive.Field
    , fieldIndex : Int
    , modul : Elm.Docs.Module

    -- So that we can refer to the model
    , model : Elm.Expression
    }


initContext : Elm.Docs.Module -> Context
initContext modul =
    { state = []
    , fieldIndex = 0
    , modul = modul
    , model =
        Elm.value
            { importFrom = []
            , name = "model"
            , annotation = Nothing
            }
    }


type alias Created =
    { context : Context

    -- an expression that has all the values that drive the expression in the form of `model.value`
    , drivenByModel :
        Elm.Expression
    }


type alias Options =
    { allowOptionalIntermediates : Bool
    }


formatInclusionName : String -> String
formatInclusionName name =
    "include" ++ capitalize name


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain


decapitalize : String -> String
decapitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toLower top ++ remain


getVal :
    String
    ->
        { input : Interactive.Input
        , init : Elm.Expression
        }
    -> Context
    -> Created
getVal nameBase options context =
    let
        arg =
            nameBase

        -- ++ String.fromInt context.fieldIndex
    in
    { drivenByModel = Elm.get arg context.model
    , context =
        { context
            | fieldIndex = context.fieldIndex + 1
            , state =
                Interactive.field arg options :: context.state
        }
    }


getValProtected :
    String
    ->
        { input : Interactive.Input
        , init : Elm.Expression
        }
    -> Context
    -> Created
getValProtected nameBase options context =
    let
        arg =
            nameBase ++ String.fromInt context.fieldIndex
    in
    { drivenByModel = Elm.get arg context.model
    , context =
        { context
            | fieldIndex = context.fieldIndex + 1
            , state =
                Interactive.field arg options :: context.state
        }
    }


build :
    Elm.Docs.Module
    -> Example.CallStack.CallStack
    -> Result String Created
build mod callstack =
    buildHelper
        { allowOptionalIntermediates = True
        , piped = False
        }
        (initContext mod)
        callstack


{-| Build the "executable" version of the callstack.
-}
buildHelper :
    { allowOptionalIntermediates : Bool
    , piped : Bool
    }
    -> Context
    -> Example.CallStack.CallStack
    -> Result String Created
buildHelper options context (Example.CallStack.CallStack callstack) =
    let
        starterCall =
            if options.piped then
                buildBuilder { allowOptionalIntermediates = options.allowOptionalIntermediates }
                    context
                    callstack.start
                    callstack.start.tipe
                    []

            else
                buildExampleCall { allowOptionalIntermediates = options.allowOptionalIntermediates }
                    context
                    { start = callstack.start
                    , end = \_ -> True
                    }
                    callstack.start.tipe
                    []
    in
    case starterCall of
        Ok call ->
            List.foldl
                (\step builtResult ->
                    case builtResult of
                        Ok built ->
                            if step.required then
                                case buildHelper { options | piped = True } built.context step.step of
                                    Ok builtStep ->
                                        Ok
                                            { context = builtStep.context
                                            , drivenByModel =
                                                built.drivenByModel
                                                    |> Elm.Op.pipe
                                                        builtStep.drivenByModel
                                            }

                                    Err err ->
                                        Err err

                            else
                                case Example.Type.getArgs <| .tipe <| Example.CallStack.start step.step of
                                    [ _ ] ->
                                        let
                                            boolVal =
                                                getVal (Example.CallStack.name step.step)
                                                    { input = Interactive.bool
                                                    , init = Elm.bool False
                                                    }
                                                    built.context
                                        in
                                        Ok
                                            { context = boolVal.context
                                            , drivenByModel =
                                                built.drivenByModel
                                                    |> Elm.Op.pipe
                                                        (Elm.ifThen boolVal.drivenByModel
                                                            (Elm.value
                                                                { importFrom = String.split "." context.modul.name
                                                                , name = Example.CallStack.name step.step
                                                                , annotation = Nothing
                                                                }
                                                            )
                                                            genIdentity
                                                        )
                                            }

                                    [ argType, _ ] ->
                                        case Interactive.fromType argType of
                                            Nothing ->
                                                -- skip, we don't have an input for this yet
                                                builtResult

                                            Just input ->
                                                let
                                                    maybeVal =
                                                        getVal (Example.CallStack.name step.step)
                                                            { input = Interactive.maybe input.input
                                                            , init = Elm.nothing
                                                            }
                                                            built.context
                                                in
                                                Ok
                                                    { context = maybeVal.context
                                                    , drivenByModel =
                                                        built.drivenByModel
                                                            |> Elm.Op.pipe
                                                                (Elm.Case.maybe maybeVal.drivenByModel
                                                                    { just =
                                                                        ( Example.CallStack.name step.step ++ "_option"
                                                                        , \val ->
                                                                            Elm.apply
                                                                                (Elm.value
                                                                                    { importFrom = String.split "." context.modul.name
                                                                                    , name = Example.CallStack.name step.step
                                                                                    , annotation = Nothing
                                                                                    }
                                                                                )
                                                                                [ val
                                                                                ]
                                                                        )
                                                                    , nothing =
                                                                        genIdentity
                                                                    }
                                                                )
                                                    }

                                    _ ->
                                        -- skip this builder, we ain't ready yet
                                        builtResult

                        Err err ->
                            Err err
                )
                (Ok call)
                (List.reverse callstack.steps)

        Err err ->
            Err err


genIdentity =
    Elm.fn ( "a", Nothing ) (\a -> a)


applyBuilder ( includeBuilder, builder ) value =
    value |> Elm.Op.pipe (Elm.ifThen includeBuilder builder genIdentity)


buildExampleCall :
    { allowOptionalIntermediates : Bool }
    -> Context
    ->
        { start : Elm.Docs.Value
        , end : Elm.Type.Type -> Bool
        }
    -> Elm.Type.Type
    -> List Elm.Expression
    -> Result String Created
buildExampleCall options context bounds targetType args =
    case targetType of
        Elm.Type.Lambda arg result ->
            case buildArg options context bounds.start.name arg of
                Ok argBuilt ->
                    case result of
                        Elm.Type.Lambda _ _ ->
                            buildExampleCall options argBuilt.context bounds result (argBuilt.drivenByModel :: args)

                        _ ->
                            -- if bounds.end result then
                            Ok
                                { context = argBuilt.context
                                , drivenByModel =
                                    Elm.apply
                                        (Elm.value
                                            { importFrom = String.split "." argBuilt.context.modul.name
                                            , name = bounds.start.name
                                            , annotation = Nothing
                                            }
                                        )
                                        (List.reverse (argBuilt.drivenByModel :: args))
                                }

                Err err ->
                    Err err

        _ ->
            Ok
                { context = context
                , drivenByModel =
                    Elm.value
                        { importFrom = String.split "." context.modul.name
                        , name = bounds.start.name
                        , annotation = Nothing
                        }
                }


buildBuilder :
    { allowOptionalIntermediates : Bool }
    -> Context
    -> Elm.Docs.Value
    -> Elm.Type.Type
    -> List Elm.Expression
    -> Result String Created
buildBuilder options context originalValue targetType args =
    case targetType of
        Elm.Type.Lambda arg ((Elm.Type.Lambda _ _) as result) ->
            case buildArg options context originalValue.name arg of
                Ok argBuilt ->
                    buildBuilder options argBuilt.context originalValue result (argBuilt.drivenByModel :: args)

                Err err ->
                    Err err

        Elm.Type.Lambda arg result ->
            -- `arg` is the last arg
            -- but because we're building a pipeline, we're skipping it
            Ok
                { context = context
                , drivenByModel =
                    Elm.apply
                        (Elm.value
                            { importFrom = String.split "." context.modul.name
                            , name = originalValue.name
                            , annotation = Nothing
                            }
                        )
                        (List.reverse args)
                }

        _ ->
            buildArg options context originalValue.name targetType


buildArg :
    { allowOptionalIntermediates : Bool }
    -> Context
    -> String
    -> Elm.Type.Type
    -> Result String Created
buildArg options context namespace target =
    case target of
        Elm.Type.Var "msg" ->
            Ok
                { context = context
                , drivenByModel = Interactive.log
                }

        Elm.Type.Var var ->
            Err ("I don't know how to build a " ++ var)

        Elm.Type.Lambda arg result ->
            Err "Nested lambdas"

        Elm.Type.Tuple [] ->
            Ok
                { context = context
                , drivenByModel = Elm.unit
                }

        Elm.Type.Tuple [ one, two ] ->
            case buildArg options context namespace one of
                Ok oneBuilt ->
                    case buildArg options oneBuilt.context namespace two of
                        Ok twoBuilt ->
                            Ok
                                { context = twoBuilt.context
                                , drivenByModel = Elm.tuple oneBuilt.drivenByModel twoBuilt.drivenByModel
                                }

                        Err err ->
                            Err err

                Err err ->
                    Err err

        Elm.Type.Tuple [ one, two, three ] ->
            case buildArg options context namespace one of
                Ok oneBuilt ->
                    case buildArg options oneBuilt.context namespace two of
                        Ok twoBuilt ->
                            case buildArg options twoBuilt.context namespace three of
                                Ok threeBuilt ->
                                    Ok
                                        { context = threeBuilt.context
                                        , drivenByModel =
                                            Elm.triple oneBuilt.drivenByModel
                                                twoBuilt.drivenByModel
                                                threeBuilt.drivenByModel
                                        }

                                Err err ->
                                    Err err

                        Err err ->
                            Err err

                Err err ->
                    Err err

        Elm.Type.Tuple _ ->
            Err "I don't know how to build a tuple with values other than a 0, 2, and three."

        Elm.Type.Type "String.String" [] ->
            Ok <|
                getVal namespace
                    { input = Interactive.string
                    , init = Elm.string "Button"
                    }
                    context

        Elm.Type.Type "Basics.Boolean" [] ->
            Ok <|
                getVal namespace
                    { input = Interactive.bool
                    , init = Elm.bool True
                    }
                    context

        Elm.Type.Type "Basics.Int" [] ->
            Ok <|
                getVal namespace
                    { input = Interactive.int
                    , init = Elm.int 1
                    }
                    context

        Elm.Type.Type "Basics.Float" [] ->
            Ok <|
                getVal namespace
                    { input = Interactive.float
                    , init = Elm.float 1
                    }
                    context

        Elm.Type.Type "Basics.Bool" [] ->
            Ok <|
                getVal namespace
                    { input = Interactive.bool
                    , init = Elm.bool True
                    }
                    context

        Elm.Type.Type "Maybe.Maybe" [ inner ] ->
            case buildArg options context namespace inner of
                Err err ->
                    Err err

                Ok innerExample ->
                    Ok
                        { context = innerExample.context
                        , drivenByModel = Elm.just innerExample.drivenByModel
                        }

        Elm.Type.Type "List.List" [ inner ] ->
            case buildArg options context namespace inner of
                Err err ->
                    Err err

                Ok innerExample ->
                    Ok
                        { context = innerExample.context
                        , drivenByModel = Elm.list [ innerExample.drivenByModel ]
                        }

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
                                        ( buildersContext, builders ) =
                                            context.modul.values
                                                |> List.foldl
                                                    (\doc (( ctxt, existingBuilders ) as untouched) ->
                                                        case Example.Type.getBuilderOf name doc of
                                                            Nothing ->
                                                                untouched

                                                            Just builder ->
                                                                let
                                                                    builtBuilderResult =
                                                                        buildBuilder { allowOptionalIntermediates = False }
                                                                            ctxt
                                                                            builder
                                                                            builder.tipe
                                                                            []
                                                                in
                                                                case builtBuilderResult of
                                                                    Err _ ->
                                                                        untouched

                                                                    Ok builtBuilder ->
                                                                        let
                                                                            builderSwitch =
                                                                                getValProtected "includeBuilder"
                                                                                    { input = Interactive.bool
                                                                                    , init = Elm.bool False
                                                                                    }
                                                                                    builtBuilder.context
                                                                        in
                                                                        ( builderSwitch.context
                                                                        , ( builderSwitch.drivenByModel, builtBuilder.drivenByModel ) :: existingBuilders
                                                                        )
                                                    )
                                                    ( context
                                                    , []
                                                    )

                                        exampleCall =
                                            buildExampleCall { allowOptionalIntermediates = False }
                                                buildersContext
                                                { start = decl
                                                , end = \_ -> True
                                                }
                                                decl.tipe
                                                []
                                    in
                                    case exampleCall of
                                        Ok builtValue ->
                                            Ok
                                                { context = builtValue.context
                                                , drivenByModel =
                                                    List.foldl applyBuilder builtValue.drivenByModel builders
                                                }

                                        Err err ->
                                            Err err

                                else
                                    buildExampleCall { allowOptionalIntermediates = False }
                                        context
                                        { start = decl
                                        , end = \_ -> True
                                        }
                                        decl.tipe
                                        []

                            else
                                buildResult
                )
                (Err ("I don't know how to build a " ++ name))
                context.modul.values

        Elm.Type.Record fields maybeName ->
            let
                renderedResult =
                    List.foldl
                        (\( fieldName, fieldType ) gathered ->
                            case gathered of
                                Err err ->
                                    gathered

                                Ok ( currentContext, renderedFields ) ->
                                    case buildArg options currentContext fieldName fieldType of
                                        Ok fieldExample ->
                                            Ok
                                                ( fieldExample.context
                                                , ( fieldName, fieldExample.drivenByModel ) :: renderedFields
                                                )

                                        Err err ->
                                            Err err
                        )
                        (Ok ( context, [] ))
                        fields
            in
            case renderedResult of
                Ok ( newContext, rendered ) ->
                    Ok
                        { context = newContext
                        , drivenByModel = Elm.record rendered
                        }

                Err err ->
                    Err err
