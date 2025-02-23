module Exemplar exposing (..)

{-| First:
Given a set of type signatures:

    1. Produce arbitrary example code
    2. Lists are given 1 value, or possibly an example value for every option.

Second:
Given a set of type signatures:

    1. Produce a set of inputs that can drive the example code.
    2. Generate both a viewable result, as well as an example of what the code looks like.

-}

import Elm
import Elm.Annotation
import Elm.Case
import Elm.Docs
import Elm.Op
import Elm.Type
import Example.Build
import Example.Interactive
import Example.Type
import Gen.Html.Attributes
import Gen.Parser
import Gen.Ui
import Interactive


{-| Given a set of type signatures:

    1. Produce arbitrary example code
    2. Lists are given 1 value, or possibly an example value for every option.
    3. Recursive values should terminate.

Algorithm:

    1. Given a name of a value, build an expression that is an example of that call.

-}
example : String -> Elm.Docs.Module -> Result String Elm.Expression
example name mod =
    case Example.Build.getValueNamed name mod.values of
        Nothing ->
            Err ("Could not find " ++ name)

        Just value ->
            Example.Build.build mod.values value


interactive : String -> Elm.Docs.Module -> Result String Interactive.Module
interactive name mod =
    case Example.Build.getValueNamed name mod.values of
        Nothing ->
            Err ("Could not find " ++ name)

        Just value ->
            let
                builtResult =
                    Example.Interactive.build mod
                        { start = value
                        , runners =
                            [ element
                            , parser
                            ]
                        }
            in
            case builtResult of
                Err err ->
                    Err err

                Ok examp ->
                    Ok
                        { name = mod.name
                        , examples =
                            [ examp
                            ]
                        }


interactiveAll : Elm.Docs.Module -> Result String Interactive.Module
interactiveAll mod =
    let
        examples =
            List.foldl
                (\val exes ->
                    if Example.Type.isStartingPoint val.tipe then
                        case Example.Build.getValueNamed val.name mod.values of
                            Nothing ->
                                exes

                            Just value ->
                                let
                                    builtResult =
                                        Example.Interactive.build mod
                                            { start = value
                                            , runners =
                                                [ element
                                                , parser
                                                ]
                                            }
                                in
                                case builtResult of
                                    Err err ->
                                        exes

                                    Ok examp ->
                                        examp :: exes

                    else
                        exes
                )
                []
                mod.values
    in
    Ok
        { name = mod.name
        , examples =
            examples
        }


element : Example.Interactive.Runner
element =
    { canRun =
        \tipe ->
            case tipe of
                Elm.Type.Type "Element.Element" _ ->
                    True

                _ ->
                    False
    , view =
        \{ model, onChange } val ->
            Gen.Ui.el
                [ Gen.Ui.padding 32
                , Gen.Ui.height
                    Gen.Ui.shrink
                , Gen.Ui.heightMin 200
                ]
                (val
                    |> Gen.Ui.el [ Gen.Ui.centerY, Gen.Ui.centerX ]
                )
    , fields = []
    }


parser : Example.Interactive.Runner
parser =
    { canRun =
        \tipe ->
            case tipe of
                Elm.Type.Type "Parser.Parser" _ ->
                    True

                _ ->
                    False
    , view =
        \{ model, onChange } foundParser ->
            Elm.Case.result (Gen.Parser.call_.run foundParser (model |> Elm.get "source"))
                { ok =
                    Tuple.pair "ok" <|
                        \ok ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "toString"
                                    , annotation = Nothing
                                    }
                                )
                                [ ok ]
                                |> Gen.Ui.call_.text
                                |> Gen.Ui.el
                                    [ Gen.Ui.paddingXY 32 0
                                    , Gen.Ui.width Gen.Ui.fill
                                    , Gen.Ui.htmlAttribute (Gen.Html.Attributes.style "background" "rgb(36,36,36)")
                                    ]
                , err =
                    Tuple.pair "err" <|
                        \err ->
                            Elm.apply
                                (Elm.value
                                    { importFrom = [ "Debug" ]
                                    , name = "toString"
                                    , annotation = Nothing
                                    }
                                )
                                [ err ]
                                |> Gen.Ui.call_.text
                                |> Gen.Ui.el
                                    [ Gen.Ui.paddingXY 32 0
                                    , Gen.Ui.width Gen.Ui.fill
                                    , Gen.Ui.htmlAttribute (Gen.Html.Attributes.style "background" "rgb(36,36,36)")
                                    ]
                }
    , fields =
        [ Interactive.field "Source"
            { input = Interactive.string
            , init = Elm.string "# Hello"
            }
        ]
    }
