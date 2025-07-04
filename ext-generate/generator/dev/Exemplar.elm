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
import Gen.Html
import Gen.Html.Attributes
import Gen.Parser
import Interactive
import Ui


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


runners : List Example.Interactive.Runner
runners =
    [ parser
    , html
    , string
    ]


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
                        , runners = runners
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


interactiveAll :
    Elm.Docs.Module
    ->
        { name : String
        , examples :
            List
                (Result
                    { name : String
                    , description : String
                    }
                    Interactive.Interactive
                )
        }
interactiveAll mod =
    { name = mod.name
    , examples =
        List.foldl
            (\val examples ->
                if Example.Type.isStartingPoint val.tipe then
                    let
                        builtResult =
                            Example.Interactive.build mod
                                { start = val
                                , runners = runners
                                }
                    in
                    case builtResult of
                        Err err ->
                            Err { name = val.name, description = err } :: examples

                        Ok examp ->
                            Ok examp :: examples

                else
                    let
                        isBuilder =
                            if Example.Type.isBuilder val.tipe then
                                "true"

                            else
                                "false"

                        debugStartingPoint =
                            Example.Type.debugStartingPoint val.tipe
                                |> String.join "\n"
                    in
                    Err
                        { name = val.name
                        , description =
                            "Not a starting point `"
                                ++ (val.name ++ "`" ++ "\n" ++ "isBuilder: " ++ isBuilder)
                                ++ "\n"
                                ++ debugStartingPoint
                        }
                        :: examples
            )
            []
            mod.values
    }



-- element : Example.Interactive.Runner
-- element =
--     { canRun =
--         \tipe ->
--             case tipe of
--                 Elm.Type.Type "Element.Element" _ ->
--                     True
--                 _ ->
--                     False
--     , view =
--         \{ model, onChange } val ->
--             Ui.el
--                 [ Ui.padding 32
--                 , Ui.height
--                     Ui.shrink
--                 , Ui.heightMin 200
--                 ]
--                 (val
--                     |> Ui.el [ Ui.centerY, Ui.centerX ]
--                 )
--     , fields = []
--     }


string : Example.Interactive.Runner
string =
    { canRun =
        \tipe ->
            case tipe of
                Elm.Type.Type "String.String" [] ->
                    True

                _ ->
                    False
    , view =
        \{ model, onChange } val ->
            Gen.Html.call_.text val
    , fields = []
    }


html : Example.Interactive.Runner
html =
    { canRun =
        \tipe ->
            case tipe of
                Elm.Type.Type "Html.Html" _ ->
                    True

                _ ->
                    False
    , view =
        \{ model, onChange } val ->
            val
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
                                |> Gen.Html.call_.text
                                |> Ui.el
                                    [ Ui.paddingXY 32 0
                                    , Ui.width Ui.fill
                                    , Ui.htmlAttribute (Gen.Html.Attributes.style "background" "rgb(36,36,36)")
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
                                |> Gen.Html.call_.text
                                |> Ui.el
                                    [ Ui.paddingXY 32 0
                                    , Ui.width Ui.fill
                                    , Ui.htmlAttribute (Gen.Html.Attributes.style "background" "rgb(36,36,36)")
                                    ]
                }
    , fields =
        [ Interactive.field "Source"
            { input = Interactive.string
            , init = Elm.string "# Hello"
            }
        ]
    }
