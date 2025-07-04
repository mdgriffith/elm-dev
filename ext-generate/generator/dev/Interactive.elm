module Interactive exposing
    ( generateSingle
    , Module, Interactive, ViewReferences
    , Field, field
    , log
    , Input(..), bool, string, int, float
    , fromType
    , details, maybe, portFile
    )

{-|

@docs generateSingle

@docs Module, Interactive, ViewReferences, generate

@docs Field, field

@docs log

@docs Input, bool, string, int, float

@docs fromType

-}

import Elm
import Elm.Annotation
import Elm.Arg
import Elm.Case
import Elm.Declare
import Elm.Docs
import Elm.Op
import Elm.Type
import Gen.Browser
import Gen.Html
import Gen.Html.Attributes
import Gen.Html.Events
import Gen.Json.Decode
import Gen.Json.Encode
import Gen.Maybe
import Gen.Platform
import Gen.Platform.Cmd
import Gen.Platform.Sub
import Gen.String
import GenApp
import Ui


type alias Module =
    { name : String
    , examples : List Interactive
    }


{-| All the information needed for an interactive example.
-}
type alias Interactive =
    { name : String
    , fields : List Field
    , view :
        ViewReferences
        -> Elm.Expression
    }


type alias ViewReferences =
    { model : Elm.Expression
    , codeOrOutput : Elm.Expression
    , onChange : Elm.Expression
    }


field :
    String
    ->
        { input : Input
        , init : Elm.Expression
        }
    -> Field
field =
    Field


type Field
    = Field
        String
        { init : Elm.Expression
        , input : Input
        }


type Input
    = InputString
    | InputBool
    | InputInt
    | InputFloat
    | InputMaybe Input


bool : Input
bool =
    InputBool


string : Input
string =
    InputString


int : Input
int =
    InputInt


float : Input
float =
    InputFloat


maybe : Input -> Input
maybe =
    InputMaybe


fromType :
    Elm.Type.Type
    ->
        Maybe
            { input : Input
            , init : Elm.Expression
            }
fromType tipe =
    case tipe of
        Elm.Type.Type "String.String" [] ->
            Just
                { input = InputString
                , init = Elm.string ""
                }

        Elm.Type.Type "Basics.Bool" [] ->
            Just
                { input = InputBool
                , init = Elm.bool False
                }

        Elm.Type.Type "Basics.Int" [] ->
            Just
                { input = InputInt
                , init = Elm.int 0
                }

        Elm.Type.Type "Basics.Float" [] ->
            Just
                { input = InputFloat
                , init = Elm.float 0
                }

        _ ->
            Nothing


details :
    Field
    ->
        { label : String
        , key : String
        , input : Input
        , onChange : Input
        }
details (Field name opts) =
    { label =
        if String.startsWith "with" name then
            name |> String.replace "with" ""

        else
            name
    , key = name
    , input = opts.input
    , onChange = opts.input
    }


inputToAnnotation : Input -> Elm.Annotation.Annotation
inputToAnnotation input =
    case input of
        InputString ->
            Elm.Annotation.string

        InputBool ->
            Elm.Annotation.bool

        InputInt ->
            Elm.Annotation.int

        InputFloat ->
            Elm.Annotation.float

        InputMaybe inner ->
            Elm.Annotation.maybe (inputToAnnotation inner)


appTypes =
    { model = Elm.Annotation.named [] "Model"
    , msg = Elm.Annotation.named [] "Msg"
    }



{- END ADDITIONAL STUFF -}


generateSingle : List String -> Interactive -> Elm.File
generateSingle moduleName interact =
    GenApp.element moduleName
        { init =
            \flags ->
                Elm.tuple
                    (Elm.record
                        (List.map
                            (\(Field name info) ->
                                ( name, info.init )
                            )
                            interact.fields
                        )
                    )
                    (sendControlsUpdated interact.fields)
        , model = fieldsToAnnotation interact.fields
        , messageHandlers =
            [ { name = "Updated"
              , data = [ fieldsToAnnotation interact.fields ]
              , updateBranch =
                    \model ->
                        Elm.Case.branch
                            (Elm.Arg.customType "Updated" Tuple.pair
                                |> Elm.Arg.item (Elm.Arg.var "name")
                                |> Elm.Arg.item (Elm.Arg.var "jsonValue")
                            )
                            (\( name, jsonValue ) ->
                                Elm.Case.string name
                                    { cases =
                                        List.map
                                            (\(Field fieldName info) ->
                                                ( fieldName
                                                , Elm.Case.result
                                                    (decode info.input jsonValue)
                                                    { ok =
                                                        ( "val"
                                                        , \decodedValue ->
                                                            Elm.tuple
                                                                (model
                                                                    |> Elm.updateRecord
                                                                        [ ( fieldName, decodedValue )
                                                                        ]
                                                                )
                                                                Gen.Platform.Cmd.none
                                                        )
                                                    , err =
                                                        ( "err"
                                                        , \error ->
                                                            Elm.tuple
                                                                model
                                                                Gen.Platform.Cmd.none
                                                        )
                                                    }
                                                )
                                            )
                                            interact.fields
                                    , otherwise =
                                        Elm.tuple
                                            model
                                            Gen.Platform.Cmd.none
                                    }
                            )
              }
            , { name = "UnknownProperty"
              , data = [ fieldsToAnnotation interact.fields ]
              , updateBranch =
                    \model ->
                        Elm.Case.branch
                            (Elm.Arg.customType "UnknownProperty" identity
                                |> Elm.Arg.item (Elm.Arg.varWith "name" Elm.Annotation.string)
                            )
                            (\name ->
                                Elm.tuple
                                    model
                                    Gen.Platform.Cmd.none
                            )
              }
            ]
        , view =
            \model ->
                interact.view
                    { model = model
                    , codeOrOutput = Elm.bool True

                    -- Elm.Op.equal
                    --     (focus.get model)
                    --     (Elm.value
                    --         { importFrom = []
                    --         , name = focus.variants.output
                    --         , annotation = Just focus.type_
                    --         }
                    --     )
                    , onChange =
                        Elm.value
                            { importFrom = []
                            , name =
                                capitalize interact.name
                            , annotation = Nothing
                            }
                    }
                    |> Elm.withType
                        (Ui.annotation_.element
                            (Elm.Annotation.named [] "Msg")
                        )
        , subscriptions =
            \_ ->
                Elm.apply
                    (Elm.value
                        { importFrom = [ "Ports" ]
                        , name = "propertyUpdated"
                        , annotation = Nothing
                        }
                    )
                    [ Elm.fn2
                        (Elm.Arg.varWith "key" Elm.Annotation.string)
                        (Elm.Arg.varWith "value" Gen.Json.Encode.annotation_.value)
                        (\key value ->
                            Elm.apply (Elm.val "PropertyUpdated") [ key, value ]
                        )
                    ]
        , declarations =
            []
        }


decode : Input -> Elm.Expression -> Elm.Expression
decode input val =
    Gen.Json.Decode.decodeValue
        (toDecoderHelper input)
        val


toDecoderHelper : Input -> Elm.Expression
toDecoderHelper input =
    case input of
        InputString ->
            Gen.Json.Decode.string

        InputBool ->
            Gen.Json.Decode.bool

        InputInt ->
            Gen.Json.Decode.int

        InputFloat ->
            Gen.Json.Decode.float

        InputMaybe inner ->
            Gen.Json.Decode.maybe (toDecoderHelper inner)



{--PORTS --}


{-| This file is shared between all interactive examples.
-}
portFile : Elm.File
portFile =
    Elm.file [ "Ports" ]
        [ -- We've updated a property, send the new value to the outside
          Elm.portIncoming "propertyUpdated"
            [ Elm.Annotation.string
            , Gen.Json.Encode.annotation_.value
            ]
        , -- These are what controls we need to drive the UI
          Elm.portOutgoing "controlsUpdated"
            Gen.Json.Encode.annotation_.value
        ]


sendControlsUpdated : List Field -> Elm.Expression
sendControlsUpdated fields =
    Elm.apply
        (Elm.value
            { importFrom = [ "Ports" ]
            , name = "controlsUpdated"
            , annotation = Nothing
            }
        )
        [ encodeControls fields ]


keyValue : String -> Elm.Expression -> Elm.Expression
keyValue key value =
    Elm.tuple
        (Elm.string key)
        value


encodeControls : List Field -> Elm.Expression
encodeControls fields =
    Gen.Json.Encode.object
        (List.map
            (\(Field name info) ->
                keyValue
                    name
                    (Gen.Json.Encode.object
                        [ keyValue "name" (Gen.Json.Encode.string name)
                        , keyValue "type" (Gen.Json.Encode.string (inputToString info.input))
                        , keyValue "required" (Gen.Json.Encode.bool (isRequired info.input))
                        , keyValue "default" info.init
                        ]
                    )
            )
            fields
        )


inputToString : Input -> String
inputToString input =
    case input of
        InputString ->
            "string"

        InputBool ->
            "bool"

        InputInt ->
            "int"

        InputFloat ->
            "float"

        InputMaybe inner ->
            inputToString inner


isRequired : Input -> Bool
isRequired input =
    case input of
        InputMaybe _ ->
            False

        _ ->
            True


log : Elm.Expression
log =
    Elm.value
        { importFrom = []
        , name = "Log"
        , annotation = Nothing
        }



-- logMsg : Elm.Variant
-- logMsg =
--     Elm.variant "Log"
-- update modelAlias mod additional =
--     Elm.declaration "update"
--         (Elm.fn2
--             (Elm.Arg.varWith "msg" (Elm.Annotation.named [] "Msg"))
--             (Elm.Arg.varWith "model" modelAlias)
--             (\msg model ->
--                 Elm.Case.custom msg
--                     (Elm.Annotation.named [] "Msg")
--                     (logUpdate model
--                         :: additional.focus.updateBranch model
--                         :: additional.example.updateBranch model
--                         :: additional.example.updateMenuBranch model
--                         :: toMsgUpdate
--                             model
--                             mod
--                     )
--                     |> Elm.withType
--                         (Elm.Annotation.tuple modelAlias
--                             (Gen.Platform.Cmd.annotation_.cmd
--                                 (Elm.Annotation.named [] "Msg")
--                             )
--                         )
--             )
--         )
-- logUpdate model =
--     Elm.Case.branch
--         (Elm.Arg.customType "Log" identity)
--         (\msg ->
--             Elm.tuple
--                 model
--                 Gen.Platform.Cmd.none
--         )
-- moduleToTabName : Module -> String
-- moduleToTabName mod =
--     mod.name |> String.replace "." ""
-- init mod additional =
--     Elm.declaration "init"
--         (Elm.fn2
--             (Elm.Arg.varWith "params" (Elm.Annotation.var "params"))
--             (Elm.Arg.varWith "shared" (Elm.Annotation.var "shared"))
--             (\params shared ->
--                 Elm.tuple
--                     (Elm.record
--                         (additional.focus.init
--                             :: additional.example.init
--                             ++ toInitFields mod
--                         )
--                     )
--                     Gen.Platform.Cmd.none
--                     |> Elm.withType
--                         (Elm.Annotation.tuple
--                             (Elm.Annotation.named [] "Model")
--                             (Gen.Platform.Cmd.annotation_.cmd (Elm.Annotation.var "msg"))
--                         )
--             )
--         )
-- view modelAlias mod additional =
--     Elm.declaration "view"
--         (Elm.fn (Elm.Arg.varWith "model" modelAlias)
--             (\model ->
--                 Elm.record
--                     [ ( "title", Elm.string "Elm Interactive" )
--                     , ( "body", viewBody model mod additional )
--                     ]
--              -- |> Elm.withType (Gen.App.View.annotation_.view (Elm.Annotation.named [] "Msg"))
--             )
--         )
-- viewBody model mod additional =
--     let
--         examplePicker =
--             Ui.el
--                 [ Ui.fontSize 24
--                 , Ui.paddingXY 32 10
--                 , Ui.pointer
--                 , Ui.fontFamily
--                     [ "Fira Code"
--                     , "sans-serif"
--                     ]
--                 , Gen.Html.Events.onClick
--                     (additional.example.toggleMenu (additional.example.getMenuOpen model))
--                 -- , Elm.ifThen (additional.example.getMenuOpen model)
--                 --     (Ui.below
--                 --         (mod.examples
--                 --             |> List.indexedMap
--                 --                 (\optionIndex option ->
--                 --                     Ui.text option.name
--                 --                         |> Ui.el
--                 --                             [ Gen.Html.Events.onClick
--                 --                                 (additional.example.onClick optionIndex)
--                 --                             ]
--                 --                 )
--                 --             |> Ui.column
--                 --                 [ Ui.padding 16
--                 --                 , Ui.move
--                 --                     (Ui.right 32)
--                 --                 , Ui.border 1
--                 --                 , Ui.rounded 4
--                 --                 , Ui.background (Ui.rgb 0 0 0)
--                 --                 , Ui.spacing 8
--                 --                 ]
--                 --         )
--                 --     )
--                 --     Ui.pointer
--                 ]
--                 (Ui.text
--                     ("▶ " ++ mod.name)
--                 )
--     in
--     Ui.layers
--         [ Ui.htmlAttribute (Gen.Html.Attributes.style "background" "rgb(36,36,36)")
--         , Ui.fontColor (Ui.rgb 1 1 1)
--         , Ui.fontFamily
--             [ "Fira Code"
--             , "sans-serif"
--             ]
--         ]
--         [ Ui.column
--             [ Ui.height Ui.fill
--             , Ui.spacing 16
--             ]
--             (examplePicker
--                 :: List.indexedMap
--                     (\index interact ->
--                         Elm.ifThen (Elm.Op.equal (Elm.int index) (additional.example.get model))
--                             (Ui.column
--                                 [ Ui.height Ui.fill
--                                 ]
--                                 [ Elm.apply
--                                     (Elm.val ("view" ++ capitalize interact.name))
--                                     [ model
--                                     , Elm.get interact.name model
--                                     ]
--                                 ]
--                             )
--                             Ui.none
--                     )
--                     mod.examples
--             )
--         , additional.focus.viewCall model
--         ]
-- renderViewer focus mod =
--     List.map (renderInteractiveViewer focus) mod.examples
-- renderInteractiveViewer focus interact =
--     Elm.declaration ("view" ++ capitalize interact.name)
--         (Elm.fn2
--             (Elm.Arg.varWith "parent" appTypes.model)
--             (Elm.Arg.varWith "model" (toModelField interact |> Tuple.second))
--             (\model submodel ->
--                 interact.view
--                     { model = submodel
--                     , codeOrOutput =
--                         Elm.Op.equal
--                             (focus.get model)
--                             (Elm.value
--                                 { importFrom = []
--                                 , name = focus.variants.output
--                                 , annotation = Just focus.type_
--                                 }
--                             )
--                     , onChange =
--                         Elm.value
--                             { importFrom = []
--                             , name =
--                                 capitalize interact.name
--                             , annotation = Nothing
--                             }
--                     }
--                     |> Elm.withType
--                         (Ui.annotation_.element
--                             (Elm.Annotation.named [] "Msg")
--                         )
--             )
--         )
-- toModuleFields : Module -> List ( String, Elm.Annotation.Annotation )
-- toModuleFields mod =
--     List.map toModelField mod.examples
-- toModelField : Interactive -> ( String, Elm.Annotation.Annotation )
-- toModelField interact =
--     ( interact.name
--     , fieldsToAnnotation
--         interact.fields
--     )


fieldsToAnnotation : List Field -> Elm.Annotation.Annotation
fieldsToAnnotation fields =
    Elm.Annotation.record
        (List.map
            (\(Field name info) ->
                ( name, inputToAnnotation info.input )
            )
            fields
        )



-- toInitFields : Module -> List ( String, Elm.Expression )
-- toInitFields mod =
--     List.map toInteractiveInitFields mod.examples
-- toInteractiveInitFields interact =
--     ( interact.name
--     , Elm.record
--         (List.map
--             (\(Field name info) ->
--                 ( name, info.init )
--             )
--             interact.fields
--         )
--     )
-- toMsgVariant : Module -> List Elm.Variant
-- toMsgVariant mod =
--     List.map toMsgVariantInteractive mod.examples
-- toMsgVariantInteractive interact =
--     Elm.variantWith interact.name
--         [ fieldsToAnnotation
--             interact.fields
--         ]
-- toMsgUpdate : Elm.Expression -> Module -> List Elm.Case.Branch
-- toMsgUpdate model mod =
--     List.map (toMsgUpdateInteractive model) mod.examples
-- toMsgUpdateInteractive : Elm.Expression -> Interactive -> Elm.Case.Branch
-- toMsgUpdateInteractive model interact =
--     Elm.Case.branch
--         (Elm.Arg.customType interact.name identity
--             |> Elm.Arg.item
--                 (Elm.Arg.varWith "updated" (fieldsToAnnotation interact.fields))
--         )
--         (\updated ->
--             Elm.tuple
--                 (model
--                     |> Elm.updateRecord
--                         [ ( interact.name, updated )
--                         ]
--                 )
--                 Gen.Platform.Cmd.none
--         )
-- page : Elm.Declaration
-- page =
--     Elm.record
--         [ ( "init"
--           , Elm.value
--                 { importFrom = []
--                 , name = "init"
--                 , annotation =
--                     Just
--                         (Elm.Annotation.function
--                             [ Elm.Annotation.unit
--                             ]
--                             (Elm.Annotation.tuple
--                                 appTypes.model
--                                 (Gen.Platform.Cmd.annotation_.cmd (Elm.Annotation.var "msg"))
--                             )
--                         )
--                 }
--           )
--         , ( "update"
--           , Elm.value
--                 { importFrom = []
--                 , name = "update"
--                 , annotation =
--                     Just
--                         (Elm.Annotation.function
--                             [ appTypes.msg
--                             , appTypes.model
--                             ]
--                             (Elm.Annotation.tuple
--                                 appTypes.model
--                                 (Gen.Platform.Cmd.annotation_.cmd (Elm.Annotation.var "msg"))
--                             )
--                         )
--                 }
--           )
--         , ( "view"
--           , Elm.value
--                 { importFrom = []
--                 , name = "view"
--                 , annotation =
--                     Nothing
--                 }
--           )
--         , ( "subscriptions"
--           , Elm.fn
--                 (Elm.Arg.var "model")
--                 (\_ ->
--                     Gen.Platform.Sub.none
--                 )
--           )
--         ]
--         |> Gen.Browser.call_.element
--         |> Elm.withType
--             (Gen.Platform.annotation_.program
--                 (Elm.Annotation.var "params")
--                 (Elm.Annotation.named [] "Model")
--                 (Elm.Annotation.named [] "Msg")
--             )
--         |> Elm.declaration "page"


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain
