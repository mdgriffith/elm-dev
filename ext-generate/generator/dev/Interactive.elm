module Interactive exposing
    ( generateSingle
    , Module, Interactive, ViewReferences
    , Field, field
    , log
    , Input(..), bool, string, int, float
    , fromType
    , details, maybe
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
    let
        moduleId =
            String.join "" moduleName

        portInName =
            "propertyUpdated" ++ moduleId

        portOutName =
            "controlsUpdated" ++ moduleId
    in
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
                    (sendControlsUpdated portOutName interact.fields)
        , model = fieldsToAnnotation interact.fields
        , messageHandlers =
            List.map
                (\(Field fieldName info) ->
                    let
                        variantName =
                            "Property" ++ capitalize fieldName ++ "Updated"
                    in
                    { name = variantName
                    , data = [ inputToAnnotation info.input ]
                    , updateBranch =
                        \model ->
                            Elm.Case.branch
                                (Elm.Arg.customType variantName identity
                                    |> Elm.Arg.item (Elm.Arg.varWith "value" (inputToAnnotation info.input))
                                )
                                (\value ->
                                    Elm.tuple
                                        (model
                                            |> Elm.updateRecord
                                                [ ( fieldName, value )
                                                ]
                                        )
                                        Gen.Platform.Cmd.none
                                )
                    }
                )
                interact.fields
                ++ [ { name = "UnknownProperty"
                     , data = [ Elm.Annotation.string ]
                     , updateBranch =
                        \model ->
                            Elm.Case.branch
                                (Elm.Arg.customType "UnknownProperty" identity
                                    |> Elm.Arg.item (Elm.Arg.varWith "name" Elm.Annotation.string)
                                )
                                (\_ ->
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
                    (Elm.value { importFrom = [], name = portInName, annotation = Nothing })
                    [ Elm.fn
                        (Elm.Arg.varWith "payload"
                            (Elm.Annotation.record
                                [ ( "name", Elm.Annotation.string )
                                , ( "value", Gen.Json.Encode.annotation_.value )
                                ]
                            )
                        )
                        (\payload ->
                            Elm.Case.string (Elm.get "name" payload)
                                { cases =
                                    List.map
                                        (\(Field fieldName info) ->
                                            let
                                                variantName =
                                                    "Property" ++ capitalize fieldName ++ "Updated"
                                            in
                                            ( fieldName
                                            , Elm.Case.result
                                                (decode info.input (Elm.get "value" payload))
                                                { ok =
                                                    ( "val"
                                                    , \decodedValue ->
                                                        Elm.apply (Elm.val variantName) [ decodedValue ]
                                                    )
                                                , err =
                                                    ( "err"
                                                    , \_ ->
                                                        Elm.apply (Elm.val "UnknownProperty") [ Elm.get "name" payload ]
                                                    )
                                                }
                                            )
                                        )
                                        interact.fields
                                , otherwise =
                                    Elm.apply (Elm.val "UnknownProperty") [ Elm.get "name" payload ]
                                }
                        )
                    ]
                    |> Elm.withType
                        (Elm.Annotation.namedWith []
                            "Sub"
                            [ Elm.Annotation.named [] "Msg"
                            ]
                        )
        , declarations =
            [ Elm.portIncoming portInName
                [ Elm.Annotation.record
                    [ ( "name", Elm.Annotation.string )
                    , ( "value", Gen.Json.Encode.annotation_.value )
                    ]
                ]
            , Elm.portOutgoing portOutName
                Gen.Json.Encode.annotation_.value
            ]
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


sendControlsUpdated : String -> List Field -> Elm.Expression
sendControlsUpdated portName fields =
    Elm.apply
        (Elm.value { importFrom = [], name = portName, annotation = Nothing })
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
                        , keyValue "default" (encodeDefault info.input info.init)
                        ]
                    )
            )
            fields
        )


encodeDefault : Input -> Elm.Expression -> Elm.Expression
encodeDefault input expr =
    case input of
        InputString ->
            Gen.Json.Encode.call_.string expr

        InputBool ->
            Gen.Json.Encode.call_.bool expr

        InputInt ->
            Gen.Json.Encode.call_.int expr

        InputFloat ->
            Gen.Json.Encode.call_.float expr

        InputMaybe inner ->
            encodeDefault inner expr


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

