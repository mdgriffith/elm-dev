module Interactive exposing
    ( Module, Interactive, ViewReferences, generate
    , Field, field
    , log
    , Input(..), bool, string, int, float
    , fromType
    , details, maybe
    )

{-|

@docs Module, Interactive, ViewReferences, generate

@docs Field, field

@docs log

@docs Input, bool, string, int, float

@docs fromType

-}

import Elm
import Elm.Annotation
import Elm.Case
import Elm.Declare
import Elm.Docs
import Elm.Op
import Elm.Type
import Gen.App.Effect
import Gen.App.Page
import Gen.App.Sub
import Gen.App.View
import Gen.Html
import Gen.Html.Attributes
import Gen.Maybe
import Gen.Platform.Cmd
import Gen.Platform.Sub
import Gen.String
import Gen.Ui
import Gen.Ui.Events
import Gen.Ui.Font


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


{-| -}
selected top modules =
    let
        recordName =
            "selectedModule_"

        msgName =
            "TabUpdated"

        typeName =
            "Selected"

        type_ =
            Elm.Annotation.named [] typeName
    in
    { type_ = type_
    , model = ( recordName, type_ )
    , init =
        ( recordName
        , Elm.value
            { importFrom = []
            , name = moduleToTabName top
            , annotation = Just type_
            }
        )
    , msg =
        Elm.variantWith msgName
            [ Elm.Annotation.named [] typeName
            ]
    , updateBranch =
        \model ->
            Elm.Case.branch1 msgName
                ( "newTab", Elm.Annotation.named [] typeName )
                (\tab ->
                    Elm.tuple
                        (model
                            |> Elm.updateRecord
                                [ ( recordName, tab )
                                ]
                        )
                        Gen.App.Effect.none
                )
    , view =
        Elm.Declare.fn "viewTab"
            ( "tab", Just type_ )
            (\tab ->
                Gen.Ui.row
                    [ Gen.Ui.spacing 24
                    , Gen.Ui.padding 24
                    ]
                    (List.map
                        (\mod ->
                            Gen.Ui.el
                                [ Gen.Ui.padding 12
                                , Gen.Ui.Events.onClick
                                    (Elm.apply
                                        (Elm.value
                                            { importFrom = []
                                            , name = msgName
                                            , annotation = Just (Elm.Annotation.function [ type_ ] appTypes.msg)
                                            }
                                        )
                                        [ Elm.value
                                            { importFrom = []
                                            , name = moduleToTabName mod
                                            , annotation = Just type_
                                            }
                                        ]
                                    )
                                ]
                                (Gen.Ui.text mod.name)
                        )
                        modules
                    )
            )
    , toString =
        Elm.Declare.fn "tabToString"
            ( "tab", Just type_ )
            (\tab ->
                Elm.Case.custom tab
                    type_
                    (List.map
                        (\mod ->
                            Elm.Case.branch0 (moduleToTabName mod)
                                (Elm.string mod.name)
                        )
                        modules
                    )
            )
    , declaration =
        Elm.customType typeName
            (List.map (Elm.variant << moduleToTabName) modules)
    }


{-| The index of which example should be visible
-}
selectedExample =
    let
        recordName =
            "selectedExample_"

        msgName =
            "SelectedExampleUpdated"

        typeName =
            "SelectedExample"

        type_ =
            Elm.Annotation.int
    in
    { type_ = type_
    , model =
        [ ( recordName, type_ )
        , ( recordName ++ "_menu", Elm.Annotation.bool )
        ]
    , init =
        [ ( recordName
          , Elm.int 0
          )
        , ( recordName ++ "_menu"
          , Elm.bool False
          )
        ]
    , onClick =
        \index ->
            Elm.apply
                (Elm.value
                    { importFrom = []
                    , name = msgName
                    , annotation = Just (Elm.Annotation.function [ Elm.Annotation.int ] appTypes.msg)
                    }
                )
                [ Elm.int index
                ]
    , toggleMenu =
        \current ->
            Elm.apply
                (Elm.value
                    { importFrom = []
                    , name = msgName ++ "_MenuUpdated"
                    , annotation = Just (Elm.Annotation.function [ Elm.Annotation.bool ] appTypes.msg)
                    }
                )
                [ Elm.apply
                    (Elm.val "not")
                    [ current ]
                ]
    , get =
        Elm.get recordName
    , getMenuOpen =
        Elm.get (recordName ++ "_menu")
    , msgs =
        [ Elm.variantWith msgName
            [ type_
            ]
        , Elm.variantWith (msgName ++ "_MenuUpdated")
            [ Elm.Annotation.bool
            ]
        ]
    , updateMenuBranch =
        \model ->
            Elm.Case.branch1 (msgName ++ "_MenuUpdated")
                ( "isOpen", type_ )
                (\isOpen ->
                    Elm.tuple
                        (model
                            |> Elm.updateRecord
                                [ ( recordName ++ "_menu", isOpen )
                                ]
                        )
                        Gen.App.Effect.none
                )
    , updateBranch =
        \model ->
            Elm.Case.branch1 msgName
                ( "newTab", type_ )
                (\tab ->
                    Elm.tuple
                        (model
                            |> Elm.updateRecord
                                [ ( recordName, tab )
                                ]
                        )
                        Gen.App.Effect.none
                )

    -- , view =
    --     Elm.Declare.fn "viewSelectedExample"
    --         ( "tab", Just type_ )
    --         (\tab ->
    --             Gen.Ui.row
    --                 [ Gen.Ui.spacing 24
    --                 , Gen.Ui.width Gen.Ui.fill
    --                 , Gen.Ui.padding 24
    --                 ]
    --                 (List.map
    --                     (\mod ->
    --                         Gen.Ui.el
    --                             [ Gen.Ui.padding 12
    --                             , Gen.Ui.Events.onClick
    --                                 (Elm.apply
    --                                     (Elm.value
    --                                         { importFrom = []
    --                                         , name = msgName
    --                                         , annotation = Just (Elm.Annotation.function [ type_ ] appTypes.msg)
    --                                         }
    --                                     )
    --                                     [ Elm.value
    --                                         { importFrom = []
    --                                         , name = moduleToTabName mod
    --                                         , annotation = Just type_
    --                                         }
    --                                     ]
    --                                 )
    --                             ]
    --                             (Gen.Ui.text mod.name)
    --                     )
    --                     modules
    --                 )
    --         )
    }


{-| -}
codeOrOutput top modules =
    let
        recordName =
            "focus_"

        msgName =
            "FocusUpdated"

        typeName =
            "Focus"

        type_ =
            Elm.Annotation.named [] typeName

        valueNamed name =
            Elm.value
                { importFrom = []
                , name = name
                , annotation = Just type_
                }

        variants =
            { output = "ShowOutput"
            , code = "ShowCode"
            }
    in
    { type_ = type_
    , model = ( recordName, type_ )
    , variants =
        variants
    , init =
        ( recordName
        , valueNamed "ShowOutput"
        )
    , msg =
        Elm.variantWith msgName
            [ Elm.Annotation.named [] typeName
            ]
    , updateBranch =
        \model ->
            Elm.Case.branch1 msgName
                ( "newTab", Elm.Annotation.named [] typeName )
                (\tab ->
                    Elm.tuple
                        (model
                            |> Elm.updateRecord
                                [ ( recordName, tab )
                                ]
                        )
                        Gen.App.Effect.none
                )
    , get =
        Elm.get recordName
    , viewCall =
        \model ->
            Elm.apply
                (Elm.value
                    { importFrom = []
                    , annotation = Nothing
                    , name = "viewCodeOrResult"
                    }
                )
                [ model |> Elm.get recordName
                ]
    , view =
        Elm.Declare.fn "viewCodeOrResult"
            ( "tab", Just type_ )
            (\tab ->
                Gen.Ui.row
                    [ Gen.Ui.spacing 8
                    , Gen.Ui.paddingXY 32 8
                    , Gen.Ui.alignRight
                    ]
                    (List.map
                        (\( label, varName ) ->
                            Gen.Ui.el
                                [ Gen.Ui.paddingXY 8 4
                                , Gen.Ui.border 1
                                , Gen.Ui.rounded 4
                                , Gen.Ui.pointer
                                , Gen.Ui.borderColor
                                    (Elm.ifThen (Elm.Op.equal tab (valueNamed varName))
                                        (Gen.Ui.rgb 1 1 1)
                                        (Gen.Ui.rgb 0 0 0)
                                    )
                                , Gen.Ui.Events.onClick
                                    (Elm.apply
                                        (Elm.value
                                            { importFrom = []
                                            , name = msgName
                                            , annotation = Just (Elm.Annotation.function [ type_ ] appTypes.msg)
                                            }
                                        )
                                        [ Elm.value
                                            { importFrom = []
                                            , name = varName
                                            , annotation = Just type_
                                            }
                                        ]
                                    )
                                ]
                                (Gen.Ui.text label)
                        )
                        [ ( "Output", "ShowOutput" )
                        , ( "Example", "ShowCode" )
                        ]
                    )
            )
    , toString =
        Elm.Declare.fn "tabToString"
            ( "tab", Just type_ )
            (\tab ->
                Elm.Case.custom tab
                    type_
                    (List.map
                        (\mod ->
                            Elm.Case.branch0 (moduleToTabName mod)
                                (Elm.string mod.name)
                        )
                        modules
                    )
            )
    , declaration =
        Elm.customType typeName
            [ Elm.variant "ShowCode"
            , Elm.variant "ShowOutput"
            ]
    }



{- END ADDITIONAL STUFF -}


generate : List String -> Module -> Elm.File
generate name mod =
    let
        example =
            selectedExample

        focus =
            codeOrOutput mod [ mod ]

        additional =
            { focus = focus
            , example = example
            }

        modelType =
            Elm.Annotation.record
                (focus.model
                    :: example.model
                    ++ toModuleFields mod
                )

        modelAlias =
            Elm.Annotation.alias []
                "Model"
                []
                modelType
    in
    Elm.file name
        (List.concat
            [ [ page
              , focus.declaration
              , Elm.alias "Model" modelType
              , init mod additional
              , Elm.customType "Msg"
                    (logMsg
                        :: focus.msg
                        :: example.msgs
                        ++ toMsgVariant mod
                    )
              , update modelAlias mod additional
              , focus.view.declaration
              , view modelAlias mod additional
              ]
            , renderViewer focus mod
            ]
        )


log : Elm.Expression
log =
    Elm.value
        { importFrom = []
        , name = "Log"
        , annotation = Nothing
        }


logMsg : Elm.Variant
logMsg =
    Elm.variant "Log"


update modelAlias mod additional =
    Elm.declaration "update"
        (Elm.fn2
            ( "msg", Just (Elm.Annotation.named [] "Msg") )
            ( "model", Just modelAlias )
            (\msg model ->
                Elm.Case.custom msg
                    (Elm.Annotation.named [] "Msg")
                    (logUpdate model
                        :: additional.focus.updateBranch model
                        :: additional.example.updateBranch model
                        :: additional.example.updateMenuBranch model
                        :: toMsgUpdate
                            model
                            mod
                    )
                    |> Elm.withType
                        (Elm.Annotation.tuple modelAlias
                            (Gen.App.Effect.annotation_.effect
                                (Elm.Annotation.named [] "Msg")
                            )
                        )
            )
        )


logUpdate model =
    Elm.Case.branch0
        "Log"
        (Elm.tuple
            model
            Gen.App.Effect.none
        )


moduleToTabName : Module -> String
moduleToTabName mod =
    mod.name |> String.replace "." ""


init mod additional =
    Elm.declaration "init"
        (Elm.fn3
            ( "params", Just (Elm.Annotation.var "params") )
            ( "shared", Just (Elm.Annotation.var "shared") )
            ( "maybeModel", Just (Elm.Annotation.maybe (Elm.Annotation.named [] "Model")) )
            (\params shared maybeModel ->
                Gen.App.Page.init
                    (Gen.Maybe.withDefault
                        (Elm.record
                            (additional.focus.init
                                :: additional.example.init
                                ++ toInitFields mod
                            )
                        )
                        maybeModel
                    )
                    |> Elm.withType
                        (Gen.App.Page.annotation_.init
                            (Elm.Annotation.named [] "Msg")
                            (Elm.Annotation.named [] "Model")
                        )
            )
        )


view modelAlias mod additional =
    Elm.declaration "view"
        (Elm.fn ( "model", Just modelAlias )
            (\model ->
                Elm.record
                    [ ( "title", Elm.string "Elm Interactive" )
                    , ( "body", viewBody model mod additional )
                    ]
                    |> Elm.withType (Gen.App.View.annotation_.view (Elm.Annotation.named [] "Msg"))
            )
        )


viewBody model mod additional =
    let
        examplePicker =
            Gen.Ui.el
                [ Gen.Ui.Font.size 24
                , Gen.Ui.paddingXY 32 10
                , Gen.Ui.pointer
                , Gen.Ui.Font.family
                    [ Gen.Ui.Font.typeface "Fira Code"
                    , Gen.Ui.Font.sansSerif
                    ]
                , Gen.Ui.Events.onClick
                    (additional.example.toggleMenu (additional.example.getMenuOpen model))
                , Elm.ifThen (additional.example.getMenuOpen model)
                    (Gen.Ui.below
                        (mod.examples
                            |> List.indexedMap
                                (\optionIndex option ->
                                    Gen.Ui.text option.name
                                        |> Gen.Ui.el
                                            [ Gen.Ui.Events.onClick
                                                (additional.example.onClick optionIndex)
                                            ]
                                )
                            |> Gen.Ui.column
                                [ Gen.Ui.padding 16
                                , Gen.Ui.move
                                    (Gen.Ui.right 32)
                                , Gen.Ui.border 1
                                , Gen.Ui.rounded 4
                                , Gen.Ui.background (Gen.Ui.rgb 0 0 0)
                                , Gen.Ui.spacing 8
                                ]
                        )
                    )
                    Gen.Ui.pointer
                ]
                (Gen.Ui.text
                    ("▶ " ++ mod.name)
                )
    in
    Gen.Ui.layout
        [ Gen.Ui.htmlAttribute (Gen.Html.Attributes.style "background" "rgb(36,36,36)")
        , Gen.Ui.Font.color (Gen.Ui.rgb 1 1 1)
        , Gen.Ui.inFront
            (additional.focus.viewCall model)
        , Gen.Ui.Font.family
            [ Gen.Ui.Font.typeface "Fira Code"
            , Gen.Ui.Font.sansSerif
            ]
        ]
        (Gen.Ui.column
            [ Gen.Ui.height Gen.Ui.fill
            , Gen.Ui.spacing 16
            ]
            (examplePicker
                :: List.indexedMap
                    (\index interact ->
                        Elm.ifThen (Elm.Op.equal (Elm.int index) (additional.example.get model))
                            (Gen.Ui.column
                                [ Gen.Ui.height Gen.Ui.fill
                                ]
                                [ Elm.apply
                                    (Elm.val ("view" ++ capitalize interact.name))
                                    [ model
                                    , Elm.get interact.name model
                                    ]
                                ]
                            )
                            Gen.Ui.none
                    )
                    mod.examples
            )
        )


renderViewer focus mod =
    List.map (renderInteractiveViewer focus) mod.examples


renderInteractiveViewer focus interact =
    Elm.declaration ("view" ++ capitalize interact.name)
        (Elm.fn2
            ( "parent", Just appTypes.model )
            ( "model", toModelField interact |> Tuple.second |> Just )
            (\model submodel ->
                interact.view
                    { model = submodel
                    , codeOrOutput =
                        Elm.Op.equal
                            (focus.get model)
                            (Elm.value
                                { importFrom = []
                                , name = focus.variants.output
                                , annotation = Just focus.type_
                                }
                            )
                    , onChange =
                        Elm.value
                            { importFrom = []
                            , name =
                                capitalize interact.name
                            , annotation = Nothing
                            }
                    }
                    |> Elm.withType
                        (Gen.Ui.annotation_.element
                            (Elm.Annotation.named [] "Msg")
                        )
            )
        )


toModuleFields : Module -> List ( String, Elm.Annotation.Annotation )
toModuleFields mod =
    List.map toModelField mod.examples


toModelField : Interactive -> ( String, Elm.Annotation.Annotation )
toModelField interact =
    ( interact.name
    , fieldsToAnnotation
        interact.fields
    )


fieldsToAnnotation : List Field -> Elm.Annotation.Annotation
fieldsToAnnotation fields =
    Elm.Annotation.record
        (List.map
            (\(Field name info) ->
                ( name, inputToAnnotation info.input )
            )
            fields
        )


toInitFields : Module -> List ( String, Elm.Expression )
toInitFields mod =
    List.map toInteractiveInitFields mod.examples


toInteractiveInitFields interact =
    ( interact.name
    , Elm.record
        (List.map
            (\(Field name info) ->
                ( name, info.init )
            )
            interact.fields
        )
    )


toMsgVariant : Module -> List Elm.Variant
toMsgVariant mod =
    List.map toMsgVariantInteractive mod.examples


toMsgVariantInteractive interact =
    Elm.variantWith interact.name
        [ fieldsToAnnotation
            interact.fields
        ]


toMsgUpdate : Elm.Expression -> Module -> List Elm.Case.Branch
toMsgUpdate model mod =
    List.map (toMsgUpdateInteractive model) mod.examples


toMsgUpdateInteractive : Elm.Expression -> Interactive -> Elm.Case.Branch
toMsgUpdateInteractive model interact =
    Elm.Case.branch1
        interact.name
        ( "updated"
        , fieldsToAnnotation
            interact.fields
        )
        (\updated ->
            Elm.tuple
                (model
                    |> Elm.updateRecord
                        [ ( interact.name, updated )
                        ]
                )
                Gen.App.Effect.none
        )


page : Elm.Declaration
page =
    Elm.record
        [ ( "init"
          , Elm.value
                { importFrom = []
                , name = "init"
                , annotation =
                    Just
                        (Elm.Annotation.function
                            [ Elm.Annotation.unit
                            ]
                            (Elm.Annotation.tuple
                                appTypes.model
                                (Gen.App.Effect.annotation_.effect (Elm.Annotation.var "msg"))
                            )
                        )
                }
          )
        , ( "update"
          , Elm.value
                { importFrom = []
                , name = "update"
                , annotation =
                    Just
                        (Elm.Annotation.function
                            [ appTypes.msg
                            , appTypes.model
                            ]
                            (Elm.Annotation.tuple
                                appTypes.model
                                (Gen.App.Effect.annotation_.effect (Elm.Annotation.var "msg"))
                            )
                        )
                }
          )
        , ( "view"
          , Elm.value
                { importFrom = []
                , name = "view"
                , annotation =
                    Nothing
                }
          )
        , ( "subscriptions"
          , Elm.fn
                ( "model", Nothing )
                (\_ ->
                    Gen.App.Sub.none
                )
          )
        ]
        |> Gen.App.Page.call_.page
        |> Elm.withType
            (Gen.App.Page.annotation_.page
                (Elm.Annotation.var "params")
                (Elm.Annotation.named [] "Model")
                (Elm.Annotation.named [] "Msg")
            )
        |> Elm.declaration "page"


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain
