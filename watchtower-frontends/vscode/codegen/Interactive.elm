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
import Gen.App
import Gen.Element
import Gen.Element.Background
import Gen.Element.Border
import Gen.Element.Events
import Gen.Element.Font
import Gen.Html
import Gen.Html.Attributes
import Gen.Platform.Cmd
import Gen.Platform.Sub
import Gen.String
import Gen.Ui


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
                        Gen.Platform.Cmd.none
                )
    , view =
        Elm.Declare.fn "viewTab"
            ( "tab", Just type_ )
            (\tab ->
                Gen.Element.row
                    [ Gen.Element.spacing 24
                    , Gen.Element.width Gen.Element.fill
                    , Gen.Element.padding 24
                    ]
                    (List.map
                        (\mod ->
                            Gen.Element.el
                                [ Gen.Element.padding 12
                                , Gen.Element.Events.onClick
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
                                (Gen.Element.text mod.name)
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
                        Gen.Platform.Cmd.none
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
                        Gen.Platform.Cmd.none
                )

    -- , view =
    --     Elm.Declare.fn "viewSelectedExample"
    --         ( "tab", Just type_ )
    --         (\tab ->
    --             Gen.Element.row
    --                 [ Gen.Element.spacing 24
    --                 , Gen.Element.width Gen.Element.fill
    --                 , Gen.Element.padding 24
    --                 ]
    --                 (List.map
    --                     (\mod ->
    --                         Gen.Element.el
    --                             [ Gen.Element.padding 12
    --                             , Gen.Element.Events.onClick
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
    --                             (Gen.Element.text mod.name)
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
                        Gen.Platform.Cmd.none
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
                Gen.Element.row
                    [ Gen.Element.spacing 8
                    , Gen.Element.paddingXY 32 8
                    , Gen.Element.alignRight
                    ]
                    (List.map
                        (\( label, varName ) ->
                            Gen.Element.el
                                [ Gen.Element.paddingXY 8 4
                                , Gen.Element.Border.width 1
                                , Gen.Element.Border.rounded 4
                                , Gen.Ui.pointer
                                , Gen.Element.Border.color
                                    (Elm.ifThen (Elm.Op.equal tab (valueNamed varName))
                                        (Gen.Element.rgb 1 1 1)
                                        (Gen.Element.rgb 0 0 0)
                                    )
                                , Gen.Element.Events.onClick
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
                                (Gen.Element.text label)
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


generate : List String -> List Module -> Maybe Elm.File
generate name modules =
    case modules of
        [] ->
            Nothing

        top :: remain ->
            let
                tab =
                    selected top modules

                example =
                    selectedExample

                focus =
                    codeOrOutput top modules

                additional =
                    { tab = tab
                    , focus = focus
                    , example = example
                    }

                modelType =
                    Elm.Annotation.record
                        (tab.model
                            :: focus.model
                            :: example.model
                            ++ List.concatMap toModuleFields modules
                        )

                modelAlias =
                    Elm.Annotation.alias []
                        "Model"
                        []
                        modelType
            in
            Just <|
                Elm.file name
                    (List.concat
                        [ [ Elm.declaration "main" callMain
                                |> Elm.expose
                          , tab.declaration
                          , tab.toString.declaration
                          , focus.declaration
                          , Elm.alias "Model" modelType
                          , init top modules additional
                          , Elm.customType "Msg"
                                (logMsg
                                    :: focus.msg
                                    :: tab.msg
                                    :: example.msgs
                                    ++ List.concatMap toMsgVariant modules
                                )
                          , update modelAlias modules additional
                          , tab.view.declaration
                          , focus.view.declaration
                          , view modelAlias modules additional
                          ]
                        , List.concatMap (renderViewer focus) modules
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


update modelAlias modules additional =
    Elm.declaration "update"
        (Elm.fn2
            ( "msg", Just (Elm.Annotation.named [] "Msg") )
            ( "model", Just modelAlias )
            (\msg model ->
                Elm.Case.custom msg
                    (Elm.Annotation.named [] "Msg")
                    (logUpdate model
                        :: additional.tab.updateBranch model
                        :: additional.focus.updateBranch model
                        :: additional.example.updateBranch model
                        :: additional.example.updateMenuBranch model
                        :: List.concatMap
                            (toMsgUpdate
                                model
                            )
                            modules
                    )
            )
        )


logUpdate model =
    Elm.Case.branch0
        "Log"
        (Elm.tuple
            model
            Gen.Platform.Cmd.none
        )


moduleToTabName : Module -> String
moduleToTabName mod =
    mod.name |> String.replace "." ""


init top modules additional =
    Elm.declaration "init"
        (Elm.fn
            ( "flags", Just Elm.Annotation.unit )
            (\model ->
                Elm.tuple
                    (Elm.record
                        (additional.tab.init
                            :: additional.focus.init
                            :: additional.example.init
                            ++ List.concatMap toInitFields modules
                        )
                        |> Elm.withType appTypes.model
                    )
                    Gen.Platform.Cmd.none
            )
        )


view modelAlias modules additional =
    Elm.declaration "view"
        (Elm.fn ( "model", Just modelAlias )
            (\model ->
                Gen.Element.layout
                    [ --     Gen.Element.htmlAttribute
                      --     (Gen.Html.Attributes.style "background" "white")
                      -- , Gen.Element.htmlAttribute
                      --     (Gen.Html.Attributes.style "color" "rgba(0, 0, 0, .87)")
                      Gen.Element.htmlAttribute (Gen.Html.Attributes.style "background" "rgb(36,36,36)")
                    , Gen.Element.Font.color (Gen.Element.rgb 1 1 1)
                    , Gen.Element.inFront
                        (additional.focus.viewCall model)
                    , Gen.Element.Font.family
                        [ Gen.Element.Font.typeface "Fira Code"
                        , Gen.Element.Font.sansSerif
                        ]
                    ]
                    (Gen.Element.column
                        [ Gen.Element.width Gen.Element.fill
                        , Gen.Element.height Gen.Element.fill
                        , Gen.Element.spacing 16
                        ]
                        (modules
                            |> List.concatMap
                                (\mod ->
                                    -- view model applet
                                    List.indexedMap
                                        (\index interact ->
                                            Elm.ifThen (Elm.Op.equal (Elm.int index) (additional.example.get model))
                                                (Gen.Element.column
                                                    [ Gen.Element.width Gen.Element.fill
                                                    , Gen.Element.height Gen.Element.fill
                                                    ]
                                                    [ Gen.Element.el
                                                        [ Gen.Element.Font.size 24
                                                        , Gen.Element.paddingXY 32 10
                                                        , Gen.Ui.pointer
                                                        , Gen.Element.Font.family
                                                            [ Gen.Element.Font.typeface "Fira Code"
                                                            , Gen.Element.Font.sansSerif
                                                            ]
                                                        , Gen.Element.Events.onClick
                                                            (additional.example.toggleMenu (additional.example.getMenuOpen model))
                                                        , Elm.ifThen (additional.example.getMenuOpen model)
                                                            (Gen.Element.below
                                                                (mod.examples
                                                                    |> List.indexedMap
                                                                        (\optionIndex option ->
                                                                            Gen.Element.text option.name
                                                                                |> Gen.Element.el
                                                                                    [ Gen.Element.Events.onClick
                                                                                        (additional.example.onClick optionIndex)
                                                                                    ]
                                                                        )
                                                                    |> Gen.Element.column
                                                                        [ Gen.Element.padding 16
                                                                        , Gen.Element.moveRight 32
                                                                        , Gen.Element.Border.width 1
                                                                        , Gen.Element.Border.rounded 4
                                                                        , Gen.Element.Background.color (Gen.Element.rgb 0 0 0)
                                                                        , Gen.Element.spacing 8
                                                                        ]
                                                                )
                                                            )
                                                            Gen.Ui.pointer
                                                        ]
                                                        (Gen.Element.text
                                                            -- (modul.name ++ "." ++ targeting.start.name)
                                                            ("▶ " ++ interact.name)
                                                        )
                                                    , Elm.apply
                                                        (Elm.value
                                                            { importFrom = []
                                                            , annotation = Nothing
                                                            , name = "view" ++ capitalize interact.name
                                                            }
                                                        )
                                                        [ model
                                                        , Elm.get interact.name model
                                                        ]
                                                    ]
                                                )
                                                Gen.Element.none
                                        )
                                        mod.examples
                                )
                        )
                    )
                    |> Elm.withType (Elm.Annotation.namedWith [ "Html" ] "Html" [ Elm.Annotation.named [] "Msg" ])
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
                        (Elm.Annotation.namedWith [ "Element" ] "Element" [ Elm.Annotation.named [] "Msg" ])
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
                Gen.Platform.Cmd.none
        )


callMain : Elm.Expression
callMain =
    Gen.App.call_.element
        (Elm.record
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
                                    (Elm.Annotation.cmd (Elm.Annotation.var "msg"))
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
                                    (Elm.Annotation.cmd (Elm.Annotation.var "msg"))
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
                        Gen.Platform.Sub.none
                    )
              )
            ]
        )
        |> Elm.withType
            (Gen.App.annotation_.prog
                Elm.Annotation.unit
                (Elm.Annotation.named [] "Model")
                (Elm.Annotation.named [] "Msg")
            )


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain
