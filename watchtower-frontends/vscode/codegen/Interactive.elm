module Interactive exposing
    ( Module, Interactive, generate
    , Field, field
    , log
    , Input(..), bool, string, int, float
    , details
    )

{-|

@docs Module, Interactive, generate

@docs Field, field

@docs log

@docs Input, bool, string, int, float

-}

import Elm
import Elm.Annotation
import Elm.Case
import Elm.Declare
import Elm.Docs
import Elm.Type
import Gen.Browser
import Gen.Element
import Gen.Element.Events
import Gen.Element.Font
import Gen.Html
import Gen.Html.Attributes
import Gen.Platform.Cmd
import Gen.Platform.Sub
import Gen.String


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
        { model : Elm.Expression
        , onChange : Elm.Expression
        }
        -> Elm.Expression
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


details :
    Field
    ->
        { label : String
        , key : String
        , input : Input
        , onChange : Input
        }
details (Field name opts) =
    { label = name
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


appTypes =
    { model = Elm.Annotation.named [] "Model"
    , msg = Elm.Annotation.named [] "Msg"
    }


selected top modules =
    let
        name =
            "Selected"

        type_ =
            Elm.Annotation.named [] name
    in
    { type_ = type_
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
                                            , name = "TabUpdated"
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
        Elm.customType name
            (List.map (Elm.variant << moduleToTabName) modules)
    , init =
        Elm.value
            { importFrom = []
            , name = moduleToTabName top
            , annotation = Just type_
            }
    }


generate : List String -> List Module -> Maybe Elm.File
generate name modules =
    case modules of
        [] ->
            Nothing

        top :: remain ->
            let
                tab =
                    selected top modules

                modelType =
                    Elm.Annotation.record
                        (( "selectedModule_", tab.type_ )
                            :: List.concatMap toModuleFields modules
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
                          , Elm.alias "Model" modelType
                          , init top modules tab
                          , Elm.customType "Msg"
                                (logMsg :: tabUpdateMsg :: List.concatMap toMsgVariant modules)
                          , update modelAlias modules
                          , tab.view.declaration
                          , view modelAlias modules tab
                          ]
                        , List.concatMap renderViewer modules
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


tabUpdateMsg : Elm.Variant
tabUpdateMsg =
    Elm.variantWith "TabUpdated"
        [ Elm.Annotation.named [] "Selected"
        ]


update modelAlias modules =
    Elm.declaration "update"
        (Elm.fn2
            ( "msg", Just (Elm.Annotation.named [] "Msg") )
            ( "model", Just modelAlias )
            (\msg model ->
                Elm.Case.custom msg
                    (Elm.Annotation.named [] "Msg")
                    (logUpdate model
                        :: tabUpdated model modules
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


tabUpdated model modules =
    Elm.Case.branch1 "TabUpdated"
        ( "newTab", Elm.Annotation.named [] "Selected" )
        (\tab ->
            Elm.tuple
                (model
                    |> Elm.updateRecord
                        [ ( "selectedModule_", tab )
                        ]
                )
                Gen.Platform.Cmd.none
        )


moduleToTabName : Module -> String
moduleToTabName mod =
    mod.name |> String.replace "." ""


init top modules tab =
    Elm.declaration "init"
        (Elm.fn
            ( "flags", Just Elm.Annotation.unit )
            (\model ->
                Elm.tuple
                    (Elm.record
                        (( "selectedModule_"
                         , tab.init
                         )
                            :: List.concatMap toInitFields modules
                        )
                        |> Elm.withType appTypes.model
                    )
                    Gen.Platform.Cmd.none
            )
        )


view modelAlias modules tab =
    Elm.declaration "view"
        (Elm.fn ( "model", Just modelAlias )
            (\model ->
                Gen.Element.layout
                    [ Gen.Element.htmlAttribute (Gen.Html.Attributes.style "background" "#242424")
                    , Gen.Element.htmlAttribute (Gen.Html.Attributes.style "color" "rgba(255, 255, 255, .87)")
                    , Gen.Element.Font.family
                        [ Gen.Element.Font.typeface "Fira Code"
                        , Gen.Element.Font.sansSerif
                        ]
                    ]
                    (Gen.Element.column
                        [ Gen.Element.spacing 24
                        , Gen.Element.width Gen.Element.fill
                        , Gen.Element.height Gen.Element.fill
                        ]
                        (List.concatMap
                            (\mod ->
                                -- view model applet
                                List.map
                                    (\interact ->
                                        Elm.apply
                                            (Elm.value
                                                { importFrom = []
                                                , annotation = Nothing
                                                , name = "view" ++ capitalize interact.name
                                                }
                                            )
                                            [ Elm.get interact.name model
                                            ]
                                    )
                                    mod.examples
                            )
                            modules
                        )
                    )
                    |> Elm.withType (Elm.Annotation.namedWith [ "Html" ] "Html" [ Elm.Annotation.named [] "Msg" ])
            )
        )


renderViewer mod =
    List.map renderInteractiveViewer mod.examples


renderInteractiveViewer interact =
    Elm.declaration ("view" ++ capitalize interact.name)
        (Elm.fn
            ( "model", toModelField interact |> Tuple.second |> Just )
            (\model ->
                interact.view
                    { model = model
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
    Gen.Browser.call_.element
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


capitalize : String -> String
capitalize str =
    let
        top =
            String.left 1 str

        remain =
            String.dropLeft 1 str
    in
    String.toUpper top ++ remain
