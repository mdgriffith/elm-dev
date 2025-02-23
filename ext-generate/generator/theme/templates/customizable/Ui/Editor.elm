module Ui.Editor exposing
    ( Control, bold, bulleted, control, h1, h2, h3, italic, code, numbered, paragraph
    , ControlBar, controls, noControls
    , fromMarkdown
    , update, Msg, Model
    , Event
    , view
    , toMarkdown
    , Style, StyleDetails, style, class
    )

{-|

@docs Control, bold, bulleted, control, h1, h2, h3, italic, code, numbered, paragraph

@docs ControlBar, controls, noControls

@docs fromMarkdown

@docs update, Msg, Model

@docs Event

@docs view

@docs toMarkdown


# Styling

@docs Style, StyleDetails, style, class

-}

import Dict
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown.Block as Block
import Markdown.Parser


column attrs children =
    Html.div attrs children


row attrs children =
    Html.div attrs children


none =
    Html.text ""


border =
    -- Ui.Theme.border.small
    Attr.style "border" "1px solid #e2e8f0"


borderRadius =
    -- Ui.Theme.borderRadius.medium
    Attr.style "border-radius" "4px"


type alias Element msg =
    Html.Html msg


h1 : List (Html.Attribute msg) -> Element msg -> Control msg
h1 =
    control (Heading 1)


h2 : List (Html.Attribute msg) -> Element msg -> Control msg
h2 =
    control (Heading 2)


h3 : List (Html.Attribute msg) -> Element msg -> Control msg
h3 =
    control (Heading 3)


paragraph : List (Html.Attribute msg) -> Element msg -> Control msg
paragraph =
    control Paragraph


bold : List (Html.Attribute msg) -> Element msg -> Control msg
bold =
    control Bold


code : List (Html.Attribute msg) -> Element msg -> Control msg
code =
    control Code


codeBlock : List (Html.Attribute msg) -> Element msg -> Control msg
codeBlock =
    control CodeBlock


italic : List (Html.Attribute msg) -> Element msg -> Control msg
italic =
    control Italic


bulleted : List (Html.Attribute msg) -> Element msg -> Control msg
bulleted =
    control Bulleted


numbered : List (Html.Attribute msg) -> Element msg -> Control msg
numbered =
    control Numbered


style :
    { header : StyleDetails -> Style
    , paragraph : StyleDetails -> Style
    , bold : StyleDetails -> Style
    , italic : StyleDetails -> Style
    , code : StyleDetails -> Style
    , bulleted : StyleDetails -> Style
    , numbered : StyleDetails -> Style
    }
style =
    { header = Style (Heading 1)
    , paragraph = Style Paragraph
    , bold = Style Bold
    , italic = Style Italic
    , code = Style Code
    , bulleted = Style Bulleted
    , numbered = Style Numbered
    }


class : String -> StyleDetails
class className =
    { class = className
    , style = []
    }


type Style
    = Style TipTapCommand StyleDetails


type alias StyleDetails =
    { class : String
    , style : List ( String, String )
    }


type TipTapCommand
    = Heading Int
    | Paragraph
    | Bold
    | Italic
    | Code
    | CodeBlock
    | Bulleted
    | Numbered


type ControlBar msg
    = ControlBar
        { elem : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
        , attrs : List (Html.Attribute msg)
        , controls : List (Control msg)
        }


noControls : ControlBar msg
noControls =
    ControlBar { attrs = [], controls = [] }


controls :
    (List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg)
    -> List (Html.Attribute msg)
    -> List (Control msg)
    -> ControlBar msg
controls elem attrs children =
    ControlBar
        { elem = elem
        , attrs = attrs
        , controls = children
        }


type Control msg
    = Control
        { command : TipTapCommand
        , attrs : List (Html.Attribute msg)
        , children : Element msg
        }


control :
    TipTapCommand
    -> List (Html.Attribute msg)
    -> Element msg
    -> Control msg
control command attrs element =
    Control
        { command = command
        , attrs = toAttr command :: attrs
        , children = element
        }


toAttr : TipTapCommand -> Html.Attribute msg
toAttr command =
    -- Ui.htmlAttribute <|
    case command of
        Heading level ->
            Attr.property "editorControl"
                (Encode.object
                    [ ( "cmd", Encode.string "toggle-heading" )
                    , ( "heading", Encode.int level )
                    ]
                )

        Paragraph ->
            Attr.property "editorControl"
                (Encode.object
                    [ ( "cmd", Encode.string "toggle-paragraph" )
                    ]
                )

        Bold ->
            Attr.property "editorControl"
                (Encode.object
                    [ ( "cmd", Encode.string "toggle-bold" )
                    ]
                )

        Italic ->
            Attr.property "editorControl"
                (Encode.object
                    [ ( "cmd", Encode.string "toggle-italic" )
                    ]
                )

        Code ->
            Attr.property "editorControl"
                (Encode.object
                    [ ( "cmd", Encode.string "toggle-code" )
                    ]
                )

        CodeBlock ->
            Attr.property "editorControl"
                (Encode.object
                    [ ( "cmd", Encode.string "toggle-codeblock" )
                    ]
                )

        Bulleted ->
            Attr.property "editorControl"
                (Encode.object
                    [ ( "cmd", Encode.string "toggle-bullet-list" )
                    ]
                )

        Numbered ->
            Attr.property "editorControl"
                (Encode.object
                    [ ( "cmd", Encode.string "toggle-numbered-list" )
                    ]
                )


fromMarkdown : String -> Model
fromMarkdown markdown =
    Model
        { initial = markdown
        , updated = Nothing
        }


type Msg
    = ContentUpdated State


update : Msg -> Model -> Model
update msg (Model details) =
    case msg of
        ContentUpdated state ->
            Model { details | updated = Just state }


type Model
    = Model ModelDetails


type alias ModelDetails =
    { initial : String
    , updated : Maybe State
    }


{-| All the events that can happen in the tiptap editor:

<https://tiptap.dev/docs/editor/api/events>

-}
type Event
    = Focus
    | Blur
    | SelectionUpdate
    | Destroy


{-| -}
type State
    = State Encode.Value


{-| -}
toMarkdown : Model -> String
toMarkdown (Model model) =
    case model.updated of
        Just (State state) ->
            case Decode.decodeValue decodeContent state of
                Ok node ->
                    nodeToMarkdown node

                Err _ ->
                    model.initial

        Nothing ->
            model.initial


type alias Content =
    String


view :
    (List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg)
    -> List (Html.Attribute msg)
    ->
        { text : Model
        , controls :
            { top : ControlBar msg
            , bottom : ControlBar msg
            , onSelection : ControlBar msg
            }
        , styles : List Style
        , editable : Bool
        , onEvent : Event -> Maybe msg
        , onUpdate : Msg -> msg
        }
    -> Element msg
view layout attrs model =
    layout
        (Attr.class "tiptap-editor-container" :: attrs)
        [ viewControlBar model.controls.top
        , Html.node "tiptap-editor"
            ([ Attr.property "content" (encodeModel model.text)
             , Attr.property "editable" (Encode.bool model.editable)
             , Attr.property "elementStyles" (Encode.dict identity encodeStyle (toStyleDict model.styles))
             ]
                ++ toEventHandlers model.onEvent model.onUpdate
            )
            []
        , viewControlBar model.controls.bottom
        ]


toStyleDict : List Style -> Dict.Dict String Style
toStyleDict styles =
    List.foldl
        (\((Style command details) as fullStyle) acc ->
            Dict.insert (commandToString command) fullStyle acc
        )
        Dict.empty
        styles


encodeStyle : Style -> Encode.Value
encodeStyle (Style command details) =
    Encode.object
        [ ( "command", Encode.string (commandToString command) )
        , ( "class", Encode.string details.class )
        , ( "style", Encode.string (List.foldl renderStyles "" details.style) )
        ]


renderStyles : ( String, String ) -> String -> String
renderStyles ( key, value ) acc =
    key ++ ":" ++ value ++ ";" ++ acc


commandToString : TipTapCommand -> String
commandToString command =
    case command of
        Heading _ ->
            "heading"

        Paragraph ->
            "paragraph"

        Bold ->
            "bold"

        Italic ->
            "italic"

        Code ->
            "code"

        CodeBlock ->
            "codeBlock"

        Bulleted ->
            "bulletList"

        Numbered ->
            "orderedList"


encodeModel : Model -> Encode.Value
encodeModel (Model details) =
    encodeMarkdown details.initial


viewControlBar : ControlBar msg -> Element msg
viewControlBar (ControlBar bar) =
    if List.isEmpty bar.controls && List.isEmpty bar.attrs then
        none

    else
        bar.elem bar.attrs
            (List.map
                (\(Control ctrl) ->
                    Html.node "tiptap-control" ctrl.attrs [ ctrl.children ]
                )
                bar.controls
            )


toEventHandlers : (Event -> Maybe msg) -> (Msg -> msg) -> List (Html.Attribute msg)
toEventHandlers onEvent onUpdate =
    let
        on eventName decoder =
            Event.on eventName
                (decoder
                    |> Decode.andThen
                        (\event ->
                            case onEvent event of
                                Just msg ->
                                    Decode.succeed msg

                                Nothing ->
                                    Decode.fail "Not listening to this event."
                        )
                )
    in
    [ on "editor-focused" (Decode.succeed Focus)
    , on "editor-blurred" (Decode.succeed Blur)
    , Event.on "editor-updated" (Decode.map (onUpdate << ContentUpdated) decodeState)
    , on "editor-selection-updated" (Decode.succeed SelectionUpdate)
    , on "destroy" (Decode.succeed Destroy)
    ]


decodeState : Decode.Decoder State
decodeState =
    Decode.field "json" Decode.value
        |> Decode.map State


decodeContent : Decode.Decoder Node
decodeContent =
    Decode.field "detail" decodeNode


{-|

    Values

        type: String (paragraph, heading, text, doc)
        attrs: List Attr
        marks: List Mark

        -- one or the other
        content: List Node
        text: String


    Mark: a list of styles.  Curious if these are always a type: value pair, because this could be a set instead of a list
        [{type: bold}, {type: italic}]

-}
decodeNode : Decode.Decoder Node
decodeNode =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\tipe ->
                case tipe of
                    "text" ->
                        Decode.map Text <|
                            Decode.map2 TextDetails
                                (Decode.field "text" Decode.string)
                                (Decode.field "marks" (Decode.list decodeMark)
                                    |> Decode.maybe
                                    |> Decode.map (Maybe.withDefault [])
                                )

                    _ ->
                        Decode.map Node <|
                            Decode.map2 (NodeDetails tipe)
                                (Decode.field "attrs" (Decode.dict decodeValue)
                                    |> Decode.maybe
                                    |> Decode.map (Maybe.withDefault Dict.empty)
                                )
                                (Decode.field "content" (Decode.list (Decode.lazy (\_ -> decodeNode))))
            )


decodeMark : Decode.Decoder Mark
decodeMark =
    Decode.field "type" Decode.string
        |> Decode.map Mark


decodeValue : Decode.Decoder Val
decodeValue =
    Decode.oneOf
        [ Decode.map Str Decode.string
        , Decode.map Integer Decode.int
        ]


type Node
    = Node NodeDetails
    | Text TextDetails


type alias TextDetails =
    { text : String
    , marks : List Mark
    }


type alias Mark =
    { type_ : String
    }


type alias NodeDetails =
    { type_ : String
    , attrs : Dict.Dict String Val
    , content : List Node
    }


type Val
    = Str String
    | Integer Int


encodeMarkdown : String -> Encode.Value
encodeMarkdown markdown =
    case Markdown.Parser.parse markdown of
        Err err ->
            Encode.string markdown

        Ok blockList ->
            Encode.object
                [ ( "type", Encode.string "doc" )
                , ( "content", Encode.list encodeBlock blockList )
                ]


encodeBlock : Block.Block -> Encode.Value
encodeBlock block =
    case block of
        Block.Heading headingLevel content ->
            Encode.object
                [ ( "type", Encode.string "heading" )
                , ( "attrs", Encode.object [ ( "level", Encode.int (Block.headingLevelToInt headingLevel) ) ] )
                , ( "content", Encode.list identity (List.concatMap encodeInline content) )
                ]

        Block.Paragraph inlines ->
            Encode.object
                [ ( "type", Encode.string "paragraph" )
                , ( "content", Encode.list identity (List.concatMap encodeInline inlines) )
                ]

        Block.HtmlBlock html ->
            Encode.string ""

        Block.UnorderedList tight items ->
            Encode.object
                [ ( "type", Encode.string "bulletList" )
                , ( "content"
                  , Encode.list
                        (\(Block.ListItem task childrenBlocks) ->
                            Encode.object
                                [ ( "type", Encode.string "listItem" )
                                , ( "content"
                                  , Encode.list encodeBlock childrenBlocks
                                  )
                                ]
                        )
                        items
                  )
                ]

        Block.OrderedList tight startingIndex listListItems ->
            Encode.object
                [ ( "type", Encode.string "orderedList" )
                , ( "attrs", Encode.object [ ( "start", Encode.int startingIndex ) ] )
                , ( "content"
                  , Encode.list
                        (\childrenBlocks ->
                            Encode.object
                                [ ( "type", Encode.string "listItem" )
                                , ( "content"
                                  , Encode.list encodeBlock childrenBlocks
                                  )
                                ]
                        )
                        listListItems
                  )
                ]

        Block.CodeBlock { body, language } ->
            Encode.object
                [ ( "type", Encode.string "codeBlock" )
                , ( "content", encodeText [] body )
                ]

        Block.ThematicBreak ->
            Encode.string ""

        Block.BlockQuote nestedBlocks ->
            Encode.string ""

        Block.Table headers rows ->
            Encode.string ""


encodeMark : Mark -> Encode.Value
encodeMark mark =
    Encode.object
        [ ( "type", Encode.string mark.type_ ) ]


encodeInline : Block.Inline -> List Encode.Value
encodeInline inline =
    encodeInlineHelper [] inline


encodeInlineHelper : List Mark -> Block.Inline -> List Encode.Value
encodeInlineHelper marks inline =
    case inline of
        Block.Strong inner ->
            List.concatMap (encodeInlineHelper ({ type_ = "bold" } :: marks)) inner

        Block.Emphasis inner ->
            List.concatMap (encodeInlineHelper ({ type_ = "italic" } :: marks)) inner

        Block.Strikethrough inner ->
            List.concatMap (encodeInlineHelper ({ type_ = "strike" } :: marks)) inner

        Block.Image src title children ->
            []

        Block.Text string ->
            [ encodeText marks string ]

        Block.CodeSpan string ->
            [ encodeText ({ type_ = "code" } :: marks) string ]

        Block.Link destination title inlines ->
            []

        Block.HardLineBreak ->
            []

        Block.HtmlInline html ->
            []


encodeText : List Mark -> String -> Encode.Value
encodeText marks text =
    case marks of
        [] ->
            Encode.object
                [ ( "type", Encode.string "text" )
                , ( "text", Encode.string text )
                ]

        _ ->
            Encode.object
                [ ( "type", Encode.string "text" )
                , ( "text", Encode.string text )
                , ( "marks", Encode.list encodeMark marks )
                ]


nodeToMarkdown : Node -> String
nodeToMarkdown node =
    case node of
        Text details ->
            case details.marks of
                [] ->
                    details.text

                _ ->
                    wrapMark details.marks details.text

        Node details ->
            let
                content =
                    List.map nodeToMarkdown details.content
                        |> String.join ""
            in
            case details.type_ of
                "paragraph" ->
                    content ++ "\n"

                "heading" ->
                    case Dict.get "level" details.attrs of
                        Just (Integer level) ->
                            String.repeat level "#" ++ " " ++ content ++ "\n"

                        _ ->
                            content

                "text" ->
                    content

                _ ->
                    content


wrapMark : List Mark -> String -> String
wrapMark marks text =
    List.foldl
        (\mark txt ->
            case mark.type_ of
                "bold" ->
                    "**" ++ txt ++ "**"

                "italic" ->
                    "*" ++ txt ++ "*"

                "strike" ->
                    "~" ++ txt ++ "~"

                "code" ->
                    "`" ++ txt ++ "`"

                _ ->
                    txt
        )
        text
        marks


getInt : String -> List ( String, String ) -> Maybe Int
getInt key attrs =
    case List.filter (\( k, _ ) -> k == key) attrs of
        [ ( _, value ) ] ->
            String.toInt value

        _ ->
            Nothing
