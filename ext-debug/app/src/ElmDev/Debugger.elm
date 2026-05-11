port module ElmDev.Debugger exposing (main)

import Browser
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (disabled, style, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Set exposing (Set)


port fromRuntime : (Decode.Value -> msg) -> Sub msg


port toRuntime : Encode.Value -> Cmd msg


defaultOpenDepth : Int
defaultOpenDepth =
    2


type alias Model =
    { events : List Event
    , selected : Maybe Int
    , open : Bool
    , viewMode : ViewMode
    , modelView : ModelView
    , lazyStats : List LazyStat
    , collapsed : Set String
    , expanded : Set String
    , messageDrawers : Set Int
    }


type ViewMode
    = TimelineView
    | LazyView


type ModelView
    = ModelBefore
    | ModelAfter
    | ModelDelta


type Event
    = Init InitEvent
    | Update UpdateEvent
    | Frame FrameEvent
    | Unknown Decode.Value


type alias InitEvent =
    { id : Int
    , model : DebugValue
    }


type alias UpdateEvent =
    { id : Int
    , message : String
    , rawMessage : DebugValue
    , modelBefore : DebugValue
    , modelAfter : DebugValue
    , duration : Float
    }


type alias FrameEvent =
    { id : Int
    , duration : Float
    , renderDuration : Float
    }


type RuntimeMessage
    = RuntimeDebuggerEvent Event
    | RuntimePerformance PerformanceSnapshot


type alias PerformanceSnapshot =
    { lazy : List LazyStat
    }


type alias LazyStat =
    { id : Int
    , name : String
    , arity : Int
    , calls : Int
    , renders : Int
    , hits : Int
    , avgRender : Float
    , maxRender : Float
    }


type DebugValue
    = BoolValue Bool
    | NumberValue String
    | StringValue String
    | CharValue String
    | RecordValue (List Field)
    | ConstructorValue (Maybe String) (List DebugValue)
    | ListValue (List DebugValue)
    | ArrayValue (List DebugValue)
    | SetValue (List DebugValue)
    | DictValue (List DictEntry)
    | OpaqueValue String


type alias Field =
    { name : String
    , value : DebugValue
    }


type alias DictEntry =
    { key : DebugValue
    , value : DebugValue
    }


type Msg
    = RuntimeEvent Decode.Value
    | Select Int
    | Scrub String
    | SelectView ViewMode
    | SelectModelView ModelView
    | ToggleOpen
    | ToggleCollapse String Bool
    | ToggleMessagePayload Int
    | CopyToClipboard String


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { events = [], selected = Nothing, open = False, viewMode = TimelineView, modelView = ModelDelta, lazyStats = [], collapsed = Set.empty, expanded = Set.empty, messageDrawers = Set.empty }, setOpen False )
        , update = update
        , subscriptions = \_ -> fromRuntime RuntimeEvent
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RuntimeEvent value ->
            case Decode.decodeValue runtimeMessageDecoder value of
                Ok (RuntimeDebuggerEvent event) ->
                    ( { model
                        | events = model.events ++ [ event ]
                        , selected = nextSelected model.selected event
                      }
                    , Cmd.none
                    )

                Ok (RuntimePerformance snapshot) ->
                    ( { model | lazyStats = snapshot.lazy }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | events = model.events ++ [ Unknown value ] }
                    , Cmd.none
                    )

        Select id ->
            ( { model | selected = Just id }, Cmd.none )

        Scrub raw ->
            ( { model | selected = String.toInt raw }, Cmd.none )

        SelectView viewMode ->
            ( { model | viewMode = viewMode }, Cmd.none )

        SelectModelView modelView ->
            ( { model | modelView = modelView }, Cmd.none )

        ToggleOpen ->
            let
                nextOpen =
                    not model.open
            in
            ( { model | open = nextOpen }, setOpen nextOpen )

        ToggleCollapse path isCollapsed ->
            if isCollapsed then
                ( { model
                    | expanded = Set.insert path model.expanded
                    , collapsed = Set.remove path model.collapsed
                  }
                , Cmd.none
                )

            else
                ( { model
                    | collapsed = Set.insert path model.collapsed
                    , expanded = Set.remove path model.expanded
                  }
                , Cmd.none
                )

        ToggleMessagePayload id ->
            if Set.member id model.messageDrawers then
                ( { model | messageDrawers = Set.remove id model.messageDrawers }, Cmd.none )

            else
                ( { model | messageDrawers = Set.insert id model.messageDrawers }, Cmd.none )

        CopyToClipboard string ->
            ( model, copyText string )


nextSelected : Maybe Int -> Event -> Maybe Int
nextSelected current event =
    case event of
        Init initEvent ->
            Just initEvent.id

        Update updateEvent ->
            Just updateEvent.id

        Frame _ ->
            current

        Unknown _ ->
            current


setOpen : Bool -> Cmd Msg
setOpen open =
    toRuntime <|
        Encode.object
            [ ( "type", Encode.string "setOpen" )
            , ( "open", Encode.bool open )
            ]


copyText : String -> Cmd Msg
copyText string =
    toRuntime <|
        Encode.object
            [ ( "type", Encode.string "copyText" )
            , ( "text", Encode.string string )
            ]


view : Model -> Html Msg
view model =
    if model.open then
        div shellStyles
            [ viewTimeline model
            , case model.viewMode of
                TimelineView ->
                    viewSelected model

                LazyView ->
                    viewLazyStats model.lazyStats
            ]

    else
        button minimizedStyles
            [ text ("Elm Dev (" ++ String.fromInt (List.length (selectableEvents model.events)) ++ ")") ]


shellStyles : List (Html.Attribute Msg)
shellStyles =
    [ style "font-family" "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace"
    , style "width" "100%"
    , style "height" "100%"
    , style "display" "flex"
    , style "background" "linear-gradient(135deg, rgba(26, 20, 10, 0.98), rgba(8, 9, 10, 0.98))"
    , style "color" "#ffd77a"
    , style "box-sizing" "border-box"
    , style "overflow" "hidden"
    , style "border" "1px solid #f6a400"
    , style "border-radius" "14px"
    , style "box-shadow" "inset 0 0 0 1px rgba(255, 188, 46, .16), 0 0 36px rgba(246, 164, 0, .18)"
    ]


minimizedStyles : List (Html.Attribute Msg)
minimizedStyles =
    [ onClick ToggleOpen
    , style "width" "100%"
    , style "height" "100%"
    , style "border" "1px solid #f6a400"
    , style "border-radius" "999px"
    , style "background" "linear-gradient(90deg, #261700, #f6a400)"
    , style "color" "#140c00"
    , style "font" "800 13px ui-monospace, monospace"
    , style "cursor" "pointer"
    , style "box-shadow" "0 8px 24px rgba(0,0,0,.35), 0 0 18px rgba(246,164,0,.35)"
    , style "letter-spacing" "0.06em"
    ]


viewTimeline : Model -> Html Msg
viewTimeline model =
    let
        maxId =
            selectableEvents model.events
                |> List.map eventId
                |> List.maximum
                |> Maybe.withDefault 0

        selected =
            Maybe.withDefault maxId model.selected
    in
    div [ style "width" "340px", style "border-right" "1px solid rgba(246, 164, 0, .45)", style "display" "flex", style "flex-direction" "column", style "background" "rgba(13, 10, 5, .74)" ]
        [ div [ style "padding" "12px 14px", style "border-bottom" "1px solid rgba(246, 164, 0, .45)", style "background" "linear-gradient(90deg, rgba(246,164,0,.24), rgba(246,164,0,.04))", style "font-weight" "800", style "letter-spacing" "0.08em", style "display" "flex", style "align-items" "center", style "justify-content" "space-between", style "text-transform" "uppercase" ]
            [ text "Elm Dev Debugger"
            , button [ onClick ToggleOpen, style "background" "rgba(246,164,0,.12)", style "color" "#ffd77a", style "border" "1px solid rgba(246,164,0,.7)", style "border-radius" "999px", style "font" "inherit", style "cursor" "pointer", style "padding" "3px 10px" ] [ text "Minimize" ]
            ]
        , viewModeTabs model.viewMode model.lazyStats
        , div [ style "padding" "12px", style "border-bottom" "1px solid rgba(246, 164, 0, .28)" ]
            [ input [ type_ "range", Html.Attributes.min "0", Html.Attributes.max (String.fromInt maxId), value (String.fromInt selected), onInput Scrub, style "width" "100%", style "accent-color" "#f6a400" ] [] ]
        , div [ style "overflow" "auto", style "padding" "6px 0" ] (List.map (viewEventSummary selected) (List.reverse model.events))
        ]


viewModeTabs : ViewMode -> List LazyStat -> Html Msg
viewModeTabs viewMode lazyStats =
    div [ style "display" "grid", style "grid-template-columns" "1fr 1fr", style "gap" "8px", style "padding" "10px 12px", style "border-bottom" "1px solid rgba(246, 164, 0, .28)" ]
        [ viewModeButton viewMode TimelineView "Timeline"
        , viewModeButton viewMode LazyView ("Lazy (" ++ String.fromInt (List.length lazyStats) ++ ")")
        ]


viewModeButton : ViewMode -> ViewMode -> String -> Html Msg
viewModeButton current target label =
    let
        selected =
            current == target
    in
    button
        [ onClick (SelectView target)
        , style "background"
            (if selected then
                "rgba(246,164,0,.24)"

             else
                "rgba(246,164,0,.06)"
            )
        , style "border"
            (if selected then
                "1px solid rgba(246,164,0,.75)"

             else
                "1px solid rgba(246,164,0,.24)"
            )
        , style "border-radius" "8px"
        , style "color" "#ffd77a"
        , style "font" "inherit"
        , style "font-weight" "800"
        , style "cursor" "pointer"
        , style "padding" "7px 8px"
        ]
        [ text label ]


viewEventSummary : Int -> Event -> Html Msg
viewEventSummary selected event =
    case event of
        Frame frameEvent ->
            div [ style "padding" "9px 12px", style "color" "#9f7a31", style "font-size" "11px", style "letter-spacing" "0.12em", style "text-transform" "uppercase", style "border-top" "1px solid rgba(246,164,0,.18)", style "border-bottom" "1px solid rgba(246,164,0,.18)", style "display" "grid", style "grid-template-columns" "1fr max-content", style "column-gap" "16px", style "align-items" "center" ]
                [ span [] [ text "Animation Frame" ]
                , span [ style "text-align" "right" ] [ text ("update " ++ formatMs frameEvent.duration ++ " / render " ++ formatMs frameEvent.renderDuration) ]
                ]

        _ ->
            let
                id =
                    eventId event

                bg =
                    if selected == id then
                        "rgba(246,164,0,.22)"

                    else
                        "transparent"
            in
            button
                [ onClick (Select id)
                , disabled (id < 0)
                , style "display" "block"
                , style "width" "100%"
                , style "text-align" "left"
                , style "font" "inherit"
                , style "color" "inherit"
                , style "background" bg
                , style "border" "0"
                , style "border-left"
                    (if selected == id then
                        "3px solid #f6a400"

                     else
                        "3px solid transparent"
                    )
                , style "padding" "9px 12px"
                , style "cursor" "pointer"
                ]
                [ span [ style "color" "#9f7a31" ] [ text (String.fromInt id ++ "  ") ]
                , text (eventLabel event)
                ]


viewSelected : Model -> Html Msg
viewSelected model =
    case model.selected |> Maybe.andThen (eventById model.events) of
        Nothing ->
            div [ style "padding" "24px", style "color" "#9f7a31" ] [ text "Waiting for debugger events..." ]

        Just event ->
            div [ style "overflow" "auto", style "flex" "1", style "min-width" "0" ]
                [ viewEventDetails model event ]


viewLazyStats : List LazyStat -> Html Msg
viewLazyStats stats =
    div [ style "overflow" "auto", style "flex" "1", style "min-width" "0", style "padding" "22px" ]
        [ div [ style "display" "flex", style "align-items" "baseline", style "justify-content" "space-between", style "gap" "20px", style "margin-bottom" "18px" ]
            [ div []
                [ div [ style "font-size" "11px", style "text-transform" "uppercase", style "letter-spacing" "0.16em", style "color" "#9f7a31" ] [ text "live performance" ]
                , div [ style "font-size" "24px", style "font-weight" "900", style "color" "#ffd77a" ] [ text "Lazy calls" ]
                ]
            , div [ style "color" "#9f7a31", style "text-align" "right" ] [ text "Rendered means the lazy thunk actually ran. Avoided renders reused the previous lazy subtree." ]
            ]
        , if List.isEmpty stats then
            div [ style "padding" "24px", style "border" "1px dashed rgba(246,164,0,.35)", style "border-radius" "12px", style "color" "#9f7a31" ]
                [ text "No Html.Lazy calls observed yet." ]

          else
            div [ style "display" "grid", style "gap" "10px" ]
                (List.map viewLazyStat (rankLazyStats stats))
        ]


viewLazyStat : LazyStat -> Html Msg
viewLazyStat stat =
    let
        renderRate =
            rate stat.renders stat.calls

        hitRate =
            rate stat.hits stat.calls
    in
    div [ style "border" "1px solid rgba(246,164,0,.28)", style "border-radius" "12px", style "background" "rgba(13,10,5,.52)", style "overflow" "hidden" ]
        [ div [ style "padding" "12px 14px", style "display" "grid", style "grid-template-columns" "1fr max-content", style "gap" "16px", style "align-items" "baseline", style "background" "rgba(246,164,0,.08)" ]
            [ div []
                [ div [ style "font-size" "17px", style "font-weight" "900", style "color" "#ffd77a", style "overflow-wrap" "anywhere" ] [ text stat.name ]
                , div [ style "color" "#9f7a31", style "font-size" "12px", style "margin-top" "3px" ] [ text ("lazy" ++ String.fromInt stat.arity ++ " #" ++ String.fromInt stat.id) ]
                ]
            , div [ style "color" "#f6a400", style "font-weight" "900" ] [ text (lazyDiagnosis stat) ]
            ]
        , div [ style "display" "grid", style "grid-template-columns" "repeat(6, minmax(90px, 1fr))", style "gap" "1px", style "background" "rgba(246,164,0,.16)" ]
            [ viewMetric "Calls" (String.fromInt stat.calls)
            , viewMetric "Rendered" (String.fromInt stat.renders ++ " / " ++ formatPercent renderRate)
            , viewMetric "Avoided renders" (String.fromInt stat.hits ++ " / " ++ formatPercent hitRate)
            , viewMetric "Avg render" (formatMs stat.avgRender)
            , viewMetric "Max render" (formatMs stat.maxRender)
            , viewMetric "Signal" (lazySignal stat)
            ]
        ]


viewMetric : String -> String -> Html Msg
viewMetric label value_ =
    div [ style "background" "rgba(8,9,10,.86)", style "padding" "10px 12px", style "min-width" "0" ]
        [ div [ style "font-size" "10px", style "text-transform" "uppercase", style "letter-spacing" "0.14em", style "color" "#9f7a31", style "margin-bottom" "4px" ] [ text label ]
        , div [ style "font-size" "14px", style "font-weight" "900", style "color" "#ffe6aa", style "overflow-wrap" "anywhere" ] [ text value_ ]
        ]


rankLazyStats : List LazyStat -> List LazyStat
rankLazyStats stats =
    List.sortWith compareLazyStats stats


compareLazyStats : LazyStat -> LazyStat -> Order
compareLazyStats left right =
    compare (lazyScore right) (lazyScore left)


lazyScore : LazyStat -> Float
lazyScore stat =
    (rate stat.renders stat.calls * 100) + stat.maxRender + (stat.avgRender * 4)


lazyDiagnosis : LazyStat -> String
lazyDiagnosis stat =
    let
        renderRate =
            rate stat.renders stat.calls
    in
    if stat.calls < 5 then
        "warming up"

    else if renderRate >= 0.95 && stat.avgRender < 1 then
        "likely unnecessary"

    else if renderRate >= 0.95 then
        "always rendering"

    else if renderRate <= 0.25 && stat.avgRender >= 1 then
        "helping"

    else if renderRate <= 0.5 then
        "mostly helping"

    else
        "mixed"


lazySignal : LazyStat -> String
lazySignal stat =
    if stat.avgRender >= 8 then
        "expensive"

    else if stat.avgRender >= 1 then
        "noticeable"

    else
        "cheap"


rate : Int -> Int -> Float
rate part total =
    if total <= 0 then
        0

    else
        toFloat part / toFloat total


formatPercent : Float -> String
formatPercent value_ =
    String.fromFloat (toFloat (round (value_ * 1000)) / 10) ++ "%"


viewEventDetails : Model -> Event -> Html Msg
viewEventDetails model event =
    case event of
        Init initEvent ->
            div []
                [ viewDetailHeader "init" Nothing Nothing False
                , viewSection model "model" initEvent.model
                ]

        Update updateEvent ->
            div []
                [ viewDetailHeader updateEvent.message (Just updateEvent.duration) (Just updateEvent.id) (Set.member updateEvent.id model.messageDrawers)
                , if Set.member updateEvent.id model.messageDrawers then
                    viewSection model ("message-" ++ String.fromInt updateEvent.id) updateEvent.rawMessage

                  else
                    text ""
                , viewModelSection model updateEvent
                ]

        Frame _ ->
            div [ style "padding" "24px" ] [ text "Frame finished" ]

        Unknown _ ->
            div [ style "padding" "24px" ] [ text "Unknown debugger event" ]


viewDetailHeader : String -> Maybe Float -> Maybe Int -> Bool -> Html Msg
viewDetailHeader label maybeDuration maybePayloadId payloadOpen =
    div [ style "padding" "18px 22px", style "border-bottom" "1px solid rgba(246,164,0,.34)", style "display" "flex", style "align-items" "baseline", style "gap" "18px", style "justify-content" "space-between", style "background" "rgba(246,164,0,.08)" ]
        [ div []
            [ div [ style "font-size" "11px", style "text-transform" "uppercase", style "letter-spacing" "0.16em", style "color" "#9f7a31" ] [ text "message" ]
            , div [ style "font-size" "22px", style "font-weight" "800", style "color" "#ffd77a", style "overflow-wrap" "anywhere", style "display" "flex", style "align-items" "center", style "gap" "10px" ]
                [ text label
                , case maybePayloadId of
                    Just id ->
                        button [ onClick (ToggleMessagePayload id), style "background" "transparent", style "border" "1px solid rgba(246,164,0,.65)", style "border-radius" "4px", style "color" "#f6a400", style "font" "inherit", style "font-weight" "900", style "line-height" "1", style "cursor" "pointer", style "padding" "2px 7px", title "Toggle message payload" ]
                            [ text
                                (if payloadOpen then
                                    "-"

                                 else
                                    "+"
                                )
                            ]

                    Nothing ->
                        text ""
                ]
            ]
        , div [ style "text-align" "right", style "white-space" "nowrap" ]
            [ div [ style "font-size" "11px", style "text-transform" "uppercase", style "letter-spacing" "0.16em", style "color" "#9f7a31" ] [ text "update duration" ]
            , div [ style "font-size" "18px", style "font-weight" "800", style "color" "#f6a400" ] [ text (Maybe.withDefault "-" (Maybe.map formatMs maybeDuration)) ]
            ]
        ]


viewSection : Model -> String -> DebugValue -> Html Msg
viewSection model label value_ =
    div [ style "padding" "18px 22px", style "border-bottom" "1px solid rgba(246,164,0,.12)" ]
        [ div [ style "font-size" "11px", style "text-transform" "uppercase", style "letter-spacing" "0.16em", style "color" "#9f7a31", style "margin-bottom" "10px" ] [ text label ]
        , viewDebugValue model 0 label value_
        ]


viewModelSection : Model -> UpdateEvent -> Html Msg
viewModelSection model updateEvent =
    div [ style "padding" "18px 22px", style "border-bottom" "1px solid rgba(246,164,0,.12)" ]
        [ div [ style "display" "flex", style "align-items" "center", style "justify-content" "space-between", style "gap" "16px", style "margin-bottom" "14px" ]
            [ div [ style "font-size" "11px", style "text-transform" "uppercase", style "letter-spacing" "0.16em", style "color" "#9f7a31" ] [ text "model" ]
            , viewModelToggle model.modelView
            ]
        , case model.modelView of
            ModelBefore ->
                viewDebugValue model 0 "before" updateEvent.modelBefore

            ModelAfter ->
                viewDebugValue model 0 "after" updateEvent.modelAfter

            ModelDelta ->
                viewDebugDelta model "delta" updateEvent.modelBefore updateEvent.modelAfter
        ]


viewModelToggle : ModelView -> Html Msg
viewModelToggle current =
    div [ style "display" "inline-grid", style "grid-template-columns" "repeat(3, max-content)", style "gap" "4px", style "padding" "3px", style "border" "1px solid rgba(246,164,0,.28)", style "border-radius" "999px", style "background" "rgba(8,9,10,.46)" ]
        [ viewModelToggleButton current ModelBefore "Before"
        , viewModelToggleButton current ModelAfter "After"
        , viewModelToggleButton current ModelDelta "Delta"
        ]


viewModelToggleButton : ModelView -> ModelView -> String -> Html Msg
viewModelToggleButton current target label =
    let
        selected =
            current == target
    in
    button
        [ onClick (SelectModelView target)
        , style "border" "0"
        , style "border-radius" "999px"
        , style "background"
            (if selected then
                "#f6a400"

             else
                "transparent"
            )
        , style "color"
            (if selected then
                "#140c00"

             else
                "#ffd77a"
            )
        , style "font" "inherit"
        , style "font-weight" "900"
        , style "cursor" "pointer"
        , style "padding" "5px 11px"
        ]
        [ text label ]


viewDebugDelta : Model -> String -> DebugValue -> DebugValue -> Html Msg
viewDebugDelta model path before after =
    if before == after then
        div [ style "padding" "18px", style "border" "1px dashed rgba(246,164,0,.28)", style "border-radius" "10px", style "color" "#9f7a31" ]
            [ text "No model changes for this message." ]

    else
        case deltaChildren model path before after of
            [] ->
                viewChangedValue model path before after

            children ->
                div [ style "display" "grid", style "gap" "10px" ] children


deltaChildren : Model -> String -> DebugValue -> DebugValue -> List (Html Msg)
deltaChildren model path before after =
    case ( before, after ) of
        ( RecordValue beforeFields, RecordValue afterFields ) ->
            changedRecordFields model path beforeFields afterFields

        ( ConstructorValue beforeName beforeArgs, ConstructorValue afterName afterArgs ) ->
            if beforeName == afterName && List.length beforeArgs == List.length afterArgs then
                List.indexedMap (viewChangedConstructorArg model path beforeArgs) afterArgs
                    |> List.filterMap identity

            else
                []

        ( ListValue beforeItems, ListValue afterItems ) ->
            changedIndexedItems model path beforeItems afterItems

        ( ArrayValue beforeItems, ArrayValue afterItems ) ->
            changedIndexedItems model path beforeItems afterItems

        _ ->
            []


changedRecordFields : Model -> String -> List Field -> List Field -> List (Html Msg)
changedRecordFields model path beforeFields afterFields =
    afterFields
        |> List.filterMap
            (\afterField ->
                case fieldByName afterField.name beforeFields of
                    Just beforeField ->
                        if beforeField.value == afterField.value then
                            Nothing

                        else
                            Just (viewChangedField model (path ++ "." ++ afterField.name) afterField.name beforeField.value afterField.value)

                    Nothing ->
                        Just (viewChangedField model (path ++ "." ++ afterField.name) afterField.name (OpaqueValue "<missing>") afterField.value)
            )


fieldByName : String -> List Field -> Maybe Field
fieldByName name fields =
    case fields of
        [] ->
            Nothing

        field :: rest ->
            if field.name == name then
                Just field

            else
                fieldByName name rest


changedIndexedItems : Model -> String -> List DebugValue -> List DebugValue -> List (Html Msg)
changedIndexedItems model path beforeItems afterItems =
    List.indexedMap (viewChangedIndexed model path beforeItems) afterItems
        |> List.filterMap identity


viewChangedIndexed : Model -> String -> List DebugValue -> Int -> DebugValue -> Maybe (Html Msg)
viewChangedIndexed model path beforeItems index after =
    case listAt index beforeItems of
        Just before ->
            if before == after then
                Nothing

            else
                Just (viewChangedField model (path ++ "." ++ String.fromInt index) (String.fromInt index) before after)

        Nothing ->
            Just (viewChangedField model (path ++ "." ++ String.fromInt index) (String.fromInt index) (OpaqueValue "<missing>") after)


viewChangedConstructorArg : Model -> String -> List DebugValue -> Int -> DebugValue -> Maybe (Html Msg)
viewChangedConstructorArg model path beforeArgs index after =
    case listAt index beforeArgs of
        Just before ->
            if before == after then
                Nothing

            else
                Just (viewChangedField model (path ++ "." ++ String.fromInt index) ("arg " ++ String.fromInt index) before after)

        Nothing ->
            Just (viewChangedField model (path ++ "." ++ String.fromInt index) ("arg " ++ String.fromInt index) (OpaqueValue "<missing>") after)


listAt : Int -> List a -> Maybe a
listAt index list =
    if index < 0 then
        Nothing

    else
        case ( index, list ) of
            ( 0, item :: _ ) ->
                Just item

            ( _, _ :: rest ) ->
                listAt (index - 1) rest

            ( _, [] ) ->
                Nothing


viewChangedField : Model -> String -> String -> DebugValue -> DebugValue -> Html Msg
viewChangedField model path label before after =
    case deltaChildren model path before after of
        [] ->
            div [ style "border" "1px solid rgba(246,164,0,.24)", style "border-radius" "10px", style "background" "rgba(8,9,10,.48)", style "overflow" "hidden" ]
                [ div [ style "padding" "8px 11px", style "background" "rgba(246,164,0,.1)", style "color" "#f6a400", style "font-weight" "900" ] [ text label ]
                , viewChangedValue model path before after
                ]

        children ->
            div [ style "border" "1px solid rgba(246,164,0,.24)", style "border-radius" "10px", style "background" "rgba(8,9,10,.35)", style "overflow" "hidden" ]
                [ div [ style "padding" "8px 11px", style "background" "rgba(246,164,0,.1)", style "color" "#f6a400", style "font-weight" "900" ] [ text label ]
                , div [ style "display" "grid", style "gap" "8px", style "padding" "10px" ] children
                ]


viewChangedValue : Model -> String -> DebugValue -> DebugValue -> Html Msg
viewChangedValue model path before after =
    div [ style "display" "grid", style "grid-template-columns" "minmax(0, 1fr) max-content minmax(0, 1fr)", style "gap" "10px", style "align-items" "stretch", style "padding" "10px" ]
        [ viewDeltaValue model (path ++ ".before") "before" before
        , div [ style "display" "flex", style "align-items" "center", style "color" "#f6a400", style "font-weight" "900" ] [ text "->" ]
        , viewDeltaValue model (path ++ ".after") "after" after
        ]


viewDeltaValue : Model -> String -> String -> DebugValue -> Html Msg
viewDeltaValue model path label value_ =
    div [ style "border" "1px solid rgba(246,164,0,.18)", style "border-radius" "8px", style "padding" "9px", style "background" "rgba(0,0,0,.22)", style "min-width" "0" ]
        [ div [ style "font-size" "10px", style "text-transform" "uppercase", style "letter-spacing" "0.14em", style "color" "#9f7a31", style "margin-bottom" "6px" ] [ text label ]
        , viewDebugValue model 0 path value_
        ]


viewDebugValue : Model -> Int -> String -> DebugValue -> Html Msg
viewDebugValue model depth path value_ =
    case value_ of
        BoolValue bool ->
            viewAtom
                (if bool then
                    "True"

                 else
                    "False"
                )

        NumberValue number ->
            viewAtom number

        StringValue string ->
            viewString string

        CharValue char ->
            viewAtom ("'" ++ char ++ "'")

        OpaqueValue label ->
            viewAtom label

        RecordValue fields ->
            viewRecord model depth path fields

        ConstructorValue maybeName args ->
            case viewInlineSingleArgConstructor maybeName args of
                Just inlineConstructor ->
                    inlineConstructor

                Nothing ->
                    viewConstructor model depth path maybeName args

        ListValue items ->
            viewSequence model depth path "[" "]" items

        ArrayValue items ->
            viewSequence model depth path "Array [" "]" items

        SetValue items ->
            if List.isEmpty items then
                viewAtom "Set {}"

            else
                viewSequence model depth path "Set {" "}" items

        DictValue entries ->
            viewDict model depth path entries


viewBranch : Model -> Int -> String -> String -> String -> (() -> List (Html Msg)) -> Html Msg
viewBranch model depth path opener closer children =
    let
        isCollapsed =
            branchIsCollapsed model depth path

        childNodes =
            if isCollapsed then
                []

            else
                children ()

        inlineOpener =
            button [ onClick (ToggleCollapse path isCollapsed), style "background" "transparent", style "border" "0", style "color" "#ffd77a", style "font" "inherit", style "font-weight" "800", style "cursor" "pointer", style "padding" "1px 0" ]
                [ text <|
                    if isCollapsed then
                        "+ " ++ opener ++ " ... " ++ closer

                    else
                        "- " ++ opener
                ]
    in
    if isCollapsed then
        div [ style "margin" "2px 0" ] [ inlineOpener ]

    else
        case childNodes of
            [] ->
                div [ style "margin" "2px 0" ] [ inlineOpener, text (" " ++ closer) ]

            first :: rest ->
                div [ style "margin" "2px 0" ]
                    [ div [ style "display" "grid", style "grid-template-columns" "max-content 1fr", style "column-gap" "1ch", style "align-items" "start" ]
                        [ inlineOpener
                        , first
                        ]
                    , div [ style "border-left" "1px solid rgba(246,164,0,.24)", style "margin-left" "0", style "padding-left" "2ch" ] rest
                    , div [ style "color" "#ffd77a", style "font-weight" "800", style "padding-left" "2ch" ] [ text closer ]
                    ]


viewCompactBranch : Model -> Int -> String -> String -> String -> (() -> List (Html Msg)) -> Html Msg
viewCompactBranch model depth path collapsedLabel openLabel children =
    let
        isCollapsed =
            branchIsCollapsed model depth path

        childNodes =
            if isCollapsed then
                []

            else
                children ()

        inlineOpener =
            button [ onClick (ToggleCollapse path isCollapsed), style "background" "transparent", style "border" "0", style "color" "#ffd77a", style "font" "inherit", style "font-weight" "800", style "cursor" "pointer", style "padding" "1px 0" ]
                [ text <|
                    if isCollapsed then
                        "+ " ++ collapsedLabel

                    else
                        "- " ++ openLabel
                ]
    in
    if isCollapsed then
        div [ style "margin" "2px 0" ] [ inlineOpener ]

    else
        case childNodes of
            [] ->
                div [ style "margin" "2px 0" ] [ inlineOpener ]

            first :: rest ->
                div [ style "margin" "2px 0" ]
                    [ div [ style "display" "grid", style "grid-template-columns" "max-content 1fr", style "column-gap" "1ch", style "align-items" "start" ]
                        [ inlineOpener
                        , first
                        ]
                    , div [ style "border-left" "1px solid rgba(246,164,0,.24)", style "margin-left" "0", style "padding-left" "2ch" ] rest
                    ]


branchIsCollapsed : Model -> Int -> String -> Bool
branchIsCollapsed model depth path =
    Set.member path model.collapsed || (depth >= defaultOpenDepth && not (Set.member path model.expanded))


viewRecord : Model -> Int -> String -> List Field -> Html Msg
viewRecord model depth path fields =
    case fields of
        [ field ] ->
            case viewInlineField field of
                Just inlineField ->
                    span [] [ text "{ ", inlineField, text " }" ]

                Nothing ->
                    viewBranch model depth path "{" "}" (\_ -> List.indexedMap (viewField model (depth + 1) path) fields)

        _ ->
            viewBranch model depth path "{" "}" (\_ -> List.indexedMap (viewField model (depth + 1) path) fields)


viewSequence : Model -> Int -> String -> String -> String -> List DebugValue -> Html Msg
viewSequence model depth path opener closer items =
    viewBranch model depth path opener closer (\_ -> List.indexedMap (viewIndexed model (depth + 1) path) items)


viewDict : Model -> Int -> String -> List DictEntry -> Html Msg
viewDict model depth path entries =
    viewBranch model depth path "Dict {" "}" (\_ -> List.indexedMap (viewDictEntry model (depth + 1) path) entries)


viewConstructor : Model -> Int -> String -> Maybe String -> List DebugValue -> Html Msg
viewConstructor model depth path maybeName args =
    let
        name =
            Maybe.withDefault "Tuple" maybeName
    in
    case args of
        [] ->
            viewAtom name

        _ ->
            let
                summary =
                    name ++ " " ++ String.join " " (List.map (\_ -> "(...)") args)
            in
            viewCompactBranch model depth path summary name (\_ -> List.indexedMap (viewConstructorArg model (depth + 1) path) args)


viewInlineSingleArgConstructor : Maybe String -> List DebugValue -> Maybe (Html Msg)
viewInlineSingleArgConstructor maybeName args =
    case args of
        [ arg ] ->
            Maybe.map
                (\inlineArg ->
                    span []
                        [ span [ style "color" "#ffd77a", style "font-weight" "800" ] [ text (Maybe.withDefault "Tuple" maybeName) ]
                        , text " "
                        , inlineArg
                        ]
                )
                (viewInlineDebugValue arg)

        _ ->
            Nothing


viewField : Model -> Int -> String -> Int -> Field -> Html Msg
viewField model depth parent index field =
    case field.value of
        RecordValue fields ->
            if List.length fields > 1 then
                div [ style "padding" "2px 0" ]
                    [ div [ style "color" "#f6a400" ] [ text (fieldPrefix index ++ field.name ++ ":") ]
                    , div [ style "padding-left" "2ch" ] [ viewDebugValue model depth (parent ++ "." ++ field.name) field.value ]
                    ]

            else
                viewInlineFieldRow model depth parent index field

        _ ->
            if isMultilineValue field.value then
                div [ style "padding" "2px 0" ]
                    [ div [ style "color" "#f6a400" ] [ text (fieldPrefix index ++ field.name ++ ":") ]
                    , div [ style "padding-left" "2ch" ] [ viewDebugValue model depth (parent ++ "." ++ field.name) field.value ]
                    ]

            else
                viewInlineFieldRow model depth parent index field


viewInlineFieldRow : Model -> Int -> String -> Int -> Field -> Html Msg
viewInlineFieldRow model depth parent index field =
    div [ style "display" "grid", style "grid-template-columns" "max-content 1fr", style "column-gap" "1ch", style "align-items" "start", style "padding" "2px 0" ]
        [ span [ style "color" "#f6a400" ] [ text (fieldPrefix index ++ field.name ++ ":") ]
        , viewDebugValue model depth (parent ++ "." ++ field.name) field.value
        ]


fieldPrefix : Int -> String
fieldPrefix index =
    if index == 0 then
        ""

    else
        ", "


isMultilineValue : DebugValue -> Bool
isMultilineValue value_ =
    case value_ of
        RecordValue fields ->
            List.length fields > 1

        ConstructorValue _ args ->
            not (constructorArgsAreInline args) && not (List.isEmpty args)

        ListValue items ->
            not (List.isEmpty items)

        ArrayValue items ->
            not (List.isEmpty items)

        SetValue items ->
            not (List.isEmpty items)

        DictValue entries ->
            not (List.isEmpty entries)

        _ ->
            False


constructorArgsAreInline : List DebugValue -> Bool
constructorArgsAreInline args =
    List.length (List.filterMap viewInlineDebugValue args) == List.length args


viewInlineField : Field -> Maybe (Html Msg)
viewInlineField field =
    Maybe.map
        (\inlineValue ->
            span []
                [ span [ style "color" "#f6a400" ] [ text (field.name ++ ": ") ]
                , inlineValue
                ]
        )
        (viewInlineDebugValue field.value)


viewInlineDebugValue : DebugValue -> Maybe (Html Msg)
viewInlineDebugValue value_ =
    case value_ of
        BoolValue bool ->
            Just
                (viewAtom
                    (if bool then
                        "True"

                     else
                        "False"
                    )
                )

        NumberValue number ->
            Just (viewAtom number)

        StringValue string ->
            Just (viewString string)

        CharValue char ->
            Just (viewAtom ("'" ++ char ++ "'"))

        OpaqueValue label ->
            Just (viewAtom label)

        ConstructorValue maybeName [] ->
            Just (viewAtom (Maybe.withDefault "Tuple" maybeName))

        ConstructorValue maybeName args ->
            let
                inlineArgs =
                    List.filterMap viewInlineDebugValue args
            in
            if List.length inlineArgs == List.length args then
                Just <|
                    span []
                        (span [ style "color" "#ffd77a", style "font-weight" "800" ] [ text (Maybe.withDefault "Tuple" maybeName) ]
                            :: List.concatMap (\arg -> [ text " ", arg ]) inlineArgs
                        )

            else
                Nothing

        RecordValue [ field ] ->
            Maybe.map (\inlineField -> span [] [ text "{ ", inlineField, text " }" ]) (viewInlineField field)

        _ ->
            Nothing


viewConstructorArg : Model -> Int -> String -> Int -> DebugValue -> Html Msg
viewConstructorArg model depth parent index value_ =
    let
        path =
            parent ++ "." ++ String.fromInt index
    in
    case viewInlineDebugValue value_ of
        Just inlineValue ->
            div [ style "padding" "2px 0" ] [ text "(", inlineValue, text ")" ]

        Nothing ->
            div [ style "padding" "2px 0" ] [ viewDebugValue model depth path value_ ]


viewIndexed : Model -> Int -> String -> Int -> DebugValue -> Html Msg
viewIndexed model depth parent index value_ =
    div [ style "display" "grid", style "grid-template-columns" "max-content 1fr", style "column-gap" "8px", style "align-items" "start", style "padding" "2px 0" ]
        [ span [ style "color" "#9f7a31" ] [ text (String.fromInt index ++ ":") ]
        , viewDebugValue model depth (parent ++ "." ++ String.fromInt index) value_
        ]


viewDictEntry : Model -> Int -> String -> Int -> DictEntry -> Html Msg
viewDictEntry model depth parent index entry =
    div [ style "display" "grid", style "grid-template-columns" "max-content 24px 1fr", style "gap" "8px", style "align-items" "start", style "padding" "2px 0" ]
        [ viewDebugValue model depth (parent ++ "." ++ String.fromInt index ++ ".key") entry.key
        , span [ style "color" "#9f7a31" ] [ text "=>" ]
        , viewDebugValue model depth (parent ++ "." ++ String.fromInt index ++ ".value") entry.value
        ]


viewAtom : String -> Html Msg
viewAtom label =
    span [ style "color" "#ffe6aa", style "overflow-wrap" "anywhere" ] [ text label ]


viewString : String -> Html Msg
viewString string =
    let
        rendered =
            Encode.encode 0 (Encode.string string)
    in
    if String.length string > 120 then
        button [ onClick (CopyToClipboard string), title "Copy full string", style "background" "rgba(246,164,0,.08)", style "border" "1px solid rgba(246,164,0,.28)", style "border-radius" "4px", style "color" "#ffe6aa", style "font" "inherit", style "cursor" "copy", style "padding" "1px 4px", style "text-align" "left", style "overflow-wrap" "anywhere" ]
            [ text (String.left 120 rendered ++ "...") ]

    else
        viewAtom rendered


formatMs : Float -> String
formatMs duration =
    String.fromFloat (toFloat (round (duration * 100)) / 100) ++ "ms"


selectableEvents : List Event -> List Event
selectableEvents events =
    List.filter
        (\event ->
            case event of
                Frame _ ->
                    False

                Unknown _ ->
                    False

                _ ->
                    True
        )
        events


eventById : List Event -> Int -> Maybe Event
eventById events id =
    case events of
        [] ->
            Nothing

        event :: rest ->
            if eventId event == id then
                Just event

            else
                eventById rest id


eventId : Event -> Int
eventId event =
    case event of
        Init initEvent ->
            initEvent.id

        Update updateEvent ->
            updateEvent.id

        Frame frameEvent ->
            frameEvent.id

        Unknown _ ->
            -1


eventLabel : Event -> String
eventLabel event =
    case event of
        Init _ ->
            "init"

        Update updateEvent ->
            updateEvent.message

        Frame _ ->
            "frame"

        Unknown _ ->
            "unknown"


runtimeMessageDecoder : Decoder RuntimeMessage
runtimeMessageDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\eventType ->
                case eventType of
                    "init" ->
                        Decode.map (RuntimeDebuggerEvent << Init) initEventDecoder

                    "update" ->
                        Decode.map (RuntimeDebuggerEvent << Update) updateEventDecoder

                    "frame" ->
                        Decode.map (RuntimeDebuggerEvent << Frame) frameEventDecoder

                    "performance" ->
                        Decode.map RuntimePerformance performanceSnapshotDecoder

                    _ ->
                        Decode.value |> Decode.map (RuntimeDebuggerEvent << Unknown)
            )


performanceSnapshotDecoder : Decoder PerformanceSnapshot
performanceSnapshotDecoder =
    Decode.map PerformanceSnapshot
        (Decode.field "lazy" (Decode.list lazyStatDecoder))


lazyStatDecoder : Decoder LazyStat
lazyStatDecoder =
    Decode.map8 LazyStat
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "arity" Decode.int)
        (Decode.field "calls" Decode.int)
        (Decode.field "renders" Decode.int)
        (Decode.field "hits" Decode.int)
        (Decode.field "avgRender" Decode.float)
        (Decode.field "maxRender" Decode.float)


initEventDecoder : Decoder InitEvent
initEventDecoder =
    Decode.map2 InitEvent
        (Decode.field "id" Decode.int)
        (Decode.field "model" debugValueDecoder)


updateEventDecoder : Decoder UpdateEvent
updateEventDecoder =
    Decode.map6 UpdateEvent
        (Decode.field "id" Decode.int)
        (Decode.field "message" Decode.string)
        (Decode.field "rawMessage" debugValueDecoder)
        (Decode.field "modelBefore" debugValueDecoder)
        (Decode.field "modelAfter" debugValueDecoder)
        (Decode.field "duration" Decode.float)


frameEventDecoder : Decoder FrameEvent
frameEventDecoder =
    Decode.map3 FrameEvent
        (Decode.field "id" Decode.int)
        (Decode.field "duration" Decode.float)
        (Decode.field "renderDuration" Decode.float)


debugValueDecoder : Decoder DebugValue
debugValueDecoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen debugValueByKind


debugValueByKind : String -> Decoder DebugValue
debugValueByKind kind =
    case kind of
        "bool" ->
            Decode.map BoolValue (Decode.field "value" Decode.bool)

        "number" ->
            Decode.map NumberValue (Decode.field "value" Decode.string)

        "string" ->
            Decode.map StringValue (Decode.field "value" Decode.string)

        "char" ->
            Decode.map CharValue (Decode.field "value" Decode.string)

        "record" ->
            Decode.map RecordValue (Decode.field "fields" (Decode.list fieldDecoder))

        "constructor" ->
            Decode.map2 ConstructorValue
                (Decode.field "name" (Decode.nullable Decode.string))
                (Decode.field "args" (Decode.list debugValueDecoder))

        "list" ->
            Decode.map ListValue (Decode.field "items" (Decode.list debugValueDecoder))

        "array" ->
            Decode.map ArrayValue (Decode.field "items" (Decode.list debugValueDecoder))

        "set" ->
            Decode.map SetValue (Decode.field "items" (Decode.list debugValueDecoder))

        "dict" ->
            Decode.map DictValue (Decode.field "entries" (Decode.list dictEntryDecoder))

        "opaque" ->
            Decode.map OpaqueValue (Decode.field "label" Decode.string)

        _ ->
            Decode.succeed (OpaqueValue "<unknown>")


fieldDecoder : Decoder Field
fieldDecoder =
    Decode.map2 Field
        (Decode.field "name" Decode.string)
        (Decode.field "value" debugValueDecoder)


dictEntryDecoder : Decoder DictEntry
dictEntryDecoder =
    Decode.map2 DictEntry
        (Decode.field "key" debugValueDecoder)
        (Decode.field "value" debugValueDecoder)
