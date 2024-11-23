module Main exposing (main)

import Browser
import Dict
import Editor
import Element as Ui
import Element.Border as Border
import Element.Events as Events
import Element.Keyed as Keyed
import Elm.ProjectStatus
import Explainer
import Flags
import Html
import Html.Attributes
import Json.Decode as Decode
import Json.Encode
import Model exposing (..)
import Navigator
import Ports
import Question
import Set exposing (Set)
import Time.Distance
import Ui
import Ui.Card
import Ui.WindowHeader


main : Program Json.Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Ports.incoming Incoming

                    -- , Time.every 500 CurrentTime
                    ]
        }


init : Json.Encode.Value -> ( Model, Cmd Msg )
init flagsJson =
    ( { server = { status = Ports.Disconnected }
      , flags =
            Decode.decodeValue Flags.decoder flagsJson
                |> Result.toMaybe
      , active = Nothing
      , visible = []
      , projects = []
      , projectsVersion = 0
      , now = Nothing
      , lastUpdated = Nothing
      , viewing = ViewingProjectList
      , warnings = Dict.empty
      , errorMenuVisible = False
      , errorCodeExpanded = Set.empty
      , callgraph = Dict.empty
      , facts = Dict.empty
      }
    , Cmd.none
    )


panelLog : a -> a
panelLog =
    Debug.log "Msg"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case panelLog msg of
        WindowMinimizeClicked ->
            ( model
            , Ports.outgoing Ports.WindowMinimize
            )

        WindowMaximizeClicked ->
            ( model
            , Ports.outgoing Ports.WindowMaximize
            )

        WindowCloseClicked ->
            ( model
            , Ports.outgoing Ports.WindowClose
            )

        Incoming (Err err) ->
            let
                _ =
                    panelLog err
            in
            ( model
            , Cmd.none
            )

        Incoming (Ok editorMsg) ->
            case editorMsg of
                Ports.VisibleEditorsUpdated { visible } ->
                    let
                        _ =
                            panelLog "VisibleEditorsUpdated"
                    in
                    ( { model
                        | visible = visible
                        , active =
                            visible
                                |> List.filter .active
                                |> List.head
                      }
                    , List.filterMap
                        (\editor ->
                            if String.endsWith ".elm" editor.filepath then
                                let
                                    _ =
                                        panelLog ("Asking for callgraph: " ++ editor.filepath)
                                in
                                Just
                                    (Question.ask.callgraph editor.filepath
                                        |> Cmd.map AnswerReceived
                                    )

                            else
                                Nothing
                        )
                        visible
                        |> Cmd.batch
                    )

                Ports.ProjectsStatusUpdated projects ->
                    let
                        _ =
                            panelLog "ProjectsStatusUpdated"
                    in
                    ( { model
                        | projects = projects
                        , projectsVersion = model.projectsVersion + 1
                        , errorMenuVisible = False
                        , errorCodeExpanded = Set.empty
                        , lastUpdated = model.now
                      }
                    , Cmd.none
                    )

                Ports.WarningsUpdated { filepath, warnings } ->
                    let
                        _ =
                            panelLog "WarningsUpdated"
                    in
                    ( { model
                        | warnings = Dict.insert filepath warnings model.warnings
                        , lastUpdated = model.now
                      }
                    , Cmd.none
                    )

                Ports.CallGraphReceived callgraph ->
                    let
                        _ =
                            panelLog ("Callgraph received! " ++ callgraph.filepath)
                    in
                    ( { model
                        | callgraph = Dict.insert callgraph.filepath callgraph.nodes model.callgraph
                        , lastUpdated = model.now
                      }
                    , Cmd.none
                    )

                Ports.ExplanationReceived { filepath, facts } ->
                    let
                        _ =
                            panelLog "Facts received!"
                    in
                    ( { model
                        | facts = Dict.insert filepath facts model.facts
                        , lastUpdated = model.now
                      }
                    , Cmd.none
                    )

                Ports.ServerStatusUpdated server ->
                    ( { model | server = server }
                    , case server.status of
                        Ports.Connected _ ->
                            Ports.outgoing Ports.RequestProjectList

                        _ ->
                            Cmd.none
                    )

        ErrorMenuUpdated new ->
            ( { model | errorMenuVisible = new }
            , Cmd.none
            )

        ErrorCodeToggled ref bool ->
            ( { model
                | errorCodeExpanded =
                    if bool then
                        Set.insert ref model.errorCodeExpanded

                    else
                        Set.remove ref model.errorCodeExpanded
              }
            , Cmd.none
            )

        EditorGoTo path region ->
            ( model
            , Ports.outgoing
                (Ports.Goto
                    { file = path
                    , region = region
                    }
                )
            )

        EditorFillTypeSignatures path ->
            ( model
            , Ports.outgoing
                (Ports.FillTypeSignatures path)
            )

        View viewing ->
            ( { model | viewing = viewing }
            , Cmd.none
            )

        CurrentTime newTime ->
            ( { model | now = Just newTime }
            , Cmd.none
            )

        AnswerReceived (Err err) ->
            let
                _ =
                    panelLog err
            in
            ( model
            , Cmd.none
            )

        AnswerReceived (Ok answer) ->
            case answer of
                Question.MissingTypeSignatures path missing ->
                    ( model
                    , Cmd.none
                    )

                Question.Health health ->
                    ( model
                    , Cmd.none
                    )

                Question.Status status ->
                    ( model
                    , Cmd.none
                    )

                Question.CallGraph callgraph ->
                    ( { model
                        | callgraph = Dict.insert callgraph.filepath callgraph.nodes model.callgraph
                      }
                    , Cmd.none
                    )


mergeProjects new existing =
    let
        ( newProjects, didMerge ) =
            List.foldl
                (\exist ( gathered, merged ) ->
                    if merged then
                        ( exist :: gathered, merged )

                    else if exist.root == new.root then
                        ( new :: gathered, True )

                    else
                        ( exist :: gathered, merged )
                )
                ( [], False )
                existing
    in
    if didMerge then
        newProjects

    else
        new :: newProjects


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "background-color" "#f5f5f5"
        , Html.Attributes.style "color" "black"
        , Html.Attributes.style "border-radius" "10px"
        ]
        [ Ui.overrides
        , Ui.layout
            [ Ui.htmlAttribute (Html.Attributes.class "base")
            , Ui.width Ui.fill
            , Ui.height Ui.fill
            , Ui.inFront
                (Ui.el
                    [ Ui.alignBottom
                    , Ui.alignRight
                    ]
                    (viewLastUpdated model)
                )
            , Ui.inFront
                (Ui.el
                    [ Ui.alignBottom
                    , Ui.alignLeft
                    ]
                    (viewVisible model)
                )
            ]
            (Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill ]
                [ Ui.WindowHeader.view
                    { onMinimize = WindowMinimizeClicked
                    , onMaximize = WindowMaximizeClicked
                    , onClose = WindowCloseClicked
                    , title = viewServerStatus model.server
                    , platform =
                        model.flags
                            |> Maybe.map .platform
                            |> Maybe.withDefault Flags.Mac
                    }
                , case model.viewing of
                    Model.ViewingProjectList ->
                        viewProjects model
                            |> Ui.el
                                [ Ui.width Ui.fill
                                , Ui.height Ui.fill
                                , Ui.htmlAttribute (Html.Attributes.style "overflow" "auto")
                                ]

                    Model.ViewingProject project ->
                        viewProject model project
                ]
            )
        ]


viewServerStatus : Ports.Server -> Ui.Element Msg
viewServerStatus server =
    case server.status of
        Ports.Disconnected ->
            Ui.el
                [ Ui.pad.xy.lg.md
                ]
                (Ui.text "Disconnected")

        Ports.Connecting ->
            Ui.el
                [ Ui.pad.xy.lg.md
                ]
                (Ui.text "Connecting")

        Ports.Connected info ->
            Ui.el
                [ Ui.pad.xy.lg.md
                ]
                (Ui.text (info.host ++ ":" ++ info.port_))


viewProjects : Model -> Ui.Element Msg
viewProjects model =
    Ui.column [ Ui.spacing 24, Ui.padding 24 ]
        [ Ui.text ("Projects (" ++ String.fromInt (List.length model.projects) ++ ")")
        , Ui.column [ Ui.spacing 24, Ui.padding 24 ]
            (List.map viewProjectCard model.projects)
        ]


viewProjectCard : Elm.ProjectStatus.Project -> Ui.Element Msg
viewProjectCard project =
    Ui.column
        [ Ui.spacing 12
        , Ui.padding 16
        , Ui.rounded.md
        , Ui.border.grey.dark
        , Border.width 1
        , Ui.pointer
        , Events.onClick (Model.View (Model.ViewingProject project))
        ]
        [ Ui.text project.root
        , Ui.text project.projectRoot
        , viewProjectStatus project.status
        , Ui.row []
            (List.map Ui.text project.entrypoints)
        ]


viewProjectStatus : Elm.ProjectStatus.Status -> Ui.Element Msg
viewProjectStatus status =
    case status of
        Elm.ProjectStatus.NoData ->
            Ui.text "No data"

        Elm.ProjectStatus.Success ->
            Ui.text "Success"

        Elm.ProjectStatus.GlobalError globe ->
            Ui.text "Global error"

        Elm.ProjectStatus.CompilerError { errors } ->
            Ui.text "Compiler error"


viewProject : Model -> Elm.ProjectStatus.Project -> Ui.Element Msg
viewProject model project =
    let
        found =
            case project.status of
                Elm.ProjectStatus.NoData ->
                    { globals = []
                    , errs = []
                    }

                Elm.ProjectStatus.Success ->
                    { globals = []
                    , errs = []
                    }

                Elm.ProjectStatus.GlobalError globe ->
                    { globals = [ globe ]
                    , errs = []
                    }

                Elm.ProjectStatus.CompilerError { errors } ->
                    { globals = []
                    , errs = errors
                    }

        viewSignatureGroup ( editor, signatures ) =
            Ui.column
                [ Ui.space.md ]
                [ Ui.header.three editor.filepath
                , Ui.el
                    [ Events.onClick (EditorFillTypeSignatures editor.filepath)
                    , Ui.pad.sm
                    , Ui.border.primary
                    , Border.width 1
                    , Ui.rounded.md
                    , Ui.pointer
                    ]
                    (Ui.text "Add all missing typesignatures")
                , Ui.column [ Ui.space.md ]
                    (List.map (viewTypeSignature editor.filepath) signatures)
                ]

        viewTypeSignature : String -> Question.TypeSignature -> Ui.Element Msg
        viewTypeSignature file signature =
            let
                expanded =
                    -- isVisible model.visible file signature.region
                    not (String.contains "\n" signature.signature)
            in
            Ui.column
                [ Events.onClick (EditorGoTo file signature.region)
                , Ui.pointer
                , Ui.space.sm
                ]
                [ Ui.row []
                    [ if signature.region.start.line == signature.region.end.line then
                        Ui.el
                            [ Ui.font.cyan
                            , Ui.alpha 0.5
                            , Ui.width (Ui.px 50)
                            ]
                            (Ui.text (String.fromInt signature.region.start.line))

                      else
                        Ui.row
                            [ Ui.font.cyan
                            , Ui.alpha 0.5
                            , Ui.width (Ui.px 50)
                            ]
                            [ Ui.text (String.fromInt signature.region.start.line)
                            , Ui.text ":"
                            , Ui.text (String.fromInt signature.region.end.line)
                            ]
                    , Ui.el [ Ui.font.cyan ]
                        (Ui.text (signature.name ++ " : "))
                    , if not (String.contains "\n" signature.signature) then
                        Ui.el
                            [ Ui.precise
                            ]
                            (Ui.text signature.signature)

                      else
                        Ui.el [ Ui.alpha 0.5 ] (Ui.text "<multiline>")
                    ]

                -- ,
                --  if  not (String.contains "\n" signature.signature) then
                --     Ui.paragraph
                --         [ Ui.pad.xy.xl.sm
                --         , Ui.precise
                --         ]
                --         [ Ui.text signature.signature ]
                --   else
                --     Ui.none
                ]

        ( visibleErrors, notVisibleErrors ) =
            List.partition
                (\file ->
                    isEditorVisible file model.visible
                )
                found.errs

        visibleFileNames =
            visibleErrors
                |> List.map .name
                |> String.join ", "
    in
    Ui.column
        [ Ui.space.lg
        , Ui.width Ui.fill
        , Ui.height Ui.fill
        , Ui.pad.xl
        , Ui.inFront
            (Ui.el
                [ Ui.alignLeft
                , Ui.alignTop
                , Ui.pointer
                , Events.onClick (Model.View ViewingProjectList)
                ]
                (Ui.text "< Back")
            )
        ]
        [ Ui.row [ Ui.width Ui.fill ]
            [ Ui.el [ Ui.font.cyan ]
                (Ui.text visibleFileNames)
            , foundErrorsMenu model found
            ]
        , Dict.toList model.callgraph
            |> Debug.log "callgraph"
            |> List.map
                (\( filepath, callgraph ) ->
                    Ui.column [ Ui.width Ui.fill ]
                        [ Ui.text filepath
                        , Navigator.view callgraph
                        ]
                )
            |> Ui.column [ Ui.width Ui.fill, Ui.htmlAttribute (Html.Attributes.id "graph") ]

        -- , viewMetric "Missing typesignatures"
        --     viewSignatureGroup
        --     missing
        -- ,
        , case visibleErrors of
            [] ->
                case notVisibleErrors of
                    [] ->
                        case found.globals of
                            [] ->
                                case List.head <| Dict.values model.callgraph of
                                    Nothing ->
                                        case List.head <| Dict.values model.facts of
                                            Nothing ->
                                                viewWarningsOrStatus model

                                            Just facts ->
                                                Explainer.view facts

                                    Just callgraph ->
                                        Navigator.view callgraph

                            -- Ui.html Ui.showLive
                            _ ->
                                viewMetric
                                    "Global"
                                    viewGlobalError
                                    found.globals

                    _ ->
                        Ui.column
                            [ Ui.space.lg
                            ]
                            [ Ui.header.three "Errors"
                            , Ui.column [ Ui.space.md ]
                                (List.map (viewFileSummary model) notVisibleErrors)
                            ]

            _ ->
                Ui.column
                    [ Ui.space.md
                    , Ui.width (Ui.px 700)

                    -- , Ui.htmlAttribute (Html.Attributes.style "height" "calc(100% - 50px)")
                    , Ui.height Ui.fill
                    , Ui.htmlAttribute (Html.Attributes.style "overflow" "auto")
                    ]
                    (List.map (viewFileErrorDetails model) visibleErrors)
        ]


viewWarningsOrStatus : Model -> Ui.Element Msg
viewWarningsOrStatus model =
    let
        activeWarnings =
            model.visible
                |> List.concatMap (visibleWarnings model.warnings)
    in
    case activeWarnings of
        [] ->
            case List.concat (Dict.values model.warnings) of
                [] ->
                    Keyed.el
                        [ Ui.centerX
                        , Ui.centerY
                        ]
                        ( String.fromInt model.projectsVersion
                        , case model.lastUpdated of
                            Nothing ->
                                Ui.el
                                    []
                                    (Ui.text "Waiting for info!")

                            Just _ ->
                                Ui.el
                                    []
                                    (Ui.text "Lookin good! 🎉")
                        )

                allWarnings ->
                    viewWarningOverview allWarnings

        _ ->
            viewWarningOverview activeWarnings


viewWarningOverview : List Ports.Warning -> Ui.Element Msg
viewWarningOverview warnings =
    let
        warningCounts =
            List.foldl
                (\warning count ->
                    case warning of
                        Ports.UnusedImport _ ->
                            { count | unusedImports = count.unusedImports + 1 }

                        Ports.UnusedVariable _ ->
                            { count | unusedValues = count.unusedValues + 1 }

                        Ports.MissingAnnotation _ ->
                            { count | missingSignature = count.missingSignature + 1 }
                )
                { unusedImports = 0
                , unusedValues = 0
                , missingSignature = 0
                }
                warnings
    in
    Ui.column [ Ui.space.md, Ui.centerX, Ui.centerY, Ui.width (Ui.px 400) ]
        [ Ui.text "No type errors! But I found a few other things."
        , Ui.column [ Ui.space.md, Ui.pad.xy.lg.zero ]
            [ viewCount "unused imports" warningCounts.unusedImports
            , viewCount "unused values" warningCounts.unusedValues
            , viewCount "missing typesignatures" warningCounts.missingSignature
            ]
        ]


viewCount label amount =
    if amount == 0 then
        Ui.none

    else
        Ui.text (String.fromInt amount ++ " " ++ label)


viewWarningList warnings =
    Ui.column
        [ Ui.space.md
        , Ui.width (Ui.px 700)

        -- , Ui.htmlAttribute (Html.Attributes.style "height" "calc(100% - 50px)")
        , Ui.height Ui.fill
        , Ui.htmlAttribute (Html.Attributes.style "overflow" "auto")
        ]
        (List.map viewWarning warnings)


viewWarning : Ports.Warning -> Ui.Element Msg
viewWarning warning =
    case warning of
        Ports.UnusedVariable unused ->
            Ui.row [ Ui.space.md ]
                [ Ui.text "Unused"
                , Ui.text unused.name
                ]

        Ports.UnusedImport unused ->
            Ui.row [ Ui.space.md ]
                [ Ui.text "Unused"
                , Ui.text unused.name
                ]

        Ports.MissingAnnotation missing ->
            Ui.column
                [ Ui.space.sm
                , Ui.width Ui.fill
                ]
                [ Ui.text "Missing signature"
                , Ui.text (missing.name ++ ": " ++ missing.signature)
                ]


visibleWarnings warnings activeEditor =
    warnings
        |> Dict.get activeEditor.filepath
        |> Maybe.withDefault []


viewGlobalError global =
    Ui.column
        [ Ui.space.md ]
        [ Ui.header.three global.problem.title
        , Ui.column [ Ui.space.md ]
            (List.map viewExpandedText global.problem.message)
        ]


plural : Int -> String -> String -> String
plural int singular pluralForm =
    if int /= 1 then
        pluralForm

    else
        singular


viewLastUpdated : Model -> Ui.Element msg
viewLastUpdated model =
    case ( model.now, model.lastUpdated ) of
        ( Just now, Just lastUpdate ) ->
            Ui.el
                [ Ui.pad.xy.lg.sm
                , Ui.font.dark.light
                , Ui.background.black
                , Ui.rounded.md
                ]
                (Ui.text
                    ("Last updated "
                        ++ Time.Distance.inWords
                            lastUpdate
                            now
                    )
                )

        _ ->
            Ui.none


viewVisible : Model -> Ui.Element msg
viewVisible model =
    Ui.el
        [ Ui.pad.xy.lg.sm
        , Ui.font.dark.light
        , Ui.background.black
        , Ui.rounded.md
        ]
        (Ui.text
            (model.visible
                |> List.filter (String.endsWith ".elm" << .filepath)
                |> List.map Editor.moduleName
                |> String.join ", "
            )
        )


foundErrorsMenu model found =
    let
        fileCount =
            List.length found.errs
    in
    if fileCount == 0 then
        Ui.none

    else
        let
            problemCount =
                List.map (.problem >> List.length) found.errs
                    |> List.sum

            summaryText =
                String.fromInt problemCount
                    ++ " "
                    ++ plural problemCount "error" "errors"
                    ++ " in "
                    ++ String.fromInt fileCount
                    ++ " "
                    ++ plural fileCount "file" "files"
        in
        Ui.row
            [ Ui.alignRight
            , Ui.pad.xy.lg.sm
            , Ui.rounded.md
            , Ui.pointer
            , Ui.space.md
            , Events.onClick (ErrorMenuUpdated (not model.errorMenuVisible))
            , Ui.below
                (viewErrorMenuContent model found)
            ]
            [ Ui.text "▶"
                |> Ui.el
                    [ Ui.font.cyan
                    , Ui.transition
                    , Ui.rotate
                        (if model.errorMenuVisible then
                            (2 * pi) * 0.25

                         else
                            0
                        )
                    ]
            , Ui.text summaryText
            ]


viewErrorMenuContent model found =
    Ui.column
        [ Ui.pad.lg
        , Ui.background.black
        , Ui.border.grey.light
        , Ui.rounded.md
        , Ui.alignRight
        , Ui.space.lg
        , Ui.transition
        , Ui.alpha
            (if model.errorMenuVisible then
                1

             else
                0
            )
        ]
        (List.map
            (viewFileSummary model)
            found.errs
        )


viewMetric name viewer vals =
    case vals of
        [] ->
            Ui.none

        _ ->
            Ui.column [ Ui.space.md, Ui.alignRight, Ui.space.lg ]
                (List.map viewer vals)


isEditorVisible : Elm.ProjectStatus.File -> List Editor.Editor -> Bool
isEditorVisible file visible =
    List.any
        (\e ->
            e.filepath == file.path
        )
        visible


viewFileSummary model file =
    case file.problem of
        [] ->
            Ui.none

        top :: _ ->
            Ui.column
                [ Ui.space.sm
                , Events.onClick (EditorGoTo file.path top.region)
                , Ui.pointer
                ]
                [ Ui.row []
                    [ Ui.text "▶ "
                        |> Ui.el
                            [ Ui.font.cyan
                            , if isEditorVisible file model.visible then
                                Ui.alpha 1

                              else
                                Ui.alpha 0
                            ]
                    , Ui.text "("
                        |> Ui.el [ Ui.font.dark.light ]
                    , Ui.text
                        (String.fromInt (List.length file.problem))
                        |> Ui.el []
                    , Ui.text ") "
                        |> Ui.el [ Ui.font.dark.light ]
                    , Ui.text file.name
                        |> Ui.el []
                    ]
                ]


viewFileErrorDetails : Model -> Elm.ProjectStatus.File -> Ui.Element Msg
viewFileErrorDetails model file =
    Ui.column [ Ui.space.md, Ui.width Ui.fill ]
        (List.map
            (\issue ->
                viewIssueDetails
                    model.errorCodeExpanded
                    (isRegionVisible model.visible file.path issue.region)
                    -- (isCursorPresent model.visible file.path issue.region)
                    (isRegionVisible model.visible file.path issue.region)
                    file
                    issue
            )
            file.problem
        )


isRegionVisible : List Editor.Editor -> String -> Editor.Region -> Bool
isRegionVisible editors path region =
    List.any
        (\e ->
            if e.filepath == path then
                Editor.visible region e.regions

            else
                False
        )
        editors



-- isCursorPresent : List Editor.Editor -> String -> Editor.Region -> Bool
-- isCursorPresent editors path region =
--     List.any
--         (\e ->
--             if e.filepath == path then
--                 Editor.visible region e.selections
--             else
--                 False
--         )
--         editors


viewIssueDetails errorCodeExpanded expanded cursorPresent file issue =
    Ui.Card.view
        { title = String.trim issue.title
        , hint =
            Just <|
                if issue.region.start.line == issue.region.end.line then
                    "line " ++ String.fromInt issue.region.start.line

                else
                    "lines "
                        ++ String.fromInt issue.region.start.line
                        ++ "–"
                        ++ String.fromInt issue.region.end.line
        , highlight = cursorPresent
        , onClick =
            if expanded then
                Nothing

            else
                Just (EditorGoTo file.path issue.region)
        }
        [ if expanded then
            Ui.el
                [ Ui.width Ui.fill
                , if expanded then
                    Ui.htmlAttribute (Html.Attributes.style "transition" "max-height 280ms, opacity 150ms")

                  else
                    Ui.htmlAttribute (Html.Attributes.style "transition" "max-height 200ms, opacity 150ms")
                , if expanded then
                    Ui.htmlAttribute (Html.Attributes.style "overflow" "visible")

                  else
                    Ui.htmlAttribute (Html.Attributes.style "overflow" "hidden")
                , if expanded then
                    Ui.alpha 1

                  else
                    Ui.alpha 0
                , Ui.htmlAttribute
                    (Html.Attributes.style "max-height"
                        (if expanded then
                            "1500px"

                         else
                            "0px"
                        )
                    )
                ]
                (Ui.paragraph
                    [ Ui.pad.xy.zero.lg
                    , Ui.precise
                    ]
                    (List.indexedMap (viewText file issue errorCodeExpanded) issue.message)
                )

          else
            Ui.none
        ]


onlyActiveFile viewing fileIssue =
    (Just fileIssue.path == Maybe.map .filepath viewing)
        || (viewing == Nothing)


onlyAbove maybeViewing iss =
    case maybeViewing of
        Nothing ->
            False

        Just viewing ->
            Editor.above iss.region viewing.ranges


onlyBelow maybeViewing iss =
    case maybeViewing of
        Nothing ->
            False

        Just viewing ->
            Editor.below iss.region viewing.ranges


fillToEighty : String -> String
fillToEighty str =
    let
        fill =
            String.repeat (80 - String.length str) "-"
    in
    str ++ fill


viewText : Elm.ProjectStatus.File -> Elm.ProjectStatus.Problem -> Set Elm.ProjectStatus.CodeReferenceKey -> Int -> Elm.ProjectStatus.Text -> Ui.Element Msg
viewText file problem errorCodeExpanded index txt =
    case txt of
        Elm.ProjectStatus.Plain str ->
            Ui.text str

        Elm.ProjectStatus.Styled styled str ->
            Ui.el
                [ Ui.htmlAttribute (colorAttribute styled.color)
                ]
                (Ui.text str)

        Elm.ProjectStatus.CodeSection lineCount section ->
            viewSection file problem errorCodeExpanded index lineCount section

        Elm.ProjectStatus.CodeQuote lineCount section ->
            viewSection file problem errorCodeExpanded index lineCount section


viewSection file problem errorCodeExpanded index lineCount section =
    if lineCount < 7 then
        Ui.paragraph
            [ Ui.width Ui.fill
            ]
            (List.indexedMap (viewText file problem errorCodeExpanded) section)

    else
        let
            currentKey =
                Elm.ProjectStatus.fileKey file problem index

            expanded =
                Set.member currentKey errorCodeExpanded
        in
        Ui.column [ Ui.width Ui.fill ]
            [ if expanded then
                Ui.row
                    [ Ui.pointer
                    , Events.onClick (ErrorCodeToggled currentKey (not expanded))
                    ]
                    [ Ui.text
                        "▼"
                        |> Ui.el [ Ui.font.cyan ]
                    , Ui.text (" hide code (" ++ String.fromInt lineCount ++ " lines)")
                        |> Ui.el [ Ui.font.dark.light ]
                    ]

              else
                Ui.row
                    [ Ui.pointer
                    , Events.onClick (ErrorCodeToggled currentKey (not expanded))
                    ]
                    [ Ui.text
                        "▶"
                        |> Ui.el [ Ui.font.cyan ]
                    , Ui.text (" show code (" ++ String.fromInt lineCount ++ " lines)\n")
                        |> Ui.el [ Ui.font.dark.light ]
                    ]
            , if expanded then
                Ui.paragraph
                    [ Ui.width Ui.fill
                    , Ui.pad.lg
                    ]
                    (List.indexedMap (viewText file problem errorCodeExpanded) section)

              else
                Ui.none
            ]


viewExpandedText : Elm.ProjectStatus.Text -> Ui.Element msg
viewExpandedText txt =
    case txt of
        Elm.ProjectStatus.Plain str ->
            Ui.text str

        Elm.ProjectStatus.Styled styled str ->
            Ui.el
                [ Ui.htmlAttribute (colorAttribute styled.color)
                ]
                (Ui.text str)

        Elm.ProjectStatus.CodeSection lineCount section ->
            Ui.paragraph
                [ Ui.width Ui.fill
                ]
                (List.map viewExpandedText section)

        Elm.ProjectStatus.CodeQuote lineCount section ->
            Ui.paragraph
                [ Ui.width Ui.fill
                ]
                (List.map viewExpandedText section)


colorAttribute : Maybe Elm.ProjectStatus.Color -> Html.Attribute msg
colorAttribute maybeColor =
    case maybeColor of
        Nothing ->
            Html.Attributes.style "" ""

        Just clr ->
            case clr of
                Elm.ProjectStatus.Yellow ->
                    Html.Attributes.class "warning"

                Elm.ProjectStatus.Red ->
                    Html.Attributes.class "danger"

                Elm.ProjectStatus.Cyan ->
                    Html.Attributes.class "info"

                Elm.ProjectStatus.Green ->
                    Html.Attributes.class "success"
