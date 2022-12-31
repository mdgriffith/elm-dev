module Main exposing (main)

import Browser
import Dict
import Editor
import Element as Ui
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import Elm
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode
import Model exposing (..)
import Ports
import Question
import Set exposing (Set)
import Time
import Time.Distance
import Ui


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Ports.incoming Incoming
                    , Time.every 500 CurrentTime
                    ]
        }


init : ( Model, Cmd Msg )
init =
    ( { active = Nothing
      , visible = []
      , projects = []
      , projectsVersion = 0
      , now = Nothing
      , lastUpdated = Nothing
      , viewing = Overview
      , warnings = Dict.empty
      , errorMenuVisible = False
      , errorCodeExpanded = Set.empty
      }
    , Cmd.none
    )


panelLog =
    Debug.log "Panel"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Incoming (Err err) ->
            ( model
            , Cmd.none
            )

        Incoming (Ok editorMsg) ->
            case editorMsg of
                Ports.VisibleEditorsUpdated visible ->
                    let
                        _ =
                            panelLog "VisibleEditorsUpdated"
                    in
                    ( { model
                        | visible = visible.visible
                        , active = visible.active
                      }
                    , Cmd.none
                    )

                Ports.ProjectsStatusUpdated statuses ->
                    let
                        success =
                            Elm.successful statuses

                        _ =
                            panelLog "ProjectsStatusUpdated"
                    in
                    ( { model
                        | projects = statuses
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
                    Debug.log "PANEL: HTTP, Answer error" err
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
        ]
        [ Ui.overrides
        , Ui.layout
            [ Ui.htmlAttribute (Html.Attributes.class "base")
            , Ui.width Ui.fill
            , Ui.height Ui.fill
            ]
            (case model.viewing of
                Overview ->
                    viewOverview model
            )
        ]


viewOverview : Model -> Ui.Element Msg
viewOverview model =
    let
        found =
            List.foldl
                (\project ({ globals, errs } as gathered) ->
                    case project of
                        Elm.NoData ->
                            gathered

                        Elm.Success ->
                            gathered

                        Elm.GlobalError globe ->
                            { globals = globe :: globals
                            , errs = errs
                            }

                        Elm.CompilerError { errors } ->
                            { globals = globals
                            , errs = errors ++ errs
                            }
                )
                { globals = []
                , errs = []
                }
                model.projects

        viewSignatureGroup ( editor, signatures ) =
            Ui.column
                [ Ui.space.md ]
                [ Ui.header.three editor.fileName
                , Ui.el
                    [ Events.onClick (EditorFillTypeSignatures editor.fileName)
                    , Ui.pad.sm
                    , Ui.border.primary
                    , Border.width 1
                    , Ui.rounded.md
                    , Ui.pointer
                    ]
                    (Ui.text "Add all missing typesignatures")
                , Ui.column [ Ui.space.md ]
                    (List.map (viewTypeSignature editor.fileName) signatures)
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
                    [ if signature.region.start.row == signature.region.end.row then
                        Ui.el
                            [ Ui.font.cyan
                            , Ui.alpha 0.5
                            , Ui.width (Ui.px 50)
                            ]
                            (Ui.text (String.fromInt signature.region.start.row))

                      else
                        Ui.row
                            [ Ui.font.cyan
                            , Ui.alpha 0.5
                            , Ui.width (Ui.px 50)
                            ]
                            [ Ui.text (String.fromInt signature.region.start.row)
                            , Ui.text ":"
                            , Ui.text (String.fromInt signature.region.end.row)
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

        -- , Ui.htmlAttribute (Html.Attributes.style "overflow" "auto")
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
        [ Ui.row [ Ui.width Ui.fill ]
            [ Ui.el [ Ui.font.cyan ]
                (Ui.text visibleFileNames)
            , foundErrorsMenu model found
            ]

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
                                viewWarningsOrStatus model

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
        |> Dict.get activeEditor.fileName
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
                |> List.filter (String.endsWith ".elm" << .fileName)
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


isEditorVisible : Elm.File -> List Editor.Editor -> Bool
isEditorVisible file visible =
    List.any
        (\e ->
            e.fileName == file.path
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


viewFileErrorDetails : Model -> Elm.File -> Ui.Element Msg
viewFileErrorDetails model file =
    Ui.column [ Ui.space.md, Ui.width Ui.fill ]
        (List.map
            (\issue ->
                viewIssueDetails
                    model.errorCodeExpanded
                    (isRegionVisible model.visible file.path issue.region)
                    (isCursorPresent model.visible file.path issue.region)
                    file
                    issue
            )
            file.problem
        )


isRegionVisible : List Editor.Editor -> String -> Editor.Region -> Bool
isRegionVisible editors path region =
    List.any
        (\e ->
            if e.fileName == path then
                Editor.visible region e.ranges

            else
                False
        )
        editors


isCursorPresent : List Editor.Editor -> String -> Editor.Region -> Bool
isCursorPresent editors path region =
    List.any
        (\e ->
            if e.fileName == path then
                Editor.visible region e.selections

            else
                False
        )
        editors


viewIssueDetails errorCodeExpanded expanded cursorPresent file issue =
    Ui.row
        [ Ui.width Ui.fill
        , Ui.space.md
        , Ui.pad.xl
        , Ui.rounded.md
        , Ui.background.dark
        , if cursorPresent then
            Ui.border.dark.light

          else
            Ui.border.dark.medium
        , Border.width 1
        ]
        [ Ui.column
            [ if expanded then
                Ui.htmlAttribute (Html.Attributes.class "")

              else
                Events.onClick (EditorGoTo file.path issue.region)
            , if expanded then
                Ui.htmlAttribute (Html.Attributes.class "")

              else
                Ui.pointer
            , Ui.width Ui.fill
            ]
            [ Ui.row [ Ui.space.md, Ui.width Ui.fill ]
                [ Ui.el []
                    (Ui.text (String.trim issue.title))
                , if issue.region.start.row == issue.region.end.row then
                    Ui.el
                        [ Ui.font.dark.light
                        , Ui.alignRight
                        ]
                        (Ui.text ("line " ++ String.fromInt issue.region.start.row))

                  else
                    Ui.row
                        [ Ui.font.dark.light
                        , Ui.alignRight
                        ]
                        [ Ui.text "lines "
                        , Ui.text (String.fromInt issue.region.start.row)
                        , Ui.text "–"
                        , Ui.text (String.fromInt issue.region.end.row)
                        ]
                ]
            , Ui.el
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
            ]
        ]


onlyActiveFile viewing fileIssue =
    (Just fileIssue.path == Maybe.map .fileName viewing)
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


viewText : Elm.File -> Elm.Problem -> Set Elm.CodeReferenceKey -> Int -> Elm.Text -> Ui.Element Msg
viewText file problem errorCodeExpanded index txt =
    case txt of
        Elm.Plain str ->
            Ui.text str

        Elm.Styled styled str ->
            Ui.el
                [ Ui.htmlAttribute (colorAttribute styled.color)
                ]
                (Ui.text str)

        Elm.CodeSection lineCount section ->
            viewSection file problem errorCodeExpanded index lineCount section

        Elm.CodeQuote lineCount section ->
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
                Elm.fileKey file problem index

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


viewExpandedText : Elm.Text -> Ui.Element msg
viewExpandedText txt =
    case txt of
        Elm.Plain str ->
            Ui.text str

        Elm.Styled styled str ->
            Ui.el
                [ Ui.htmlAttribute (colorAttribute styled.color)
                ]
                (Ui.text str)

        Elm.CodeSection lineCount section ->
            Ui.paragraph
                [ Ui.width Ui.fill
                ]
                (List.map viewExpandedText section)

        Elm.CodeQuote lineCount section ->
            Ui.paragraph
                [ Ui.width Ui.fill
                ]
                (List.map viewExpandedText section)


colorAttribute : Maybe Elm.Color -> Html.Attribute msg
colorAttribute maybeColor =
    case maybeColor of
        Nothing ->
            Html.Attributes.style "" ""

        Just clr ->
            case clr of
                Elm.Yellow ->
                    Html.Attributes.class "warning"

                Elm.Red ->
                    Html.Attributes.class "danger"

                Elm.Cyan ->
                    Html.Attributes.class "info"

                Elm.Green ->
                    Html.Attributes.class "success"
