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
import Ui


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Ports.incoming Incoming
                    ]
        }


init =
    ( { active = Nothing
      , visible = []
      , projects = []
      , projectsVersion = 0
      , viewing = Overview
      , missingTypesignatures = Dict.empty
      , errorMenuVisible = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Message" msg of
        Incoming (Err err) ->
            ( model
            , Cmd.none
            )

        Incoming (Ok editorMsg) ->
            case editorMsg of
                Ports.VisibleEditorsUpdated visible ->
                    ( { model
                        | visible = visible.visible
                        , active = visible.active
                      }
                    , if Elm.successful model.projects then
                        Cmd.batch
                            (visible.visible
                                |> List.filterMap
                                    (\editor ->
                                        if editor.unsavedChanges || Dict.member editor.fileName model.missingTypesignatures then
                                            Nothing

                                        else
                                            editor.fileName
                                                |> Question.ask.missingTypesignatures
                                                |> Cmd.map AnswerReceived
                                                |> Just
                                    )
                            )

                      else
                        Cmd.none
                    )

                Ports.ProjectsStatusUpdated statuses ->
                    let
                        success =
                            Elm.successful statuses
                    in
                    ( { model
                        | projects = statuses
                        , projectsVersion = model.projectsVersion + 1
                        , missingTypesignatures =
                            if success then
                                model.missingTypesignatures

                            else
                                Dict.empty
                      }
                    , if success then
                        Cmd.batch
                            (model.visible
                                |> List.filterMap
                                    (\editor ->
                                        if editor.unsavedChanges || Dict.member editor.fileName model.missingTypesignatures then
                                            Nothing

                                        else
                                            editor.fileName
                                                |> Question.ask.missingTypesignatures
                                                |> Cmd.map AnswerReceived
                                                |> Just
                                    )
                            )

                      else
                        Cmd.none
                    )

        ErrorMenuUpdated new ->
            ( { model | errorMenuVisible = new }
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

        AnswerReceived (Err err) ->
            let
                _ =
                    Debug.log "HTTP, Answer error" err
            in
            ( model
            , Cmd.none
            )

        AnswerReceived (Ok answer) ->
            case answer of
                Question.MissingTypeSignatures path missing ->
                    ( { model
                        | missingTypesignatures =
                            model.missingTypesignatures
                                |> Dict.insert path
                                    (missing
                                        |> List.sortBy
                                            (\signature ->
                                                signature.region.start.row
                                            )
                                    )
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


view model =
    { title = "Watchtower"
    , body =
        [ Ui.overrides
        , Ui.layout
            [ Ui.htmlAttribute (Html.Attributes.class "base")
            , Ui.width Ui.fill
            , Ui.height Ui.fill
            ]
          <|
            case model.viewing of
                Overview ->
                    viewOverview model
        ]
    }


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

        viewGlobalError global =
            Ui.column
                [ Ui.space.md ]
                [ Ui.header.three global.problem.title
                , Ui.column [ Ui.space.md ]
                    (List.map viewText global.problem.message)
                ]

        missing =
            List.filterMap
                (\editor ->
                    case Dict.get editor.fileName model.missingTypesignatures of
                        Nothing ->
                            Nothing

                        Just signatures ->
                            Just
                                ( editor
                                , signatures
                                )
                )
                model.visible

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
        ]
        [ Ui.row [ Ui.width Ui.fill ]
            [ Ui.el [ Ui.font.cyan ]
                (Ui.text visibleFileNames)
            , foundErrorsMenu model found
            ]
        , Keyed.el []
            ( String.fromInt model.projectsVersion
            , Ui.el
                [ Ui.anim.blink
                ]
                (Ui.text "No errors 🎉")
            )
            |> Ui.when
                (List.isEmpty found.globals
                    && List.isEmpty found.errs
                    && List.isEmpty missing
                )

        -- , viewMetric "Missing typesignatures"
        --     viewSignatureGroup
        --     missing
        -- ,
        , viewMetric
            "Global"
            viewGlobalError
            found.globals
        , case visibleErrors of
            [] ->
                case notVisibleErrors of
                    [] ->
                        Ui.none

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
                    , Ui.htmlAttribute (Html.Attributes.style "height" "calc(100% - 50px)")
                    , Ui.htmlAttribute (Html.Attributes.style "overflow" "auto")
                    ]
                    (List.map (viewFileErrorDetails model) visibleErrors)
        ]


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
                if fileCount == 1 then
                    String.fromInt problemCount ++ " errors in " ++ String.fromInt fileCount ++ " file"

                else
                    String.fromInt problemCount ++ " errors in " ++ String.fromInt fileCount ++ " files"
        in
        Ui.row
            [ Ui.alignRight
            , Ui.pad.xy.lg.sm
            , Ui.rounded.md
            , Ui.pointer
            , Events.onClick (ErrorMenuUpdated (not model.errorMenuVisible))
            , Ui.below
                (viewErrorMenuContent model found)
            ]
            [ Ui.text summaryText
            ]


viewErrorMenuContent model found =
    Ui.column
        [ Ui.pad.lg
        , Ui.background.dark
        , Ui.rounded.md
        , Ui.alignRight
        , Ui.space.lg
        , Ui.htmlAttribute (Html.Attributes.style "transition" "opacity 100ms")
        , Ui.alpha 1
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


viewIssueDetails expanded cursorPresent file issue =
    Ui.row
        [ Ui.width Ui.fill
        , Ui.space.md
        , Ui.pad.xl
        , Ui.rounded.md
        , Ui.background.dark
        , if cursorPresent then
            Ui.border.light

          else
            Ui.border.dark.medium
        , Border.width 1
        ]
        [ Ui.column
            [ Events.onClick (EditorGoTo file.path issue.region)
            , Ui.pointer
            , Ui.space.lg
            , Ui.width Ui.fill
            ]
            [ Ui.row [ Ui.space.md ]
                [ Ui.el []
                    (Ui.text (String.trim issue.title))
                , if issue.region.start.row == issue.region.end.row then
                    Ui.el
                        [ Ui.font.cyan
                        ]
                        (Ui.text (String.fromInt issue.region.start.row))

                  else
                    Ui.row
                        [ Ui.font.cyan
                        ]
                        [ Ui.text (String.fromInt issue.region.start.row)
                        , Ui.text ":"
                        , Ui.text (String.fromInt issue.region.end.row)
                        ]
                ]
            , Ui.el
                [ Ui.width Ui.fill
                , if expanded then
                    Ui.htmlAttribute (Html.Attributes.style "transition" "max-height 250ms")

                  else
                    Ui.htmlAttribute (Html.Attributes.style "transition" "max-height 100ms")
                , Ui.htmlAttribute (Html.Attributes.style "overflow" "auto")
                , Ui.htmlAttribute
                    (Html.Attributes.style "max-height"
                        (if expanded then
                            "700px"

                         else
                            "0px"
                        )
                    )
                , Ui.htmlAttribute
                    (if expanded then
                        Html.Attributes.class ""

                     else
                        Html.Attributes.style "margin"
                            "0px"
                    )
                ]
              <|
                Ui.paragraph
                    [ Ui.pad.xy.zero.sm
                    , Ui.precise
                    ]
                    (List.map viewText issue.message)
            ]
        ]


viewFile path model =
    let
        foundErrs =
            List.foldl
                (\project ({ handled, errs } as gathered) ->
                    if handled then
                        gathered

                    else
                        case project of
                            Elm.NoData ->
                                gathered

                            Elm.Success ->
                                gathered

                            Elm.GlobalError globe ->
                                gathered

                            Elm.CompilerError { errors } ->
                                let
                                    newErrs =
                                        List.filter
                                            (\e ->
                                                path == e.path
                                            )
                                            errors
                                in
                                case newErrs of
                                    [] ->
                                        gathered

                                    _ ->
                                        { handled = True
                                        , errs = newErrs ++ errs
                                        }
                )
                { handled = False
                , errs = []
                }
                model.projects
                |> .errs
    in
    List.concatMap viewFileIssue foundErrs


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


viewFileIssue fileIssue =
    List.map (viewProblemDetails fileIssue)
        fileIssue.problem


viewIssue viewing iss =
    Ui.column [ Ui.precise ]
        [ Ui.column [ Ui.font.info ]
            [ Ui.text (fillToEighty ("-- " ++ String.toUpper iss.title ++ " ")) ]
        , Ui.column []
            (List.map viewText iss.message)
        ]


viewProblemDetails file issue =
    Ui.column
        [ Ui.precise
        , Events.onClick (EditorGoTo file.path issue.region)
        , Ui.pointer
        ]
        [ Ui.column [ Ui.font.cyan ]
            [ Ui.text (fillToEighty ("-- " ++ String.toUpper issue.title ++ " ")) ]
        , Ui.column []
            (List.map viewText issue.message)
        ]


fillToEighty str =
    let
        fill =
            String.repeat (80 - String.length str) "-"
    in
    str ++ fill


viewText : Elm.Text -> Ui.Element msg
viewText txt =
    case txt of
        Elm.Plain str ->
            Ui.text str

        Elm.Styled styled str ->
            Ui.el
                [ Ui.htmlAttribute (colorAttribute styled.color)
                ]
                (Ui.text str)

        Elm.CodeSection section ->
            Ui.paragraph
                [ Ui.width Ui.fill
                , Ui.htmlAttribute (Html.Attributes.style "overflow" "auto")
                , Ui.htmlAttribute
                    (Html.Attributes.style "max-height"
                        "150px"
                    )
                , Ui.htmlAttribute
                    (Html.Attributes.style "max-width"
                        "600px"
                    )
                ]
                (List.map viewText section)

        -- Ui.el [ Ui.font.cyan ] (Ui.text "\n◀ see editor")
        Elm.CodeQuote section ->
            Ui.none


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


viewRange sel =
    Ui.column []
        [ Ui.column []
            [ Ui.text "start"
            , viewPos sel.start
            ]
        , Ui.column []
            [ Ui.text "end"
            , viewPos sel.end
            ]
        ]


viewSelection sel =
    Ui.column [ Ui.space.md ]
        [ Ui.column []
            [ Ui.text "anchor"
            , viewPos sel.anchor
            ]
        , Ui.column []
            [ Ui.text "active"
            , viewPos sel.active
            ]
        ]


viewPos { row, col } =
    Ui.column [ Ui.space.md ]
        [ Ui.text ("row: " ++ String.fromInt row)
        , Ui.text ("col: " ++ String.fromInt col)
        ]


viewFileName name =
    String.split "/" name
        |> List.reverse
        |> List.head
        |> Maybe.withDefault "Empty File"
