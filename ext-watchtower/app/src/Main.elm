module Main exposing (main)

import Browser
import Dict
import Editor
import Element as Ui
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
                    , Cmd.none
                    )

                Ports.ProjectsStatusUpdated statuses ->
                    ( { model
                        | projects = statuses
                        , projectsVersion = model.projectsVersion + 1
                      }
                    , if Elm.successful statuses then
                        case model.active of
                            Nothing ->
                                Cmd.none

                            Just active ->
                                Question.ask.missingTypesignatures active.fileName
                                    |> Cmd.map AnswerReceived

                      else
                        Cmd.none
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
        , Ui.layout [ Ui.htmlAttribute (Html.Attributes.class "base") ] <|
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

        viewFileOverview file =
            Ui.column [ Ui.space.md ]
                [ Ui.text (file.name ++ ".elm")
                , Ui.column [ Ui.space.md ]
                    (List.map
                        (\issue ->
                            viewIssueDetails
                                (isVisible model.visible file.path issue.region)
                                file
                                issue
                        )
                        file.problem
                    )
                ]

        viewGlobalError global =
            Ui.column
                [ Ui.space.md ]
                [ Ui.header.three global.problem.title
                , Ui.column [ Ui.space.md ]
                    (List.map viewText global.problem.message)
                ]

        ( missingFile, missing ) =
            case model.active of
                Nothing ->
                    ( "", [] )

                Just active ->
                    ( active.fileName
                    , Dict.get active.fileName model.missingTypesignatures
                        |> Maybe.withDefault []
                    )

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
    in
    Ui.column
        [ Ui.space.lg
        , Ui.centerX
        , Ui.centerY
        , Ui.width Ui.fill
        , Ui.pad.xl
        ]
        [ Ui.header.two "Overview"
        , Keyed.el []
            ( String.fromInt model.projectsVersion
            , Ui.el
                [ Ui.anim.blink
                ]
                (Ui.text "No errors 🎉")
            )
            |> Ui.when (List.isEmpty found.globals && List.isEmpty found.errs)
        , viewMetric "Missing typesignatures"
            (Just
                { text = "Insert all missing signatures"
                , msg = EditorFillTypeSignatures missingFile
                }
            )
            (viewTypeSignature missingFile)
            missing
        , viewMetric "Global" Nothing viewGlobalError found.globals
        , viewMetric "Errors" Nothing viewFileOverview found.errs
        ]


viewMetric name maybeAction viewer vals =
    case vals of
        [] ->
            Ui.none

        _ ->
            Ui.column [ Ui.space.lg ]
                [ Ui.header.three name
                    |> Ui.when (not (List.isEmpty vals))
                , case maybeAction of
                    Nothing ->
                        Ui.none

                    Just action ->
                        Ui.el
                            [ Events.onClick action.msg
                            , Ui.pad.sm
                            , Ui.border.primary
                            , Ui.pointer
                            ]
                            (Ui.text action.text)
                , Ui.column [ Ui.space.lg ]
                    (List.map viewer vals)
                ]


isVisible : List Editor.Editor -> String -> Editor.Region -> Bool
isVisible editors path region =
    List.any
        (\e ->
            if e.fileName == path then
                Editor.visible region e.ranges

            else
                False
        )
        editors


viewIssueDetails expanded file issue =
    Ui.column
        [ Events.onClick (EditorGoTo file.path issue.region)
        , Ui.pointer
        , Ui.space.lg
        ]
        [ Ui.row []
            [ if issue.region.start.row == issue.region.end.row then
                Ui.el
                    [ Ui.font.cyan
                    , Ui.alpha 0.5
                    , Ui.width (Ui.px 50)
                    ]
                    (Ui.text (String.fromInt issue.region.start.row))

              else
                Ui.row
                    [ Ui.font.cyan
                    , Ui.alpha 0.5
                    , Ui.width (Ui.px 50)
                    ]
                    [ Ui.text (String.fromInt issue.region.start.row)
                    , Ui.text ":"
                    , Ui.text (String.fromInt issue.region.end.row)
                    ]
            , Ui.el [ Ui.font.cyan ]
                (Ui.text issue.title)
            ]
        , if expanded then
            Ui.paragraph
                [ Ui.pad.xy.xl.sm
                , Ui.precise
                ]
                (List.map viewText issue.message)

          else
            Ui.none
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


viewIf condition html =
    if condition then
        html

    else
        Ui.text ""


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


viewText txt =
    case txt of
        Elm.Plain str ->
            Ui.text str

        Elm.Styled styled ->
            Ui.el
                [ Ui.htmlAttribute (colorAttribute styled.color)
                ]
                (Ui.text styled.string)


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
