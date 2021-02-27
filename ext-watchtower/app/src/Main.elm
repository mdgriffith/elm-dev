module Main exposing (main)

import Browser
import Dict
import Editor
import Element
import Element.Font as Font
import Elm
import Html
import Html.Attributes
import Html.Events
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode
import Model exposing (..)
import Ports


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
                    , Cmd.none
                    )

        EditorGoTo file problem ->
            ( model
            , Ports.outgoing
                (Ports.Goto
                    { file = file.path
                    , region = problem.region
                    }
                )
            )

        View viewing ->
            ( { model | viewing = viewing }
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


styleSheet =
    """


html {
    min-height:100%;
}
body {
    background-color: var(--vscode-editor-background);
    color: var(--vscode-editor-foreground);
    /*font-family: "Fira Code" !important; */
    font-family: var(--vscode-editor-font-family);
    font-weight: var(--vscode-editor-font-weight);
    font-size: var(--vscode-editor-font-size);
    margin: 0;
    padding: 0 20px;
    min-height: 100vh;
    display:flex;
    flex-direction: column;
    justify-content: center;
    align-items: flex-start;
}

@keyframes blink {
  from {opacity: 1;}
  50%  {opacity: 0.2;}
  100% {opacity: 1;}
}


.info {
    color: var(--vscode-editorInfo-foreground);
}

.warning {
    color: var(--vscode-editorWarning-foreground);
}

.danger {
    color: var(--vscode-editorError-foreground);
}

.success {
    color: var(--vscode-testing-iconPassed);
}

.blink {
    opacity:1;
    animation: blink 250ms linear;
}



"""


view model =
    { title = "Elm Live Errors"
    , body =
        [ Html.node "style" [] [ Html.text styleSheet ]
        , case model.viewing of
            Overview ->
                viewOverview model

            ViewingFile path ->
                Html.div [] (viewFile path model)
        ]

    -- case model.projects of
    --     [] ->
    --         Html.node "style" [] [ Html.text styleSheet ]
    --             :: viewEditorFocusToken model.active
    --             :: [ Html.div [] [ Html.text "No projects" ] ]
    --     status :: _ ->
    --         Html.node "style" [] [ Html.text styleSheet ]
    --             :: viewEditorFocusToken model.active
    --             :: viewErrorCountHints model.active status
    --             :: viewError model.active model.visible status
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

        viewProblemOverview problem =
            Html.div
                []
                [ Html.text problem.title
                ]

        viewFileOverview file =
            Html.div
                []
                [ Html.text (file.name ++ ".elm")
                , Html.div []
                    (List.map
                        (\issue ->
                            viewIssueDetails
                                (isVisible model.visible file issue)
                                file
                                issue
                        )
                        file.problem
                    )
                ]

        viewGlobalError global =
            Html.div
                []
                [ Html.h3 [] [ Html.text global.problem.title ]
                , Html.div []
                    (List.map viewText global.problem.message)
                ]
    in
    Html.div
        [ Html.Attributes.style "width" "70%"
        , Html.Attributes.style "margin-bottom" "130px"
        ]
        [ Html.h2 []
            [ Html.text "Overview"
            ]
        , if List.isEmpty found.globals && List.isEmpty found.errs then
            Keyed.node "div"
                []
                [ ( String.fromInt model.projectsVersion
                  , Html.div
                        [ Html.Attributes.class "blink"
                        ]
                        [ Html.text "No errors 🎉"
                        , Html.text (String.fromInt model.projectsVersion)
                        ]
                  )
                ]

          else
            Html.text ""
        , if List.isEmpty found.globals then
            Html.text ""

          else
            Html.h3 []
                [ Html.text "Global"
                ]
        , Html.div []
            (List.map viewGlobalError found.globals)
        , if List.isEmpty found.errs then
            Html.text ""

          else
            Html.h3 []
                [ Html.text "Errors"
                ]
        , Html.div [] (List.map viewFileOverview found.errs)
        ]


isVisible : List Editor.Editor -> Elm.File -> Elm.Problem -> Bool
isVisible editors file prob =
    List.any
        (\e ->
            if e.fileName == file.path then
                Editor.visible prob.region e.ranges

            else
                False
        )
        editors


viewIssueDetails expanded file issue =
    Html.div
        [ Html.Events.onClick (EditorGoTo file issue)
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ Html.div []
            [ if issue.region.start.row == issue.region.end.row then
                Html.span
                    [ Html.Attributes.style "color" "cyan"
                    , Html.Attributes.style "opacity" "0.5"
                    , Html.Attributes.style "width" "50px"
                    , Html.Attributes.style "display" "inline-block"
                    ]
                    [ Html.text (String.fromInt issue.region.start.row)
                    ]

              else
                Html.span
                    [ Html.Attributes.style "color" "cyan"
                    , Html.Attributes.style "opacity" "0.5"
                    , Html.Attributes.style "width" "50px"
                    , Html.Attributes.style "display" "inline-block"
                    ]
                    [ Html.text (String.fromInt issue.region.start.row)
                    , Html.text ":"
                    , Html.text (String.fromInt issue.region.end.row)
                    ]
            , Html.span [ Html.Attributes.style "color" "cyan" ]
                [ Html.text issue.title
                ]
            ]
        , if expanded then
            Html.div
                [ Html.Attributes.style "padding-left" "100px"
                , Html.Attributes.style "white-space" "pre"
                ]
                (List.map viewText issue.message)

          else
            Html.text ""
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
        ++ [ Html.div [ Html.Attributes.style "height" "100px" ] [] ]


viewEditorFocusToken viewing =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "padding" "4px"
        , Html.Attributes.style "border" "1px solid red"
        , Html.Attributes.style "right" "0"
        ]
        [ case viewing of
            Nothing ->
                Html.text "No file detected."

            Just selected ->
                Html.div []
                    [ Html.div []
                        [ Html.text "Watchtower: "
                        , Html.text
                            (viewFileName selected.fileName)
                        ]

                    -- , Html.div []
                    --     (List.map viewSelection selected.selections)
                    -- , Html.div []
                    --     (List.map viewRange selected.visible)
                    ]
        ]


viewEditorFocus viewing =
    case viewing of
        Nothing ->
            Html.text "No file detected."

        Just selected ->
            Html.div []
                [ Html.div []
                    [ Html.text
                        ("file: " ++ viewFileName selected.fileName)
                    ]
                , Html.div []
                    (List.map viewSelection selected.selections)
                , Html.div []
                    (List.map viewRange selected.ranges)
                ]


viewErrorCountHints viewing error =
    case error of
        Elm.NoData ->
            Html.text ""

        Elm.Success ->
            Html.text ""

        Elm.GlobalError err ->
            Html.text ""

        Elm.CompilerError { errors } ->
            let
                errorsForFile =
                    List.filter (onlyActiveFile viewing) errors
            in
            case errorsForFile of
                [] ->
                    Html.text ""

                _ ->
                    let
                        countsAbove =
                            errorsForFile
                                |> List.concatMap .problem
                                |> List.filter (onlyAbove viewing)
                                |> List.length

                        countsBelow =
                            errorsForFile
                                |> List.concatMap .problem
                                |> List.filter (onlyBelow viewing)
                                |> List.length
                    in
                    Html.div
                        [ Html.Attributes.style "position" "fixed"
                        , Html.Attributes.style "pointer-events" "none"
                        , Html.Attributes.style "height"
                            "100%"
                        , Html.Attributes.style
                            "width"
                            "100%"
                        ]
                        [ Html.div
                            [ Html.Attributes.style "position" "absolute"
                            , Html.Attributes.style "left" "0"
                            , Html.Attributes.style "top" "0"
                            , Html.Attributes.style "padding-left" "24px"
                            , Html.Attributes.style "color" "red"
                            ]
                            [ Html.text ("↑ " ++ String.fromInt countsAbove ++ " errors") ]
                            |> viewIf (countsAbove /= 0)
                        , Html.div
                            [ Html.Attributes.style "position" "absolute"
                            , Html.Attributes.style "left" "0"
                            , Html.Attributes.style "bottom" "0"
                            , Html.Attributes.style "padding-left" "24px"
                            , Html.Attributes.style "color" "red"
                            ]
                            [ Html.text ("↓ " ++ String.fromInt countsBelow ++ " errors") ]
                            |> viewIf (countsBelow /= 0)
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


viewIf condition html =
    if condition then
        html

    else
        Html.text ""


viewFileIssue fileIssue =
    List.map (viewProblemDetails fileIssue)
        fileIssue.problem


viewIssue viewing iss =
    Html.div [ Html.Attributes.style "white-space" "pre" ]
        [ Html.div [ Html.Attributes.class "info" ]
            [ Html.text (fillToEighty ("-- " ++ String.toUpper iss.title ++ " ")) ]
        , Html.div []
            (List.map viewText iss.message)
        ]


viewProblemDetails file issue =
    Html.div
        [ Html.Attributes.style "white-space" "pre"
        , Html.Events.onClick (EditorGoTo file issue)
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ Html.div [ Html.Attributes.style "color" "cyan" ]
            [ Html.text (fillToEighty ("-- " ++ String.toUpper issue.title ++ " ")) ]
        , Html.div []
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
            Html.text str

        Elm.Styled styled ->
            Html.span
                [ colorAttribute styled.color
                ]
                [ Html.text styled.string ]


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
    Html.div []
        [ Html.div []
            [ Html.text "start"
            , viewPos sel.start
            ]
        , Html.div []
            [ Html.text "end"
            , viewPos sel.end
            ]
        ]


viewSelection sel =
    Html.div []
        [ Html.div []
            [ Html.text "anchor"
            , viewPos sel.anchor
            ]
        , Html.div []
            [ Html.text "active"
            , viewPos sel.active
            ]
        ]


viewPos { row, col } =
    Html.div []
        [ Html.div [] [ Html.text ("row: " ++ String.fromInt row) ]
        , Html.div [] [ Html.text ("col: " ++ String.fromInt col) ]
        ]


viewFileName name =
    String.split "/" name
        |> List.reverse
        |> List.head
        |> Maybe.withDefault "Empty File"
