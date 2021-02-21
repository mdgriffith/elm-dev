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
      , workspace = []
      , diagnostics = Elm.NoData
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
                Ports.VisiblEditorsUpdated visible ->
                    ( { model | visible = visible }
                    , Cmd.none
                    )

                Ports.VisibleRangeUpdated viewed ->
                    let
                        updateSelection editor =
                            if viewed.fileName == editor.fileName then
                                { editor
                                    | ranges = viewed.ranges
                                }

                            else
                                editor

                        newViewing =
                            Maybe.map updateSelection model.active
                    in
                    ( { model
                        | active = newViewing
                        , visible = List.map updateSelection model.visible
                      }
                    , Cmd.none
                    )

                Ports.SelectionUpdated current ->
                    let
                        updateSelection editor =
                            if current.fileName == editor.fileName then
                                { editor
                                    | selections = current.selections
                                }

                            else
                                editor

                        newViewing =
                            Maybe.map updateSelection model.active
                    in
                    ( { model
                        | active = newViewing
                        , visible = List.map updateSelection model.visible
                      }
                    , Cmd.none
                    )

                Ports.ActiveEditorUpdated current ->
                    ( { model | active = Just current }
                    , Cmd.none
                    )

                Ports.Errors diags ->
                    ( { model | diagnostics = diags }
                    , Cmd.none
                    )

                Ports.WorkspaceFolders newFolders ->
                    ( { model | workspace = newFolders }
                    , Cmd.none
                    )

        GoTo file problem ->
            ( model
            , Ports.outgoing
                (Ports.Goto
                    { file = file.path
                    , region = problem.region
                    }
                )
            )


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



"""


view model =
    { title = "Elm Live Errors"
    , body =
        Html.node "style" [] [ Html.text styleSheet ]
            :: viewEditorFocusToken model.active
            :: viewErrorCountHints model.active model.diagnostics
            :: viewError model.active model.visible model.diagnostics
    }


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


viewError active visible error =
    case error of
        Elm.NoData ->
            [ Html.div
                [ Html.Attributes.style "white-space" "pre"
                ]
                [ Html.text "No data"
                ]
            ]

        Elm.Success ->
            [ Html.div
                [ Html.Attributes.style "white-space" "pre"
                ]
                [ Html.span
                    [ Html.Attributes.style "color" "green"
                    ]
                    [ Html.text "  ✓" ]
                , Html.text " Success!"
                ]
            ]

        Elm.GlobalError err ->
            let
                shortMarkupName =
                    err.path
                        |> String.split "/"
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault "Unknown"
            in
            [ viewIssue active err.problem ]

        Elm.CompilerError { errors } ->
            let
                errorsForFile =
                    List.filter (\err -> List.any (Elm.inEditor err) visible) errors
            in
            case errorsForFile of
                [] ->
                    [ viewOverview errors ]

                _ ->
                    List.concatMap (viewFileIssue active visible) errorsForFile
                        ++ [ Html.div [ Html.Attributes.style "height" "100px" ] [] ]


viewOverview errors =
    let
        fileOverview file =
            Html.div
                []
                [ Html.text (file.name ++ ".elm")

                -- , Html.div []
                --     (file.problem
                --         |> List.foldl countTitleOccurances Dict.empty
                --         |> Dict.toList
                --         |> List.map renderCount
                --     )
                , Html.div []
                    (List.map (viewProblemDetails file) file.problem)
                ]

        renderCount ( title, count ) =
            Html.div [ Html.Attributes.style "padding-left" "12px" ]
                [ Html.span [] [ Html.text ("(" ++ String.fromInt count ++ ") ") ]
                , Html.span [] [ Html.text title ]
                ]

        countTitleOccurances prob dict =
            Dict.update prob.title
                (\value ->
                    value
                        |> Maybe.map ((+) 1)
                        |> Maybe.withDefault 0
                        |> Just
                )
                dict

        viewProblemOverview problem =
            Html.div
                []
                [ Html.text problem.title
                ]

        overviewTitle =
            Html.h3 []
                [ Html.text "Overview of Errors"
                ]
    in
    Html.div
        [ Html.Attributes.style "width" "70%"
        , Html.Attributes.style "margin-bottom" "130px"
        ]
        (overviewTitle :: List.map fileOverview errors)


viewFileIssue active visible fileIssue =
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
    -- if Editor.visible iss.region visibleRanges then
    Html.div
        [ Html.Attributes.style "white-space" "pre"
        , Html.Events.onClick (GoTo file issue)
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ Html.div [ Html.Attributes.style "color" "cyan" ]
            [ Html.text (fillToEighty ("-- " ++ String.toUpper issue.title ++ " ")) ]
        , Html.div []
            (List.map viewText issue.message)
        ]


viewProblemPreview file issue =
    Html.div
        [ Html.Events.onClick (GoTo file issue)
        , Html.Attributes.style "cursor" "pointer"
        ]
        [ Html.div [] [ Html.text issue.title ]
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
