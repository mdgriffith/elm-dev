## Interactive Examples

Previous architecture was rendering like the following:

```elm
module Dev.Ui.Button exposing (..)

import App.Effect
import App.Page
import App.Shared
import App.Sub
import App.View
import App.View.Id
import Elm
import Elm.Annotation
import Elm.Op
import Html
import Html.Attributes
import Ui
import Ui.Button
import Ui.Events
import Ui.Font
import Ui.Theme.Input


page : App.Page.Page params Model Msg
page =
    App.Page.page
        { init = init
        , update = update
        , view = view
        , subscriptions = \model -> App.Sub.none
        }


type Focus
    = ShowCode
    | ShowOutput


type alias Model =
    { focus_ : Focus
    , selectedExample_ : Int
    , selectedExample__menu : Bool
    , init :
        { withEnabled : Maybe Bool
        , withSecondary : Bool
        , withSmall : Bool
        , text : String
        }
    }


init : params -> shared -> Maybe Model -> App.Page.Init Msg Model
init params shared maybeModel =
    App.Page.init
        (Maybe.withDefault
            { focus_ = ShowOutput
            , selectedExample_ = 0
            , selectedExample__menu = False
            , init =
                { withEnabled = Nothing
                , withSecondary = False
                , withSmall = False
                , text = "Button"
                }
            }
            maybeModel
        )


type Msg
    = Log
    | FocusUpdated Focus
    | SelectedExampleUpdated Int
    | SelectedExampleUpdated_MenuUpdated Bool
    | Init
        { withEnabled : Maybe Bool
        , withSecondary : Bool
        , withSmall : Bool
        , text : String
        }


update : Msg -> Model -> ( Model, App.Effect.Effect Msg )
update msg model =
    case msg of
        Log ->
            ( model, App.Effect.none )

        FocusUpdated newTab ->
            ( { model | focus_ = newTab }, App.Effect.none )

        SelectedExampleUpdated newTab ->
            ( { model | selectedExample_ = newTab }, App.Effect.none )

        SelectedExampleUpdated_MenuUpdated isOpen ->
            ( { model | selectedExample__menu = isOpen }, App.Effect.none )

        Init updated ->
            ( { model | init = updated }, App.Effect.none )


viewCodeOrResult : Focus -> Ui.Element Msg
viewCodeOrResult tab =
    Ui.row
        [ Ui.spacing 8, Ui.paddingXY 32 8, Ui.alignRight ]
        [ Ui.el
            [ Ui.paddingXY 8 4
            , Ui.border 1
            , Ui.rounded 4
            , Ui.pointer
            , Ui.borderColor
                (if tab == ShowOutput then
                    Ui.rgb 1 1 1

                 else
                    Ui.rgb 0 0 0
                )
            , Ui.Events.onClick (FocusUpdated ShowOutput)
            ]
            (Ui.text "Output")
        , Ui.el
            [ Ui.paddingXY 8 4
            , Ui.border 1
            , Ui.rounded 4
            , Ui.pointer
            , Ui.borderColor
                (if tab == ShowCode then
                    Ui.rgb 1 1 1

                 else
                    Ui.rgb 0 0 0
                )
            , Ui.Events.onClick (FocusUpdated ShowCode)
            ]
            (Ui.text "Example")
        ]


view : Model -> App.View.View Msg
view model =
    { title = "Elm Interactive"
    , body =
        Ui.layout
            [ Ui.htmlAttribute
                (Html.Attributes.style "background" "rgb(36,36,36)")
            , Ui.Font.color (Ui.rgb 1 1 1)
            , Ui.inFront (viewCodeOrResult model.focus_)
            , Ui.Font.family [ Ui.Font.typeface "Fira Code", Ui.Font.sansSerif ]
            ]
            (Ui.column
                [ Ui.height Ui.fill, Ui.spacing 16 ]
                [ Ui.el
                    [ Ui.Font.size 24
                    , Ui.paddingXY 32 10
                    , Ui.pointer
                    , Ui.Font.family
                        [ Ui.Font.typeface "Fira Code", Ui.Font.sansSerif ]
                    , Ui.Events.onClick
                        (SelectedExampleUpdated_MenuUpdated
                            (not model.selectedExample__menu)
                        )
                    , if model.selectedExample__menu then
                        Ui.below
                            (Ui.column
                                [ Ui.padding 16
                                , Ui.move (Ui.right 32)
                                , Ui.border 1
                                , Ui.rounded 4
                                , Ui.background (Ui.rgb 0 0 0)
                                , Ui.spacing 8
                                ]
                                [ Ui.el
                                    [ Ui.Events.onClick
                                        (SelectedExampleUpdated 0)
                                    ]
                                    (Ui.text "init")
                                ]
                            )

                      else
                        Ui.pointer
                    ]
                    (Ui.text "▶ Ui.Button")
                , if 0 == model.selectedExample_ then
                    Ui.column
                        [ Ui.height Ui.fill ]
                        [ viewInit model model.init ]

                  else
                    Ui.none
                ]
            )
    }


viewInit :
    Model
    -> { withEnabled : Maybe Bool
    , withSecondary : Bool
    , withSmall : Bool
    , text : String
    }
    -> Ui.Element Msg
viewInit parent model =
    Ui.column
        [ Ui.width Ui.fill, Ui.height Ui.fill ]
        [ if parent.focus_ == ShowOutput then
            Ui.el
                [ Ui.width Ui.fill
                , Ui.Font.color (Ui.rgb 0 0 0)
                , Ui.background (Ui.rgb 1 1 1)
                ]
                (Ui.el
                    [ Ui.padding 32, Ui.height Ui.shrink, Ui.heightMin 200 ]
                    (Ui.el
                        [ Ui.centerY, Ui.centerX ]
                        (Ui.Button.init { onClick = Log, text = model.text }
                            |> (if model.withSmall then
                                    Ui.Button.withSmall

                                else
                                    \a -> a
                               )
                            |> (if model.withSecondary then
                                    Ui.Button.withSecondary

                                else
                                    \a -> a
                               )
                            |> (case model.withEnabled of
                                    Nothing ->
                                        \a -> a

                                    Just withEnabled_option ->
                                        Ui.Button.withEnabled withEnabled_option
                               )
                            |> Ui.Button.view
                        )
                    )
                )

          else
            Ui.el
                [ Ui.padding 32, Ui.height Ui.shrink, Ui.heightMin 200 ]
                (Ui.el
                    [ Ui.centerY ]
                    (Ui.text
                        (Elm.toString
                            (Elm.apply
                              (Elm.value
                                  { importFrom = [ "Ui", "Button" ]
                                  , name = "init"
                                  , annotation = Nothing
                                  }
                              )
                              [ Elm.record
                                  [ ( "onClick"
                                    , Elm.value
                                          { importFrom = []
                                          , name = "Log"
                                          , annotation = Nothing
                                          }
                                    )
                                  , ( "text", Elm.string model.text )
                                  ]
                              ]
                                |> (if model.withSmall then
                                        \a ->
                                            Elm.Op.pipe
                                                (Elm.apply
                                                    (Elm.value
                                                        { importFrom =
                                                            [ "Ui", "Button" ]
                                                        , name = "withSmall"
                                                        , annotation = Nothing
                                                        }
                                                    )
                                                    []
                                                )
                                                a

                                    else
                                        \a -> a
                                   )
                                |> (if model.withSecondary then
                                        \a ->
                                            Elm.Op.pipe
                                                (Elm.apply
                                                    (Elm.value
                                                        { importFrom =
                                                            [ "Ui", "Button" ]
                                                        , name = "withSecondary"
                                                        , annotation = Nothing
                                                        }
                                                    )
                                                    []
                                                )
                                                a

                                    else
                                        \a -> a
                                   )
                                |> (case model.withEnabled of
                                        Nothing ->
                                            \a -> a

                                        Just withEnabled_option ->
                                            \a ->
                                                Elm.Op.pipe
                                                    (Elm.apply
                                                        (Elm.value
                                                            { importFrom =
                                                                [ "Ui"
                                                                , "Button"
                                                                ]
                                                            , name =
                                                                "withEnabled"
                                                            , annotation =
                                                                Nothing
                                                            }
                                                        )
                                                        [ Elm.bool
                                                            withEnabled_option
                                                        ]
                                                    )
                                                    a
                                   )
                                |> (\a ->
                                        Elm.Op.pipe
                                            (Elm.apply
                                                (Elm.value
                                                    { importFrom =
                                                        [ "Ui", "Button" ]
                                                    , name = "view"
                                                    , annotation = Nothing
                                                    }
                                                )
                                                []
                                            )
                                            a
                                   )
                            )
                        )
                    )
                )
        , Ui.el
            [ Ui.width Ui.fill, Ui.padding 32 ]
            (Ui.column
                [ Ui.width Ui.fill, Ui.spacing 16 ]
                [ Ui.Theme.Input.string
                    "text"
                    (\new -> Init { model | text = new })
                    model.text
                , Ui.Theme.Input.bool
                    "Small"
                    (\new -> Init { model | withSmall = new })
                    model.withSmall
                , Ui.Theme.Input.bool
                    "Secondary"
                    (\new -> Init { model | withSecondary = new })
                    model.withSecondary
                , Ui.Theme.Input.maybeBool
                    "Enabled"
                    (\new -> Init { model | withEnabled = new })
                    model.withEnabled
                ]
            )
        ]
```
