port module App exposing (Prog, element)

{-| -}

import Browser
import Html exposing (Html)


port unmount : (() -> msg) -> Sub msg


type Model internalModel
    = Mounted internalModel
    | Unmounted


type Msg internalMsg
    = Unmount
    | App internalMsg


type alias Prog flags model msg =
    Program flags (Model model) (Msg msg)


element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Prog flags model msg
element config =
    Browser.element
        { init =
            \flags ->
                let
                    ( internalModel, internalCmd ) =
                        config.init flags
                in
                ( Mounted internalModel
                , Cmd.map App internalCmd
                )
        , update =
            \msg model ->
                case model of
                    Mounted internalModel ->
                        case msg of
                            Unmount ->
                                ( Unmounted
                                , Cmd.none
                                )

                            App internalMsg ->
                                let
                                    ( newInternalModel, internalCmd ) =
                                        config.update internalMsg internalModel
                                in
                                ( Mounted newInternalModel
                                , Cmd.map App internalCmd
                                )

                    Unmounted ->
                        ( Unmounted, Cmd.none )
        , subscriptions =
            \model ->
                case model of
                    Mounted internalModel ->
                        Sub.batch
                            [ unmount (\_ -> Unmount)
                            , Sub.map App (config.subscriptions internalModel)
                            ]

                    Unmounted ->
                        Sub.none
        , view =
            \model ->
                case model of
                    Mounted internalModel ->
                        Html.map App (config.view internalModel)

                    Unmounted ->
                        Html.text ""
        }
