module Store.DevServer exposing
    ( store
    , Model, Msg
    )

{-|

@docs store

@docs Model, Msg

-}

import App.Store
import Effect
import Listen
import Listen.DevServer


type alias Model =
    { base : Maybe String
    , status : Listen.DevServer.ServerStatus
    }


type Msg
    = DevServerReceived Listen.DevServer.Event


store : App.Store.Store Msg Model
store =
    App.Store.store
        { init =
            \_ _ maybeCachedModel ->
                let
                    model =
                        maybeCachedModel
                            |> Maybe.withDefault
                                { base = Nothing
                                , status = Listen.DevServer.Disconnected
                                }
                in
                ( model
                , Effect.none
                )
        , update =
            \msg model ->
                case msg of
                    DevServerReceived event ->
                        case event of
                            Listen.DevServer.ServerStatusUpdated { status } ->
                                case status of
                                    Listen.DevServer.Connected info ->
                                        ( { model
                                            | status = status
                                            , base = Just ("http://" ++ info.host ++ ":" ++ info.port_)
                                          }
                                        , Effect.none
                                        )

                                    Listen.DevServer.Connecting ->
                                        ( { model | status = status, base = Nothing }, Effect.none )

                                    Listen.DevServer.Disconnected ->
                                        ( { model | status = status, base = Nothing }, Effect.none )

                            _ ->
                                ( model, Effect.none )
        , subscriptions =
            \_ ->
                Listen.batch
                    [ Listen.DevServer.listen DevServerReceived
                    ]
        }

