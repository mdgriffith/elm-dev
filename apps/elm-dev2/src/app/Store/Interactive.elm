module Store.Interactive exposing
    ( Model
    , Msg(..)
    , get
    , store
    )

{-| Store of interactive controls keyed by filepath.
-}

import App.Store
import Broadcast
import Data.Controls as Controls
import Dict
import Effect
import Listen
import Ui.Interactive.Controls


type alias FilePath =
    String


type alias Model =
    { controlsByFile : Dict.Dict FilePath Controls.Controls
    }


type Msg
    = ControlsUpdated FilePath Controls.Controls
    | PropertyUpdated FilePath String Controls.Value


get : FilePath -> Model -> Maybe Controls.Controls
get filepath model =
    Dict.get filepath model.controlsByFile


store : App.Store.Store Msg Model
store =
    App.Store.store
        { init =
            \_ _ maybeCached ->
                ( maybeCached
                    |> Maybe.withDefault
                        { controlsByFile = Dict.empty
                        }
                , Effect.none
                )
        , update =
            \msg model ->
                case msg of
                    ControlsUpdated filepath controls ->
                        ( Debug.log "ControlsUpdated"
                            { model
                                | controlsByFile =
                                    Dict.insert filepath controls model.controlsByFile
                            }
                        , Effect.none
                        )

                    PropertyUpdated filepath key value ->
                        let
                            updatedDict =
                                case Dict.get filepath model.controlsByFile of
                                    Nothing ->
                                        model.controlsByFile

                                    Just controls ->
                                        Dict.insert filepath (Controls.setValueForPath key value controls) model.controlsByFile
                        in
                        ( Debug.log "PropertyUpdated"
                            { model | controlsByFile = updatedDict }
                        , Ui.Interactive.Controls.propertyUpdated { filepath = filepath, key = key, value = value }
                        )
        , subscriptions =
            \_ ->
                Listen.batch
                    [ Ui.Interactive.Controls.listen (\{ filepath, controls } -> ControlsUpdated filepath controls)
                    , Listen.onBroadcast
                        (\msg ->
                            case msg of
                                Broadcast.InteractivePropertyUpdated { filepath, key, value } ->
                                    Just (PropertyUpdated filepath key value)

                                _ ->
                                    Nothing
                        )
                    ]
        }

