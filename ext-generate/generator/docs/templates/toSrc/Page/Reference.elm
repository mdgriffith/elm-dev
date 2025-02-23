module Page.Reference exposing (page, Model, Msg)

{-|

@docs page, Model, Msg

-}

import App.Page
import App.Page.Id
import App.Resources
import App.View
import App.View.Id
import Broadcast
import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes as Attr
import Listen exposing (Listen)
import Ref
import Theme
import Theme.Color
import Ui.Attr
import Ui.Module


{-| -}
type alias Model =
    { references : List Ref.Ref
    }


{-| -}
type Msg
    = RemoveById Ref.Id
    | RefAdded Ref.Ref


page : App.Page.Page App.Resources.Resources App.Page.Id.Reference_Params Msg Model
page =
    App.Page.page
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init :
    App.Page.Id.Id
    -> App.Page.Id.Reference_Params
    -> App.Resources.Resources
    -> Maybe Model
    -> App.Page.Init Msg Model
init pageId params shared maybeCached =
    case maybeCached of
        Nothing ->
            App.Page.init
                { references = []
                }

        Just model ->
            App.Page.init model


update : App.Resources.Resources -> Msg -> Model -> ( Model, Effect Msg )
update resources msg model =
    case msg of
        RefAdded ref ->
            ( { model
                | references = ref :: model.references
              }
            , Effect.none
            )

        RemoveById removedId ->
            ( { model
                | references =
                    List.filter (\{ id } -> id /= removedId)
                        model.references
              }
            , Effect.none
            )


subscriptions : App.Resources.Resources -> Model -> Listen Msg
subscriptions resources model =
    Listen.onBroadcast
        (\broadcastMsg ->
            case broadcastMsg of
                Broadcast.RefPinned ref ->
                    Just (RefAdded ref)
        )


view : App.View.Id.Id -> App.Resources.Resources -> Model -> App.View.View Msg
view viewId shared model =
    { title = "Reference"
    , body =
        let
            attrs =
                if List.isEmpty model.references then
                    [ Attr.style "pointer-events" "none"
                    ]

                else
                    [ Theme.Color.backgroundDefault
                    , Attr.style "min-height" "100dvh"
                    , Attr.style "box-sizing" "border-box"
                    , Theme.Color.border
                    , Attr.style "border-left" "1px solid"
                    ]
        in
        Theme.column.lg3
            ([ Theme.pad.xy.md.lg
             , Attr.style "max-width" (String.fromInt 600 ++ "px")
             ]
                ++ attrs
            )
            (model.references
                |> List.foldl
                    (\ref rendered ->
                        Ui.Module.viewBlock (Ref.toModuleName ref)
                            { onClick = Nothing }
                            ref.block
                            :: rendered
                    )
                    []
            )
    }
