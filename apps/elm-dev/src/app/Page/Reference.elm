module Page.Reference exposing (page, Model, Msg)

{-|

@docs page, Model, Msg

-}

import App.Page
import App.Page.Id
import App.Stores
import App.View
import App.View.Region
import Broadcast
import Docs.Ref
import Effect exposing (Effect)
import Elm.Module
import Html exposing (Html)
import Html.Attributes as Attr
import Listen exposing (Listen)
import Ui
import Ui.Attr
import Ui.Module


{-| -}
type alias Model =
    { references : List Docs.Ref.Ref
    }


{-| -}
type Msg
    = RemoveById Docs.Ref.Id
    | RefAdded Docs.Ref.Ref


page : App.Page.Page App.Stores.Stores App.Page.Id.Reference_Params Msg Model
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
    -> App.Stores.Stores
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


update : App.Stores.Stores -> Msg -> Model -> ( Model, Effect Msg )
update stores msg model =
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


subscriptions : App.Stores.Stores -> Model -> Listen Msg
subscriptions stores model =
    Listen.onBroadcast
        (\broadcastMsg ->
            case broadcastMsg of
                Broadcast.RefPinned ref ->
                    Just (RefAdded ref)

                _ ->
                    Nothing
        )


view : App.View.Region.Id -> App.Stores.Stores -> Model -> App.View.View Msg
view viewId shared model =
    { title = "Reference"
    , body =
        let
            attrs =
                if List.isEmpty model.references then
                    [ Attr.style "pointer-events" "none"
                    ]

                else
                    [ Attr.style "min-height" "100dvh"
                    , Attr.style "box-sizing" "border-box"
                    , Attr.style "border-left" "1px solid"
                    ]
        in
        Ui.column
            ([ Ui.Attr.padXY 16 32
             , Ui.Attr.gap 16
             , Attr.style "max-width" (String.fromInt 600 ++ "px")
             ]
                ++ attrs
            )
            (model.references
                |> List.foldl
                    (\ref rendered ->
                        Ui.Module.viewBlock (Elm.Module.toString ref.source.moduleName)
                            { onClick = Nothing }
                            ref.block
                            :: rendered
                    )
                    []
            )
    }
