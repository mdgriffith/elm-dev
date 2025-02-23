module Page.Package exposing (page, Model, Msg)

{-|

@docs page, Model, Msg

-}

import App.Page
import App.Page.Id
import App.Resources
import App.View
import App.View.Id
import Broadcast
import Docs.Packages
import Effect exposing (Effect)
import Elm.Docs
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Listen exposing (Listen)
import Ref
import Theme
import Ui.Attr
import Ui.Module


{-| -}
type alias Model =
    { name : String
    , modules : List Elm.Docs.Module
    , focusedModule : Maybe String
    }


{-| -}
type Msg
    = ModuleClicked String
    | TypeClicked String


page : App.Page.Page App.Resources.Resources App.Page.Id.Package_Params Msg Model
page =
    App.Page.page
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : App.Page.Id.Id -> App.Page.Id.Package_Params -> App.Resources.Resources -> Maybe Model -> App.Page.Init Msg Model
init pageId params shared maybeCached =
    let
        key =
            String.join "/" params.path_

        docs =
            List.foldl
                (\doc found ->
                    case found of
                        Just d ->
                            found

                        Nothing ->
                            if doc.name == key then
                                Just doc

                            else
                                Nothing
                )
                Nothing
                Docs.Packages.directory
    in
    case docs of
        Nothing ->
            App.Page.notFound

        Just doc ->
            App.Page.init
                { name = doc.name
                , modules = doc.modules
                , focusedModule =
                    List.head doc.modules
                        |> Maybe.map .name
                }


update : App.Resources.Resources -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        ModuleClicked name ->
            ( { model | focusedModule = Just name }
            , Effect.none
            )

        TypeClicked typename ->
            ( model
            , case Ref.lookup typename of
                Nothing ->
                    Effect.none

                Just ref ->
                    Effect.broadcast (Broadcast.RefPinned ref)
            )


subscriptions : App.Resources.Resources -> Model -> Listen Msg
subscriptions shared model =
    Listen.none


getModule : List Elm.Docs.Module -> String -> Maybe Elm.Docs.Module
getModule modules name =
    List.foldl
        (\mod found ->
            case found of
                Just m ->
                    found

                Nothing ->
                    if mod.name == name then
                        Just mod

                    else
                        Nothing
        )
        Nothing
        modules


view : App.View.Id.Id -> App.Resources.Resources -> Model -> App.View.View Msg
view viewId shared model =
    { title = model.name
    , body =
        Theme.column.lg3
            [ Ui.Attr.pad 48
            , Ui.Attr.width 600
            ]
            [ Html.h1 [] [ Html.text model.name ]
            , Html.div []
                (List.map
                    (\mod ->
                        Html.div
                            [ Events.onClick (ModuleClicked mod.name)
                            , Attr.style "cursor" "pointer"
                            , Attr.style "text-decoration" "underline"
                            ]
                            [ Html.text mod.name ]
                    )
                    model.modules
                )
            , case Maybe.andThen (getModule model.modules) model.focusedModule of
                Nothing ->
                    Html.text ""

                Just focusedModule ->
                    Ui.Module.view { onClick = Just TypeClicked } focusedModule
            ]
    }
