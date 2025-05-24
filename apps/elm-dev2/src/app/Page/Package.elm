module Page.Package exposing (page, Model, Msg)

{-|

@docs page, Model, Msg

-}

import App.Page
import App.Page.Id
import App.Stores
import App.View
import App.View.Region
import Broadcast
import Docs.Ref.Get
import Effect exposing (Effect)
import Elm.Docs
import Elm.Module
import Elm.Package
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Listen exposing (Listen)
import Store.Modules
import Store.Packages
import Ui.Attr
import Ui.Module


{-| -}
type alias Model =
    { package : Store.Packages.Package
    , focused : Maybe Elm.Module.Name
    }


{-| -}
type Msg
    = ModuleClicked Elm.Module.Name
    | TypeClicked String


page : App.Page.Page App.Stores.Stores App.Page.Id.Package_Params Msg Model
page =
    App.Page.page
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : App.Page.Id.Id -> App.Page.Id.Package_Params -> App.Stores.Stores -> Maybe Model -> App.Page.Init Msg Model
init pageId params shared maybeCached =
    case Store.Packages.lookup params.path_ shared.packages of
        Nothing ->
            App.Page.notFound

        Just pkg ->
            App.Page.init
                { package = pkg
                , focused = Nothing
                }


update : App.Stores.Stores -> Msg -> Model -> ( Model, Effect Msg )
update stores msg model =
    case msg of
        ModuleClicked name ->
            ( { model | focused = Just name }
            , Effect.none
            )

        TypeClicked typename ->
            ( model
            , case Docs.Ref.Get.lookup typename stores of
                Nothing ->
                    Effect.none

                Just ref ->
                    Effect.broadcast (Broadcast.RefPinned ref)
            )


subscriptions : App.Stores.Stores -> Model -> Listen Msg
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


view : App.View.Region.Id -> App.Stores.Stores -> Model -> App.View.View Msg
view viewId stores model =
    { title = Elm.Package.toString model.package.info.name
    , body =
        Html.div
            [ Ui.Attr.pad 48
            , Ui.Attr.width 600
            ]
            [ Html.h1 [] [ Html.text (Elm.Package.toString model.package.info.name) ]
            , Html.div []
                (List.map
                    (\modName ->
                        Html.div
                            [ Events.onClick (ModuleClicked modName)
                            , Attr.style "cursor" "pointer"
                            , Attr.style "text-decoration" "underline"
                            ]
                            [ Html.text (Elm.Module.toString modName) ]
                    )
                    (Store.Packages.getExposed model.package)
                )
            , let
                focused =
                    case model.focused of
                        Nothing ->
                            Nothing

                        Just modName ->
                            Store.Modules.getByName modName stores.modules
              in
              case focused of
                Nothing ->
                    Html.text ""

                Just focusedModule ->
                    Ui.Module.view { onClick = Just TypeClicked } focusedModule.info
            ]
    }
