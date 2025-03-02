module Page.Guide exposing (page, Model, Msg)

{-|

@docs page, Model, Msg

-}

import App.Page
import App.Page.Id
import App.Stores
import App.View
import App.View.Id
import Docs.Guides
import Effect exposing (Effect)
import Html exposing (Html)
import Listen exposing (Listen)
import Theme
import Ui.Attr
import Ui.Markdown


{-| -}
type alias Model =
    { guide : Maybe { path : String, content : String } }


{-| -}
type Msg
    = ReplaceMe


page : App.Page.Page App.Stores.Stores App.Page.Id.Guide_Params Msg Model
page =
    App.Page.page
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : App.Page.Id.Id -> App.Page.Id.Guide_Params -> App.Stores.Stores -> Maybe Model -> App.Page.Init Msg Model
init pageId params shared maybeCached =
    App.Page.init { guide = lookupGuide (String.join "/" params.path_) }


lookupGuide : String -> Maybe { path : String, content : String }
lookupGuide path =
    List.head (List.filter (\guide -> guide.path == path) Docs.Guides.all_)


update : App.Stores.Stores -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    ( model, Effect.none )


subscriptions : App.Stores.Stores -> Model -> Listen Msg
subscriptions shared model =
    Listen.none


view : App.View.Id.Id -> App.Stores.Stores -> Model -> App.View.View Msg
view viewId shared model =
    { title = "Guides"
    , body =
        case model.guide of
            Just guide ->
                Ui.Markdown.view guide.content
                    |> Theme.el
                        [ Ui.Attr.pad 48
                        , Ui.Attr.width 600
                        ]

            Nothing ->
                Html.text "Guide not found"
    }
