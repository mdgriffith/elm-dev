module Page.Home exposing (page, Model, Msg)

{-|

@docs page, Model, Msg

-}

import App.Page
import App.Page.Id
import App.Route
import App.Stores
import App.View
import App.View.Id
import Docs.Packages
import Effect exposing (Effect)
import Html
import Listen exposing (Listen)


{-| -}
type alias Model =
    {}


{-| -}
type Msg
    = ReplaceMe


page : App.Page.Page App.Stores.Stores App.Page.Id.Home_Params Msg Model
page =
    App.Page.page
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : App.Page.Id.Id -> App.Page.Id.Home_Params -> App.Stores.Stores -> Maybe Model -> App.Page.Init Msg Model
init pageId params shared maybeCached =
    App.Page.init {}


update : App.Stores.Stores -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    ( model, Effect.none )


subscriptions : App.Stores.Stores -> Model -> Listen Msg
subscriptions shared model =
    Listen.none


view : App.View.Id.Id -> App.Stores.Stores -> Model -> App.View.View Msg
view viewId shared model =
    { title = "Directory"
    , body = viewPackages
    }


viewPackages =
    Html.div []
        [ Html.h1 [] [ Html.text "Packages" ]
        ]
