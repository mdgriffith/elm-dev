module Page.{{name}} exposing
    ( page
    , Model, Msg
    )

{-|

@docs page
@docs Model, Msg

-}

import Effect
import App.Page
import App.Page.Id
import App.Stores
import Listen
import App.View
import App.View.Id
import Html


{-| -}
type alias Model =
    {}


{-| -}
type Msg
    = ReplaceMe


page : App.Page.Page App.Stores.Stores App.Page.Id.{{name_underscored}}_Params Msg Model
page =
    App.Page.page
        { init = init
        , update = update
        , subscriptions = \stores model -> Listen.none
        , view = view
        }


init : App.Page.Id.Id -> App.Page.Id.{{name_underscored}}_Params -> App.Stores.Stores -> Maybe Model -> App.Page.Init Msg Model
init pageId urlParams stores maybeCached =
    App.Page.init {}


update : App.Stores.Stores -> Msg -> Model -> ( Model, Effect.Effect Msg )
update stores msg model =
    ( model, Effect.none )


view : App.View.Id.Id -> App.Stores.Stores -> Model -> App.View.View Msg
view viewId stores model =
   { title = "{{name}}"
   , body = Html.text "{{name}}"
   }
