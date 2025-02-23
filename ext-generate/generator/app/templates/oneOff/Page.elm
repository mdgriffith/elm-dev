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
import App.Resources
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


page : App.Page.Page App.Resources.Resources App.Page.Id.{{name_underscored}}_Params Msg Model
page =
    App.Page.page
        { init = init
        , update = update
        , subscriptions = \resources model -> Listen.none
        , view = view
        }


init : App.Page.Id.Id -> App.Page.Id.{{name_underscored}}_Params -> App.Resources.Resources -> Maybe Model -> App.Page.Init Msg Model
init pageId urlParams resources maybeCached =
    App.Page.init {}


update : App.Resources.Resources -> Msg -> Model -> ( Model, Effect.Effect Msg )
update resources msg model =
    ( model, Effect.none )


view : App.View.Id.Id -> App.Resources.Resources -> Model -> App.View.View Msg
view viewId resources model =
   { title = "{{name}}"
   , body = Html.text "{{name}}"
   }
