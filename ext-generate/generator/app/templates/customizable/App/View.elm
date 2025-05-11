module App.View exposing (View, map)

{-|

@docs View, map

-}

import Html


type alias View msg =
    { title : String
    , body : Html.Html msg
    }


map : (a -> b) -> View a -> View b
map fn myView =
    { title = myView.title
    , body = Html.map fn myView.body
    }
