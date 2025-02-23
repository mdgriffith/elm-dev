module App.View exposing
    ( View, map
    , Regions, isVisible
    )

{-|

@docs View, map

@docs Regions, isVisible

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



{- Regions -}


{-| -}
type alias Regions view =
    { primary : Maybe view
    , detail : List view
    }


{-| -}
isVisible : view -> Regions view -> Bool
isVisible view regions =
    case regions.primary of
        Just primaryView ->
            if view == primaryView then
                True

            else
                List.any ((==) view) regions.detail

        Nothing ->
            List.any ((==) view) regions.detail
