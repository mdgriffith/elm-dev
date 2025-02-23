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
    , detail : Maybe view
    }


{-| -}
isVisible : view -> Regions view -> Bool
isVisible view regions =
    case regions.primary of
        Just primaryView ->
            if view == primaryView then
                True

            else
                Just view == regions.detail

        Nothing ->
            Just view == regions.detail
