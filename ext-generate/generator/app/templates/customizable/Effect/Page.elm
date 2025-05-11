module Effect.Page exposing
    ( preload
    , loadAt, clear
    )

{-|

@docs preload

@docs loadAt, clear

-}

import App.Page.Id
import App.View.Region
import Effect exposing (Effect)


{-| -}
preload : App.Page.Id.Id -> Effect msg
preload =
    Effect.Preload


{-| -}
loadAt : App.View.Region.Region -> App.Page.Id.Id -> Effect msg
loadAt region pageId =
    Effect.ViewUpdated (App.View.Region.Push region pageId)


{-| -}
clear : App.View.Region.Region -> Effect msg
clear region =
    Effect.ViewUpdated (App.View.Region.ClearRegion region)
