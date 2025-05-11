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


{-| Load the data for a page, but don't show it yet.
-}
preload : App.Page.Id.Id -> Effect msg
preload =
    Effect.Preload


{-| Load the page (if necessary) and view a page at a specific region
-}
loadAt : App.View.Region.Region -> App.Page.Id.Id -> Effect msg
loadAt region pageId =
    Effect.ViewUpdated (App.View.Region.Push region pageId)


{-| -}
clear : App.View.Region.Region -> Effect msg
clear region =
    Effect.ViewUpdated (App.View.Region.ClearRegion region)
