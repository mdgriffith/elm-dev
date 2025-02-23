module Effect.Page exposing
    ( preload
    , loadAt, clear
    )

{-|

@docs preload

@docs loadAt, clear

-}

import App.Page.Id
import App.View.Id
import Effect exposing (Effect)


{-| -}
preload : App.Page.Id.Id -> Effect msg
preload =
    Effect.Preload


{-| -}
loadAt : App.View.Id.Region -> App.Page.Id.Id -> Effect msg
loadAt region pageId =
    Effect.ViewUpdated (App.View.Id.Push region pageId)


{-| -}
clear : App.View.Id.Region -> Effect msg
clear region =
    Effect.ViewUpdated (App.View.Id.ClearRegion region)
