module Effect.Nav exposing
    ( toRoute
    , load, reload, reloadAndSkipCache
    , forward, back
    , pushUrl, replaceUrl
    )

{-|


# Navigation

This package mirrors everything in <https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation>

But also adds `toRoute` which is like `pushUrl` but with your Route type.

@docs toRoute

@docs load, reload, reloadAndSkipCache

@docs forward, back

@docs pushUrl, replaceUrl

-}

import App.Page.Id
import App.Route
import App.View.Region
import Effect exposing (Effect)
import Url


{-| The default way to navigate between routes within your app.

This will change the URL, add a new entry to the browser history, but not reload the page.

-}
toRoute : App.Route.Route -> Effect msg
toRoute route =
    Effect.PushUrl (App.Route.toString route)


{-| Change the URL, but do not trigger a page load.

This will add a new entry to the browser history.

-}
pushUrl : String -> Effect msg
pushUrl =
    Effect.PushUrl


{-| Change the URL, but do not trigger a page load.

This will _not_ add a new entry to the browser history.

-}
replaceUrl : String -> Effect msg
replaceUrl =
    Effect.ReplaceUrl


{-| -}
forward : Int -> Effect msg
forward =
    Effect.Forward


{-| -}
back : Int -> Effect msg
back =
    Effect.Back


{-| -}
load : String -> Effect msg
load =
    Effect.Load


{-| -}
reload : Effect msg
reload =
    Effect.Reload


{-| -}
reloadAndSkipCache : Effect msg
reloadAndSkipCache =
    Effect.ReloadAndSkipCache
