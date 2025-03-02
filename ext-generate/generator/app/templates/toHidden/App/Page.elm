module App.Page exposing
    ( Page, page
    , Init, init, initWith, notFound, loadFrom, error
    , withGuard, withKey, withPageCacheLimit
    , InitPlan(..), toInternalDetails, mapInitPlan
    )

{-|

@docs Page, page

@docs Init, init, initWith, notFound, loadFrom, error

@docs withGuard, withKey, withPageCacheLimit


# Internal Details

These are used internally and you shouldn't need to worry about them!

@docs InitPlan, toInternalDetails, mapInitPlan

-}

import App.Page.Error
import App.Page.Id
import App.Stores
import App.View
import App.View.Id
import Effect
import Listen


type Page shared params msg model
    = Page
        { toKey : Maybe (params -> String)
        , pageCacheLimit : Int
        , init : App.Page.Id.Id -> params -> shared -> Maybe model -> Init msg model
        , update : shared -> msg -> model -> ( model, Effect.Effect msg )
        , subscriptions : shared -> model -> Listen.Listen msg
        , view : App.View.Id.Id -> shared -> model -> Result App.Page.Error.Error (App.View.View msg)
        }


{-| -}
page :
    { init : App.Page.Id.Id -> params -> App.Stores.Stores -> Maybe model -> Init msg model
    , update : App.Stores.Stores -> msg -> model -> ( model, Effect.Effect msg )
    , subscriptions : App.Stores.Stores -> model -> Listen.Listen msg
    , view : App.View.Id.Id -> App.Stores.Stores -> model -> App.View.View msg
    }
    -> Page App.Stores.Stores params msg model
page options =
    Page
        { toKey = Nothing
        , pageCacheLimit = 10
        , init = options.init
        , update = options.update
        , subscriptions = options.subscriptions
        , view =
            \region shared model ->
                Ok (options.view region shared model)
        }


{-| This is the key that is used to store the page's state globally.

It defaults to the page's name.

-}
withKey : (params -> String) -> Page shared params msg model -> Page shared params msg model
withKey toKey (Page options) =
    Page { options | toKey = Just toKey }


{-| This is the maximum number of page instances that will be cached, above what is already visible.

This defaults to 10.

-}
withPageCacheLimit : Int -> Page shared params msg model -> Page shared params msg model
withPageCacheLimit limit (Page options) =
    Page { options | pageCacheLimit = max 0 limit }


{-| -}
withGuard :
    (stores -> Result App.Page.Error.Error newStores)
    -> Page newStores params msg model
    -> Page stores params msg model
withGuard toStores (Page options) =
    Page
        { toKey = options.toKey
        , pageCacheLimit = options.pageCacheLimit
        , init =
            \pageId params stores maybeModel ->
                case toStores stores of
                    Err err ->
                        Error err

                    Ok newShared ->
                        options.init pageId params newShared maybeModel
        , update =
            \stores msg model ->
                case toStores stores of
                    Err err ->
                        ( model, Effect.none )

                    Ok newShared ->
                        options.update newShared msg model
        , subscriptions =
            \stores model ->
                case toStores stores of
                    Err err ->
                        Listen.none

                    Ok newShared ->
                        options.subscriptions newShared model
        , view =
            \region stores model ->
                case toStores stores of
                    Err err ->
                        Err err

                    Ok newShared ->
                        options.view region newShared model
        }


{-| -}
type alias Init msg model =
    InitPlan msg model


{-| -}
type InitPlan msg model
    = NotFound
    | Error App.Page.Error.Error
    | Loaded model (Effect.Effect msg)
    | LoadFrom (Effect.Effect (InitPlan msg model))


{-| -}
mapInitPlan :
    { onModel : model -> model2
    , onMsg : msg -> msg2
    }
    -> InitPlan msg model
    -> InitPlan msg2 model2
mapInitPlan ({ onModel, onMsg } as fns) initPlan =
    case initPlan of
        NotFound ->
            NotFound

        Error err ->
            Error err

        Loaded model effect ->
            Loaded (onModel model) (Effect.map onMsg effect)

        LoadFrom effect ->
            LoadFrom (Effect.map (mapInitPlan fns) effect)


{-| -}
init : model -> Init msg model
init model =
    Loaded model Effect.none


{-| -}
initWith : model -> Effect.Effect msg -> Init msg model
initWith model effect =
    Loaded model effect


{-| -}
notFound : Init msg model
notFound =
    NotFound


{-| -}
loadFrom : Effect.Effect (Init msg model) -> Init msg model
loadFrom effect =
    LoadFrom effect


{-| -}
error : App.Page.Error.Error -> Init msg model
error pageError =
    Error pageError



{- Internal -}


{-| -}
toInternalDetails :
    Page shared params msg model
    ->
        { toKey : Maybe (params -> String)
        , pageCacheLimit : Int
        , init : App.Page.Id.Id -> params -> shared -> Maybe model -> Init msg model
        , update : shared -> msg -> model -> ( model, Effect.Effect msg )
        , subscriptions : shared -> model -> Listen.Listen msg
        , view : App.View.Id.Id -> shared -> model -> Result App.Page.Error.Error (App.View.View msg)
        }
toInternalDetails (Page details) =
    details
