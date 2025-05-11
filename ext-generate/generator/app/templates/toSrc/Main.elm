module Main exposing (main)

{-| -}

import App
import App.Page.Id
import App.Route
import App.Stores
import App.View
import App.View.Region
import Browser
import Effect exposing (Effect)
import Effect.Nav
import Effect.Page
import Effect.Scroll
import Html
import Html.Attributes as Attr
import Listen
import Url


type alias Model =
    { urlNotFound : Bool
    }


{-| -}
main : App.App Model Msg
main =
    App.app
        { init =
            \stores flags url ->
                loadUrl url { urlNotFound = False }
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        , update = update
        , subscriptions =
            \stores model ->
                Listen.none
        , toCmd = toCmd
        , toSub = toSub
        , view =
            \stores toAppMsg model regions ->
                if model.urlNotFound then
                    -- Globally, this url was not found
                    { title = "Not found"
                    , body = [ Html.text "Not found" ]
                    }

                else
                    case regions.primary of
                        Nothing ->
                            { title = "Nothing"
                            , body = [ Html.text "" ]
                            }

                        Just (App.Loading _) ->
                            { title = "Loading"
                            , body = [ Html.text "Loading" ]
                            }

                        Just App.NotFound ->
                            { title = "Not found"
                            , body = [ Html.text "Not found" ]
                            }

                        Just (App.Error error) ->
                            -- error is a type you control that lives at App.Page.Error
                            { title = "Not found"
                            , body = [ Html.text "Not found" ]
                            }

                        Just (App.View page) ->
                            { title = page.title
                            , body =
                                [ page.body
                                ]
                            }
        }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : App.Stores.Stores -> Msg -> Model -> ( Model, Effect Msg )
update stores msg model =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( model, Effect.Nav.pushUrl (Url.toString url) )

        UrlRequested (Browser.External urlStr) ->
            ( model, Effect.Nav.load urlStr )

        UrlChanged url ->
            loadUrl url model


loadUrl : Url.Url -> Model -> ( Model, Effect Msg )
loadUrl url model =
    case App.Route.parse url of
        Nothing ->
            ( { model | urlNotFound = True }, Effect.none )

        Just { isRedirect, route } ->
            if isRedirect then
                ( model, Effect.Nav.replaceUrl (App.Route.toString route) )

            else
                case App.Page.Id.fromRoute route of
                    Nothing ->
                        ( { model | urlNotFound = True }
                        , Effect.none
                        )

                    Just pageId ->
                        ( { model | urlNotFound = False }
                        , Effect.batch
                            [ Effect.Page.loadAt App.View.Region.Primary pageId
                            , Effect.Scroll.resetWindow
                            ]
                        )


toSub : App.Stores.Stores -> App.SubOptions Msg -> Model -> Listen.Listen (App.Msg Msg) -> Sub (App.Msg Msg)
toSub stores options model sub =
    Listen.toSubscription options sub


toCmd : App.Stores.Stores -> App.CmdOptions Msg -> Model -> Effect.Effect (App.Msg Msg) -> Cmd (App.Msg Msg)
toCmd stores options model effect =
    Effect.toCmd options
        (\urlBase ->
            case urlBase of
                Effect.UrlApi ->
                    { headers = []
                    , urlBase = ""
                    }

                Effect.UrlStaticFile ->
                    { headers = []
                    , urlBase = ""
                    }

                Effect.UrlCustom base ->
                    { headers = []
                    , urlBase = base
                    }
        )
        effect
