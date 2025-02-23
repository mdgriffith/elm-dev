module Effect exposing
    ( none, batch, map
    , Effect(..)
    , broadcast
    , now, nowAfter
    , sendMsg, sendMsgAfter
    , Expect(..), UrlBase(..)
    , toCmd
    )

{-|

@docs none, batch, map

@docs Effect


# Broadcast

@docs broadcast


# Time

@docs now, nowAfter


# Callbacks

@docs sendMsg, sendMsgAfter


# Internal Http Details

@docs Expect, UrlBase


# Effects

@docs toCmd

-}

import App.Page.Id
import App.View.Id
import Broadcast
import Browser.Dom
import Browser.Navigation
import Bytes
import Bytes.Decode
import File
import File.Select
import Http
import Json.Decode
import Json.Encode
import Process
import Random
import Task
import Time


none : Effect msg
none =
    None


batch : List (Effect msg) -> Effect msg
batch =
    Batch


sendMsg : msg -> Effect msg
sendMsg =
    SendMsg


sendMsgAfter : Int -> msg -> Effect msg
sendMsgAfter delay msg =
    SendMsgAfter delay msg


{-| Get the current time
-}
now : (Time.Posix -> msg) -> Effect msg
now =
    Now Nothing


{-| Delay for some number of milliseconds, then get the current time
-}
nowAfter : Float -> (Time.Posix -> msg) -> Effect msg
nowAfter wait =
    Now (Just wait)


{-| -}
broadcast : Broadcast.Msg -> Effect msg
broadcast =
    SendBroadcast


type Effect msg
    = None
    | Batch (List (Effect msg))
      --
    | SendMsg msg
    | SendMsgAfter Int msg
      -- Random generation
    | Generate (Random.Generator msg)
      -- Time
    | Now (Maybe Float) (Time.Posix -> msg)
      -- Focus/Blur
    | Focus String (Result Browser.Dom.Error () -> msg)
    | Blur String (Result Browser.Dom.Error () -> msg)
      -- Get bounding boxes
    | GetBoundingBox { id : String } (Result Browser.Dom.Error Browser.Dom.Element -> msg)
    | GetBoundingBoxList
        { ids : List String }
        (Result
            Browser.Dom.Error
            (List Browser.Dom.Element)
         -> msg
        )
      -- Scroll
    | ScrollToBottomOf { id : String, onScrollFinish : msg }
    | ScrollToTopOf { id : String, onScrollFinish : msg }
    | ScrollTo
        { scrollTo : String
        , viewport : String
        , offsetY : Float
        , onScrollFinish : Result Browser.Dom.Error () -> msg
        }
      -- Urls
    | PushUrl String
    | ReplaceUrl String
      -- Files
    | File (List String) (File.File -> msg)
    | Files (List String) (File.File -> List File.File -> msg)
    | FileToUrl File.File (String -> msg)
      -- Loading
    | ViewUpdated (App.View.Id.Operation App.Page.Id.Id)
      -- Browser Navigation
    | Preload App.Page.Id.Id
    | Load String
    | Reload
    | ReloadAndSkipCache
      -- History navigation
    | Forward Int
    | Back Int
      -- Http
    | HttpRequest (RequestDetails msg)
    | SendBroadcast Broadcast.Msg
      -- JS interop
    | SendToWorld
        { toPort : Json.Encode.Value -> Cmd msg
        , portName : String
        , payload : Json.Encode.Value
        }


type alias RequestDetails msg =
    { method : String
    , headers : List Http.Header
    , url : String
    , urlBase : Maybe UrlBase
    , body : Http.Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }


{-| This type is here if you want to do something like include special headers for your API

Or switch out urls depending on how the app is configured.

-}
type UrlBase
    = UrlApi
    | UrlStaticFile
    | UrlCustom String


type Expect msg
    = ExpectString (Result Http.Error String -> msg)
    | ExpectStringResponse (Http.Response String -> msg)
    | ExpectJson (Json.Decode.Decoder msg) (Http.Error -> msg)
    | ExpectBytes (Bytes.Decode.Decoder msg) (Http.Error -> msg)
    | ExpectBytesResponse (Http.Response Bytes.Bytes -> msg)
    | ExpectWhatever (Result Http.Error () -> msg)


toCmd :
    { navKey : Browser.Navigation.Key
    , preload : App.Page.Id.Id -> msg
    , dropPageCache : msg
    , viewRequested : App.View.Id.Operation App.Page.Id.Id -> msg
    , broadcast : Broadcast.Msg -> msg
    }
    ->
        (UrlBase
         ->
            { headers : List Http.Header
            , urlBase : String
            }
        )
    -> Effect msg
    -> Cmd msg
toCmd options toHttpTarget effect =
    case effect of
        None ->
            Cmd.none

        Batch effects ->
            Cmd.batch (List.map (toCmd options toHttpTarget) effects)

        Generate generator ->
            Random.generate identity generator

        Now Nothing toMsg ->
            Time.now
                |> Task.perform toMsg

        Now (Just wait) toMsg ->
            Process.sleep wait
                |> Task.andThen
                    (\_ -> Time.now)
                |> Task.perform toMsg

        Focus id toMsg ->
            Process.sleep 1
                |> Task.andThen
                    (\_ -> Browser.Dom.focus id)
                |> Task.attempt toMsg

        Blur id toMsg ->
            Browser.Dom.blur id
                |> Task.attempt toMsg

        PushUrl url ->
            Browser.Navigation.pushUrl options.navKey url

        ReplaceUrl url ->
            Browser.Navigation.replaceUrl options.navKey url

        GetBoundingBox { id } toMsg ->
            Browser.Dom.getElement id
                |> Task.attempt toMsg

        GetBoundingBoxList { ids } toMsg ->
            ids
                |> List.map Browser.Dom.getElement
                |> Task.sequence
                |> Task.attempt toMsg

        ScrollToBottomOf { id, onScrollFinish } ->
            --Wait a moment because the DOM needs to update
            Process.sleep 1
                |> Task.andThen (\() -> Browser.Dom.getViewportOf id)
                |> Task.andThen (\info -> Browser.Dom.setViewportOf id 0 info.scene.height)
                |> Task.attempt (\_ -> onScrollFinish)

        ScrollToTopOf { id, onScrollFinish } ->
            Browser.Dom.setViewportOf id 0 0
                |> Task.attempt (\_ -> onScrollFinish)

        ScrollTo scrollOptions ->
            Browser.Dom.getElement scrollOptions.scrollTo
                |> Task.andThen
                    (\scrollToElem ->
                        Task.map2
                            (\viewport containerElem ->
                                { scrollToElem = scrollToElem
                                , containerElem = containerElem
                                , viewport = viewport
                                }
                            )
                            (Browser.Dom.getViewportOf scrollOptions.viewport)
                            (Browser.Dom.getElement scrollOptions.viewport)
                    )
                |> Task.andThen
                    (\elems ->
                        Browser.Dom.setViewportOf scrollOptions.viewport
                            0
                            ((elems.scrollToElem.element.y + (elems.viewport.viewport.y - elems.containerElem.element.y)) - scrollOptions.offsetY)
                    )
                |> Task.attempt scrollOptions.onScrollFinish

        ViewUpdated op ->
            Task.succeed ()
                |> Task.perform
                    (\_ ->
                        options.viewRequested op
                    )

        Load url ->
            Browser.Navigation.load url

        Reload ->
            Browser.Navigation.reload

        ReloadAndSkipCache ->
            Browser.Navigation.reloadAndSkipCache

        Forward steps ->
            Browser.Navigation.forward options.navKey steps

        Back steps ->
            Browser.Navigation.back options.navKey steps

        SendToWorld { toPort, payload } ->
            toPort payload

        SendBroadcast msg ->
            Task.succeed msg
                |> Task.perform options.broadcast

        SendMsg msg ->
            Task.succeed ()
                |> Task.perform (\_ -> msg)

        SendMsgAfter delay msg ->
            Process.sleep (toFloat delay)
                |> Task.map (\_ -> msg)
                |> Task.perform identity

        Preload pageId ->
            Task.succeed ()
                |> Task.perform (\_ -> options.preload pageId)

        File extensions toMsg ->
            File.Select.file extensions toMsg

        Files extensions toMsg ->
            File.Select.files extensions toMsg

        FileToUrl fileData toMsg ->
            File.toUrl fileData
                |> Task.perform toMsg

        HttpRequest req ->
            let
                maybeUrlBase =
                    Maybe.map toHttpTarget req.urlBase
            in
            Http.request
                { method = req.method
                , body = req.body
                , url =
                    case maybeUrlBase of
                        Nothing ->
                            req.url

                        Just base ->
                            joinPath base.urlBase req.url
                , headers =
                    case maybeUrlBase of
                        Nothing ->
                            req.headers

                        Just base ->
                            req.headers ++ base.headers
                , expect = toHttpExpect req.expect
                , timeout = req.timeout
                , tracker = req.tracker
                }


joinPath : String -> String -> String
joinPath base path =
    let
        baseSlash =
            String.endsWith "/" base

        pathSlash =
            String.startsWith "/" path
    in
    if baseSlash && pathSlash then
        base ++ String.dropLeft 1 path

    else if baseSlash || pathSlash then
        base ++ path

    else
        base ++ "/" ++ path


map : (a -> b) -> Effect a -> Effect b
map f effect =
    case effect of
        None ->
            None

        Batch effects ->
            Batch (List.map (map f) effects)

        PushUrl url ->
            PushUrl url

        ReplaceUrl url ->
            ReplaceUrl url

        ViewUpdated op ->
            ViewUpdated op

        Load url ->
            Load url

        Reload ->
            Reload

        ReloadAndSkipCache ->
            ReloadAndSkipCache

        Forward n ->
            Forward n

        Back n ->
            Back n

        SendToWorld { toPort, portName, payload } ->
            SendToWorld
                { toPort = \val -> Cmd.map f (toPort val)
                , portName = portName
                , payload = payload
                }

        SendBroadcast msg ->
            SendBroadcast msg

        SendMsg msg ->
            SendMsg (f msg)

        SendMsgAfter delay msg ->
            SendMsgAfter delay (f msg)

        Focus id msg ->
            Focus id (msg >> f)

        Blur id msg ->
            Blur id (msg >> f)

        GetBoundingBox id msg ->
            GetBoundingBox id (msg >> f)

        GetBoundingBoxList ids msg ->
            GetBoundingBoxList ids (msg >> f)

        ScrollToBottomOf { id, onScrollFinish } ->
            ScrollToBottomOf { id = id, onScrollFinish = f onScrollFinish }

        ScrollToTopOf { id, onScrollFinish } ->
            ScrollToTopOf { id = id, onScrollFinish = f onScrollFinish }

        ScrollTo scrollToDetials ->
            ScrollTo
                { scrollTo = scrollToDetials.scrollTo
                , viewport = scrollToDetials.viewport
                , offsetY = scrollToDetials.offsetY
                , onScrollFinish = scrollToDetials.onScrollFinish >> f
                }

        Preload route ->
            Preload route

        HttpRequest req ->
            HttpRequest
                { method = req.method
                , headers = req.headers
                , url = req.url
                , urlBase = req.urlBase
                , body = req.body
                , expect = mapExpect f req.expect
                , timeout = req.timeout
                , tracker = req.tracker
                }

        File extensions toMsg ->
            File extensions (toMsg >> f)

        Files extensions toMsg ->
            Files extensions (\top remaining -> toMsg top remaining |> f)

        FileToUrl fileData toMsg ->
            FileToUrl fileData (toMsg >> f)

        Now maybeWait toMsg ->
            Now maybeWait (toMsg >> f)

        Generate generator ->
            Generate (Random.map f generator)


toHttpExpect : Expect msg -> Http.Expect msg
toHttpExpect expect =
    case expect of
        ExpectString toMsg ->
            Http.expectString toMsg

        ExpectStringResponse toMsg ->
            Http.expectStringResponse
                (\result ->
                    case result of
                        Err err ->
                            err

                        Ok value ->
                            value
                )
                (\response ->
                    Ok (toMsg response)
                )

        ExpectJson decoder onError ->
            Http.expectJson
                (\result ->
                    case result of
                        Err err ->
                            onError err

                        Ok value ->
                            value
                )
                decoder

        ExpectBytes decoder onError ->
            Http.expectBytes
                (\result ->
                    case result of
                        Err err ->
                            onError err

                        Ok value ->
                            value
                )
                decoder

        ExpectBytesResponse toMsg ->
            Http.expectBytesResponse
                (\result ->
                    case result of
                        Err err ->
                            err

                        Ok value ->
                            value
                )
                (\response ->
                    Ok (toMsg response)
                )

        ExpectWhatever toMsg ->
            Http.expectWhatever toMsg


mapExpect : (a -> b) -> Expect a -> Expect b
mapExpect fn expect =
    case expect of
        ExpectString toMsg ->
            ExpectString (toMsg >> fn)

        ExpectStringResponse toMsg ->
            ExpectStringResponse (toMsg >> fn)

        ExpectJson decoder onError ->
            ExpectJson (Json.Decode.map fn decoder) (onError >> fn)

        ExpectBytes decoder onError ->
            ExpectBytes (Bytes.Decode.map fn decoder) (onError >> fn)

        ExpectBytesResponse toMsg ->
            ExpectBytesResponse (toMsg >> fn)

        ExpectWhatever toMsg ->
            ExpectWhatever (toMsg >> fn)
