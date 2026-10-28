module Effect.Http exposing
    ( get, request, cancel
    , expectString, expectStringResponse, expectJson, expectBytes, expectWhatever
    , bytesBody, emptyBody, fileBody, header, jsonBody, multipartBody, stringBody
    )

{-|


# Http

@docs get, request, cancel

@docs Expect, expectString, expectStringResponse, expectJson, expectBytes, expectWhatever

-}

import Bytes
import Bytes.Decode
import Effect exposing (Effect)
import File
import Http
import Json.Decode
import Json.Encode


type alias Expect msg =
    Effect.Expect msg


get : String -> Expect msg -> Effect msg
get url expect =
    request
        { method = "GET"
        , headers = []
        , url = url
        , urlBase = Nothing
        , body = emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


{-| This is how to make an HTTP request, but it's kinda low level!

Instead of using it directly, which would be a bit of a pain, make a new module that models the API you're working with.

So, let's say you're working with the GitHub API.

Run `elm-prefab add effect http Github`.

You should now have a new module called `Effect.Github` that you can use to make requests to the GitHub API. Bake in as many details as you can to make the API nice!

-}
request :
    { method : String
    , headers : List Effect.Header
    , url : String
    , urlBase : Maybe Effect.UrlBase
    , body : Effect.Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Effect msg
request options =
    Effect.HttpRequest options


{-| Cancel requests with the given tracker. Mirrors `Http.cancel`.
Set `tracker = Just tracker` when making the requests you want to cancel.
-}
cancel : String -> Effect msg
cancel =
    Effect.CancelHttp


expectString : (Result Http.Error String -> msg) -> Expect msg
expectString =
    Effect.ExpectString


{-| When you need more control over the error handling
-}
expectStringResponse : (Http.Response String -> msg) -> Expect msg
expectStringResponse =
    Effect.ExpectStringResponse


expectJson : Json.Decode.Decoder msg -> (Http.Error -> msg) -> Expect msg
expectJson =
    Effect.ExpectJson


expectBytes : Bytes.Decode.Decoder msg -> (Http.Error -> msg) -> Expect msg
expectBytes =
    Effect.ExpectBytes


expectWhatever : (Result Http.Error () -> msg) -> Expect msg
expectWhatever =
    Effect.ExpectWhatever


header : String -> String -> Effect.Header
header =
    Tuple.pair


emptyBody : Effect.Body
emptyBody =
    Effect.EmptyBody


stringBody : String -> String -> Effect.Body
stringBody =
    Effect.StringBody


jsonBody : Json.Encode.Value -> Effect.Body
jsonBody =
    Effect.JsonBody


bytesBody : String -> Bytes.Bytes -> Effect.Body
bytesBody =
    Effect.BytesBody


fileBody : File.File -> Effect.Body
fileBody =
    Effect.FileBody


multipartBody : List Http.Part -> Effect.Body
multipartBody =
    Effect.MultipartBody
