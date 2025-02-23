module Effect.{{name}} exposing (query, mutation)

{-| -}

import Effect exposing (Effect)
import Effect.Http
import Json.Decode
import GraphQL.Engine
import Http
import {{name}}

query :
    { onError : GraphQL.Engine.Error -> msg
    , query : {{name}}.Query msg
    }
    -> Effect msg
query options =
    let
        { payload, decoder } =
            GraphQL.Engine.queryToTestingDetails options.query
    in
    Effect.Http.request
        { method = "POST"
        , headers = []
        , url = ""
        , urlBase = Just Effect.UrlApi
        , body = Http.jsonBody payload
        , expect = expect options.onError decoder
        , timeout = Nothing
        , tracker = Nothing
        }


mutation :
    { onError : GraphQL.Engine.Error -> msg
    , mutation : {{name}}.Mutation msg
    }
    -> Effect msg
mutation options =
    let
        { payload, decoder } =
            GraphQL.Engine.mutationToTestingDetails options.mutation
    in
    Effect.Http.request
        { method = "POST"
        , headers = []
        , url = ""
        , urlBase = Just Effect.UrlApi
        , body = Http.jsonBody payload
        , expect = expect options.onError decoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| -}
expect : (GraphQL.Engine.Error -> msg) -> Json.Decode.Decoder msg -> Effect.Expect msg
expect fromError decoder =
    Effect.ExpectStringResponse
        (\response ->
            case responseToResult decoder response of
                Ok data ->
                    data

                Err err ->
                    fromError err
        )


responseToResult : Json.Decode.Decoder data -> Http.Response String -> Result GraphQL.Engine.Error data
responseToResult decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (GraphQL.Engine.BadUrl url)

        Http.Timeout_ ->
            Err GraphQL.Engine.Timeout

        Http.NetworkError_ ->
            Err GraphQL.Engine.NetworkError

        Http.BadStatus_ metadata responseBody ->
            Err
                (GraphQL.Engine.BadStatus
                    { status = metadata.statusCode
                    , responseBody = responseBody
                    }
                )

        Http.GoodStatus_ metadata responseBody ->
            let
                bodyDecoder =
                    Json.Decode.oneOf
                        [ Json.Decode.map2
                            (\_ errs ->
                                Err errs
                            )
                            (Json.Decode.field "data" (Json.Decode.null ()))
                            (Json.Decode.field "errors"
                                (Json.Decode.list gqlErrorDecoder)
                            )
                        , Json.Decode.field "data" decoder
                            |> Json.Decode.map Ok
                        , Json.Decode.field "errors"
                            (Json.Decode.list gqlErrorDecoder)
                            |> Json.Decode.map Err
                        ]
            in
            case Json.Decode.decodeString bodyDecoder responseBody of
                Ok (Ok success) ->
                    Ok success

                Ok (Err graphqlErrors) ->
                    Err
                        (GraphQL.Engine.ErrorField
                            { errors = graphqlErrors
                            }
                        )

                Err err ->
                    Err
                        (GraphQL.Engine.BadBody
                            { responseBody = responseBody
                            , decodingError = Json.Decode.errorToString err
                            }
                        )



{-| A graphQL error specified here: <https://github.com/graphql/graphql-spec/blob/main/spec/Section%207%20--%20Response.md>
-}
gqlErrorDecoder : Json.Decode.Decoder GqlError
gqlErrorDecoder =
    Json.Decode.map4 GqlError
        (Json.Decode.field "message" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "path" (Json.Decode.list Json.Decode.string)))
        (Json.Decode.maybe (Json.Decode.field "locations" (Json.Decode.list locationDecoder)))
        (Json.Decode.maybe (Json.Decode.field "extensions" Json.Decode.value))


locationDecoder : Json.Decode.Decoder Location
locationDecoder =
    Json.Decode.map2 Location
        (Json.Decode.field "line" Json.Decode.int)
        (Json.Decode.field "column" Json.Decode.int)


type alias GqlError =
    { message : String
    , path : Maybe (List String)
    , locations : Maybe (List Location)
    , extensions : Maybe Json.Decode.Value
    }


type alias Location =
    { line : Int
    , column : Int
    }
