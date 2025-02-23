module Resource.Auth exposing
    ( resource, isAuthenticated
    , Authenticated(..), Model, Msg(..)
    )

{-|

@docs resource, isAuthenticated

@docs Authenticated, Model, Msg

-}

import App.Resource
import Broadcast
import Effect
import Json.Decode as Decode
import Json.Encode as Encode
import Listen
import Time
import Url.Parser exposing ((<?>))


isAuthenticated : Model -> Bool
isAuthenticated model =
    case model.authenticated of
        Authenticated _ ->
            True

        Unauthenticated ->
            False


type alias Model =
    { apiUrl : String
    , authenticated : Authenticated
    }


type Authenticated
    = Authenticated
        { tokens : Tokens
        }
    | Unauthenticated


type alias Tokens =
    { refresh : String
    , access : String
    , expiresAt : Time.Posix
    }


type Msg
    = LogOut
    | AuthenticationReceived Tokens


resource : App.Resource.Resource Msg Model
resource =
    App.Resource.resource
        { init =
            \flags url maybeCachedModel ->
                let
                    apiUrl =
                        case Decode.decodeValue (Decode.field "apiUrl" Decode.string) flags of
                            Err _ ->
                                ""

                            Ok foundApiUrl ->
                                foundApiUrl
                in
                case maybeCachedModel of
                    Just cached ->
                        ( { cached
                            | apiUrl = apiUrl
                          }
                        , Effect.none
                        )

                    Nothing ->
                        ( { apiUrl = apiUrl
                          , authenticated = Unauthenticated
                          }
                        , Effect.none
                        )
        , update =
            \msg model ->
                case msg of
                    AuthenticationReceived tokens ->
                        ( { model
                            | authenticated =
                                Authenticated
                                    { tokens = tokens
                                    }
                          }
                        , Effect.none
                        )

                    LogOut ->
                        ( { model
                            | authenticated = Unauthenticated
                          }
                        , Effect.none
                        )
        , subscriptions = \_ -> Listen.none
        }
        -- Persist the modle to local storage
        |> App.Resource.withLocalStorage
            { decoder = decoder
            , encode = encode
            }



{- JSON encoders and decoders.

   These is used to serialize and deserialize the model to and from JSON.

-}


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( "apiUrl", Encode.string model.apiUrl )
        , ( "authenticated", encodeAuthenticated model.authenticated )
        ]


encodeAuthenticated : Authenticated -> Encode.Value
encodeAuthenticated authenticated =
    case authenticated of
        Authenticated { tokens } ->
            Encode.object
                [ ( "tokens"
                  , encodeTokens tokens
                  )
                ]

        Unauthenticated ->
            Encode.null


encodeTokens : Tokens -> Encode.Value
encodeTokens tokens =
    Encode.object
        [ ( "refresh", Encode.string tokens.refresh )
        , ( "access", Encode.string tokens.access )
        , ( "expiresAt", Encode.int (Time.posixToMillis tokens.expiresAt) )
        ]


decoder : Decode.Decoder Model
decoder =
    Decode.map2
        (\apiUrl auth ->
            { apiUrl = apiUrl
            , authenticated = auth
            }
        )
        (Decode.field "apiUrl" Decode.string)
        (Decode.field "authenticated" decodeAuthenticated)


decodeAuthenticated : Decode.Decoder Authenticated
decodeAuthenticated =
    Decode.oneOf
        [ Decode.map
            (\tokens ->
                Authenticated
                    { tokens = tokens
                    }
            )
            (Decode.field "tokens" decodeTokens)
        , Decode.succeed Unauthenticated
        ]


decodeTokens : Decode.Decoder Tokens
decodeTokens =
    Decode.map3 Tokens
        (Decode.field "refresh" Decode.string)
        (Decode.field "access" Decode.string)
        (Decode.field "expiresAt" (Decode.map Time.millisToPosix Decode.int))
