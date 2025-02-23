module Options.Docs exposing (Docs, decoder)

{-| -}

import Dict
import Elm.Docs
import Elm.Project
import Elm.Type
import Json.Decode exposing (Decoder)
import Json.Encode


type alias Docs =
    { readme : Maybe String
    , guides : List Guide
    , project : Elm.Project.Project
    , modules : List Elm.Docs.Module
    , deps : Dict.Dict String (List Elm.Docs.Module)
    }


type alias Guide =
    { name : String
    , content : Maybe String
    }



{- Decoders -}


decoder : Json.Decode.Decoder Docs
decoder =
    Json.Decode.map5 Docs
        (Json.Decode.field "readme" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "guides" (Json.Decode.list decodeGuide))
        (Json.Decode.field "project" Elm.Project.decoder)
        (Json.Decode.field "modules" (Json.Decode.list decodeModule))
        (Json.Decode.field "deps" (Json.Decode.dict (Json.Decode.list Elm.Docs.decoder)))


safeList : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
safeList tag innerDecoder =
    Json.Decode.oneOf
        [ Json.Decode.list innerDecoder
        , Json.Decode.succeed []
            |> Json.Decode.andThen
                (\_ ->
                    let
                        _ =
                            Debug.log tag "Failed!"
                    in
                    Json.Decode.succeed []
                )
        ]


safeString : String -> Json.Decode.Decoder String
safeString tag =
    Json.Decode.oneOf
        [ Json.Decode.string
        , Json.Decode.succeed ""
            |> Json.Decode.andThen
                (\_ ->
                    let
                        _ =
                            Debug.log tag "Failed string!"
                    in
                    Json.Decode.succeed ""
                )
        ]


decodeGuide : Json.Decode.Decoder Guide
decodeGuide =
    Json.Decode.map2 Guide
        (Json.Decode.field "path" Json.Decode.string)
        (Json.Decode.field "contents" (Json.Decode.maybe Json.Decode.string))


{-| Decode the JSON documentation produced by `elm-make` for an individual
module. The documentation for a whole package is an array of module docs,
so you may need to say `(Decode.list Docs.decoder)` depending on what you
want to do.
-}
decodeModule : Decoder Elm.Docs.Module
decodeModule =
    Json.Decode.oneOf
        [ Json.Decode.map6 Elm.Docs.Module
            (Json.Decode.field "name" (safeString "name"))
            (Json.Decode.field "comment" (safeString "comment"))
            (Json.Decode.field "unions" (safeList "unions" unionDecoder))
            (Json.Decode.field "aliases" (safeList "alias" aliasDecoder))
            (Json.Decode.field "values" (safeList "values" valueDecoder))
            (Json.Decode.field "binops" (safeList "binops" binopDecoder))
        , Json.Decode.value
            |> Json.Decode.andThen
                (\value ->
                    let
                        _ =
                            Debug.log "Failed!" (Json.Encode.encode 4 value)
                    in
                    Json.Decode.succeed (Elm.Docs.Module "FAILED" "" [] [] [] [])
                )
        ]


aliasDecoder : Decoder Elm.Docs.Alias
aliasDecoder =
    Json.Decode.map4 Elm.Docs.Alias
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "comment" Json.Decode.string)
        (Json.Decode.field "args" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "type" Elm.Type.decoder)


unionDecoder : Decoder Elm.Docs.Union
unionDecoder =
    Json.Decode.map4 Elm.Docs.Union
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "comment" Json.Decode.string)
        (Json.Decode.field "args" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "cases" (Json.Decode.list tagDecoder))


tagDecoder : Decoder ( String, List Elm.Type.Type )
tagDecoder =
    Json.Decode.map2 (\a b -> ( a, b ))
        (Json.Decode.index 0 Json.Decode.string)
        (Json.Decode.index 1 (Json.Decode.list Elm.Type.decoder))


valueDecoder : Decoder Elm.Docs.Value
valueDecoder =
    Json.Decode.map3 Elm.Docs.Value
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "comment" Json.Decode.string)
        (Json.Decode.field "type" Elm.Type.decoder)


binopDecoder : Decoder Elm.Docs.Binop
binopDecoder =
    Json.Decode.map5 Elm.Docs.Binop
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "comment" Json.Decode.string)
        (Json.Decode.field "type" Elm.Type.decoder)
        (Json.Decode.field "associativity" assocDecoder)
        (Json.Decode.field "precedence" Json.Decode.int)


assocDecoder : Decoder Elm.Docs.Associativity
assocDecoder =
    Json.Decode.andThen toAssoc Json.Decode.string


toAssoc : String -> Decoder Elm.Docs.Associativity
toAssoc str =
    case str of
        "left" ->
            Json.Decode.succeed Elm.Docs.Left

        "non" ->
            Json.Decode.succeed Elm.Docs.None

        "right" ->
            Json.Decode.succeed Elm.Docs.Right

        _ ->
            Json.Decode.fail "expecting one of the following values: left, non, right"
