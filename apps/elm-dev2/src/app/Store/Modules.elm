module Store.Modules exposing
    ( store
    , Model, Msg(..)
    )

{-|

@docs store

@docs Model, Msg

-}

import App.Store
import Effect
import Elm.Docs
import Json.Decode
import Json.Encode
import Listen


type alias Model =
    { modules : List Elm.Docs.Module }


type Msg
    = ModulesReceived (List Elm.Docs.Module)


store : App.Store.Store Msg Model
store =
    App.Store.store
        { init =
            \flags url maybeCachedModel ->
                let
                    model =
                        -- `maybeCachedModel` is the model from localstorage
                        -- If `App.Store.withLocalStorage` is defined
                        -- and it's available
                        maybeCachedModel
                            |> Maybe.withDefault
                                { modules = [] }
                in
                ( model
                , Effect.none
                )
        , update =
            \msg model ->
                case msg of
                    ModulesReceived modules ->
                        ( { model | modules = modules }
                        , Effect.none
                        )
        , subscriptions = \_ -> Listen.none
        }
        |> App.Store.withLocalStorage
            { decoder = decoder
            , encode = encode
            }


encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "modules", Json.Encode.list encodeModule model.modules )
        ]


encodeModule : Elm.Docs.Module -> Json.Encode.Value
encodeModule module_ =
    Json.Encode.object
        [ ( "name", Json.Encode.string module_.name )
        , ( "comment", Json.Encode.string module_.comment )
        , ( "unions", Json.Encode.list encodeUnion module_.unions )
        , ( "aliases", Json.Encode.list encodeAlias module_.aliases )
        , ( "values", Json.Encode.list encodeValue module_.values )
        , ( "binops", Json.Encode.list encodeBinop module_.binops )
        ]


encodeUnion : Elm.Docs.Union -> Json.Encode.Value
encodeUnion union =
    Json.Encode.object
        [ ( "name", Json.Encode.string union.name )
        , ( "comment", Json.Encode.string union.comment )
        , ( "args", Json.Encode.list Json.Encode.string union.args )
        , ( "tags", Json.Encode.list encodeTag union.tags )
        ]


encodeTag : ( String, List Elm.Type.Type ) -> Json.Encode.Value
encodeTag ( name, args ) =
    Json.Encode.list identity
        [ Json.Encode.string name
        , Json.Encode.list Elm.Type.encode args
        ]


encodeAlias : Elm.Docs.Alias -> Json.Encode.Value
encodeAlias alias_ =
    Json.Encode.object
        [ ( "name", Json.Encode.string alias_.name )
        , ( "comment", Json.Encode.string alias_.comment )
        , ( "args", Json.Encode.list Json.Encode.string alias_.args )
        , ( "type", Elm.Type.encode alias_.tipe )
        ]


encodeValue : Elm.Docs.Value -> Json.Encode.Value
encodeValue value =
    Json.Encode.object
        [ ( "name", Json.Encode.string value.name )
        , ( "comment", Json.Encode.string value.comment )
        , ( "type", Elm.Type.encode value.tipe )
        ]


encodeBinop : Elm.Docs.Binop -> Json.Encode.Value
encodeBinop binop =
    Json.Encode.object
        [ ( "name", Json.Encode.string binop.name )
        , ( "comment", Json.Encode.string binop.comment )
        , ( "type", Elm.Type.encode binop.tipe )
        , ( "associativity", encodeAssociativity binop.associativity )
        , ( "precedence", Json.Encode.int binop.precedence )
        ]


encodeAssociativity : Elm.Docs.Associativity -> Json.Encode.Value
encodeAssociativity assoc =
    Json.Encode.string <|
        case assoc of
            Elm.Docs.Left ->
                "left"

            Elm.Docs.None ->
                "non"

            Elm.Docs.Right ->
                "right"


decoder : Json.Decode.Decoder Model
decoder =
    Json.Decode.map Model
        (Json.Decode.field "modules" (Json.Decode.list moduleDecoder))


moduleDecoder : Json.Decode.Decoder Elm.Docs.Module
moduleDecoder =
    Json.Decode.map6 Elm.Docs.Module
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "comment" Json.Decode.string)
        (Json.Decode.field "unions" (Json.Decode.list unionDecoder))
        (Json.Decode.field "aliases" (Json.Decode.list aliasDecoder))
        (Json.Decode.field "values" (Json.Decode.list valueDecoder))
        (Json.Decode.field "binops" (Json.Decode.list binopDecoder))


unionDecoder : Json.Decode.Decoder Elm.Docs.Union
unionDecoder =
    Json.Decode.map4 Elm.Docs.Union
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "comment" Json.Decode.string)
        (Json.Decode.field "args" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "tags" (Json.Decode.list tagDecoder))


tagDecoder : Json.Decode.Decoder ( String, List Elm.Type.Type )
tagDecoder =
    Json.Decode.map2 (\a b -> ( a, b ))
        (Json.Decode.index 0 Json.Decode.string)
        (Json.Decode.index 1 (Json.Decode.list Elm.Type.decoder))


aliasDecoder : Json.Decode.Decoder Elm.Docs.Alias
aliasDecoder =
    Json.Decode.map4 Elm.Docs.Alias
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "comment" Json.Decode.string)
        (Json.Decode.field "args" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "type" Elm.Type.decoder)


valueDecoder : Json.Decode.Decoder Elm.Docs.Value
valueDecoder =
    Json.Decode.map3 Elm.Docs.Value
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "comment" Json.Decode.string)
        (Json.Decode.field "type" Elm.Type.decoder)


binopDecoder : Json.Decode.Decoder Elm.Docs.Binop
binopDecoder =
    Json.Decode.map5 Elm.Docs.Binop
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "comment" Json.Decode.string)
        (Json.Decode.field "type" Elm.Type.decoder)
        (Json.Decode.field "associativity" assocDecoder)
        (Json.Decode.field "precedence" Json.Decode.int)


assocDecoder : Json.Decode.Decoder Elm.Docs.Associativity
assocDecoder =
    Json.Decode.andThen toAssoc Json.Decode.string


toAssoc : String -> Json.Decode.Decoder Elm.Docs.Associativity
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
