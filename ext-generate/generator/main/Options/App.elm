module Options.App exposing
    ( Options
    , PageUsage
    , Store
    , decode
    )

{-| -}

import Json.Decode
import Options.Route
import Set exposing (Set)


type alias Options =
    { pages : List PageUsage
    , stores : List Store
    }


type alias PageUsage =
    { id : String
    , moduleName : List String
    , value : String
    , paramType : Maybe String
    , elmModuleIsPresent : Bool
    , urlOnly : Bool

    --
    , route : Maybe Options.Route.ParsedPage
    }


type alias Store =
    { id : String
    }


decode : Json.Decode.Decoder Options
decode =
    Json.Decode.map2 Options
        (Json.Decode.field "pages" decodePageUsages)
        (Json.Decode.field "stores" (Json.Decode.list decodeStore))


decodeStore : Json.Decode.Decoder Store
decodeStore =
    Json.Decode.map Store
        (Json.Decode.field "id" Json.Decode.string)


decodePageUsages : Json.Decode.Decoder (List PageUsage)
decodePageUsages =
    Json.Decode.list
        (Json.Decode.map7 PageUsage
            (Json.Decode.field "id" Json.Decode.string)
            (Json.Decode.field "moduleName" (Json.Decode.list Json.Decode.string))
            (Json.Decode.field "value" Json.Decode.string)
            (Json.Decode.field "paramType"
                (Json.Decode.oneOf
                    [ Json.Decode.map Just Json.Decode.string
                    , Json.Decode.null Nothing
                    ]
                )
            )
            (Json.Decode.field "elmModuleIsPresent" Json.Decode.bool)
            (Json.Decode.field "urlOnly" Json.Decode.bool)
            (Json.Decode.maybe (Json.Decode.field "route" Options.Route.decodePage))
        )
