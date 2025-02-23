module Options exposing (Options, decoder)

import Elm.Docs
import Json.Decode


type alias Options =
    { output : String
    , project : List Elm.Docs.Module
    , viewers : List Elm.Docs.Module
    }


decoder : Json.Decode.Decoder Options
decoder =
    Json.Decode.map3 Options
        (Json.Decode.field "output" Json.Decode.string)
        (Json.Decode.field "project"
            (Json.Decode.list Elm.Docs.decoder)
        )
        (Json.Decode.field "viewers"
            (Json.Decode.list Elm.Docs.decoder)
        )
