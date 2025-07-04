module Options exposing (Options, decoder)

import Elm.Docs
import Json.Decode


type alias Options =
    { project : List Elm.Docs.Module
    , viewers : List Elm.Docs.Module
    }


decoder : Json.Decode.Decoder Options
decoder =
    Json.Decode.map2 Options
        (Json.Decode.field "project"
            (Json.Decode.list Elm.Docs.decoder)
        )
        (Json.Decode.field "viewers"
            (Json.Decode.list Elm.Docs.decoder)
        )
