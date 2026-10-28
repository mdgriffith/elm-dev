module Support.TestApp exposing (defaults)

import Json.Encode
import Main
import Time


defaults =
    { app = Main.config
    , httpTarget = Main.httpTarget
    , url = "https://example.test/"
    , flags = Json.Encode.object []
    , time = Time.millisToPosix 0
    , zone = Time.utc
    , zoneName = Time.Name "UTC"
    }
