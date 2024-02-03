module Question exposing (Answer(..), TypeSignature, ask)

{-| -}

import Editor
import Http
import Json.Decode as Decode
import Url
import Url.Builder



watchtower : List String -> List Url.Builder.QueryParameter -> String
watchtower =
    Url.Builder.crossOrigin "http://localhost:51213"


ask : { missingTypesignatures : String -> Cmd (Result Http.Error Answer) }
ask =
    { missingTypesignatures =
        \path ->
            Http.get
                { url =
                    watchtower
                        [ "list-missing-signatures"
                        ]
                        [ Url.Builder.string "file" path
                        ]
                , expect =
                    Http.expectJson
                        (Result.map (MissingTypeSignatures path))
                        (Decode.list
                            decodeMissingTypesignature
                        )
                }
    }



type Answer
    = MissingTypeSignatures String (List TypeSignature)


type alias TypeSignature =
    { name : String
    , region : Editor.Region
    , signature : String
    }



decodeMissingTypesignature : Decode.Decoder TypeSignature
decodeMissingTypesignature =
    Decode.map3 TypeSignature
        (Decode.field "name" Decode.string)
        (Decode.field "region" Editor.decodeRegion)
        (Decode.field "signature" Decode.string)
