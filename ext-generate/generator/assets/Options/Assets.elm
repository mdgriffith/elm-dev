module Options.Assets exposing
    ( AssetGroup
    , Content(..)
    , File
    , decodeAssetGroup
    )

{-| -}

import Dict
import Json.Decode
import Set exposing (Set)


type alias AssetGroup =
    { name : String
    , files : List File
    , fileInfo : { markdown : { frontmatter : Dict.Dict String String } }
    }


type alias File =
    { name : String

    -- All the directories leading to this file
    , crumbs : List String
    , pathOnServer : String
    , content : Content
    }


type Content
    = Text String
    | Binary



{- Decoders -}


decodeAssetGroup : Json.Decode.Decoder AssetGroup
decodeAssetGroup =
    Json.Decode.map3 AssetGroup
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "files" (Json.Decode.list decodeFile))
        (Json.Decode.field "fileInfo"
            (Json.Decode.map
                (\dict ->
                    { markdown = { frontmatter = dict }
                    }
                )
                (Json.Decode.field "markdown"
                    (Json.Decode.field "frontmatter"
                        (Json.Decode.dict Json.Decode.string)
                    )
                )
            )
        )


decodeFile : Json.Decode.Decoder File
decodeFile =
    Json.Decode.map4 File
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "crumbs" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "pathOnServer" Json.Decode.string)
        (Json.Decode.field "content" decodeContent)


decodeContent : Json.Decode.Decoder Content
decodeContent =
    Json.Decode.oneOf
        [ Json.Decode.map Text Json.Decode.string
        , Json.Decode.succeed Binary
        ]
