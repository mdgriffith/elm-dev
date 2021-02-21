{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Details where

import qualified Reporting.Annotation as Ann
import qualified Json.Decode
import qualified Json.String
import qualified Json.Encode
import Json.Encode ((==>))
import Control.Applicative ((<|>), (<$>), (<*>))

data Status
    = Success
    | Failing [ Error ]


data Location =
    Location 
        { _file :: FilePath
        , _position :: Ann.Position
        }

data Position = Position
data Range = Range
data Callgraph = CallGraph
data Error = Error


data Visible =
    Visible 
        { _active :: Maybe Editor
        , _visible :: [ Editor ]
        }


data Editor =
    Editor
        { _path :: FilePath
        , _visibleRanges :: [ Ann.Region ]
        , _selection :: [ Ann.Region ]
        }    


-- ENCODERS

encodeVisible :: Visible -> Json.Encode.Value
encodeVisible (Visible active vis) =
    Json.Encode.object 
        [ ("active" ==>
                (case active of
                    Nothing ->
                        Json.Encode.null
                    Just edit ->
                        encodeEditor edit
                )
            )
        , ("visible" ==> Json.Encode.list encodeEditor vis)
        ]

encodeEditor :: Editor -> Json.Encode.Value
encodeEditor (Editor path vis sel) =
    Json.Encode.object 
        [ ("path" ==> Json.Encode.string (Json.String.fromChars path))
        , ("visibleRegions" ==> Json.Encode.list encodeRegion vis)
        , ("selections" ==> Json.Encode.list encodeRegion sel)
        ]

encodeRegion :: Ann.Region -> Json.Encode.Value
encodeRegion (Ann.Region start end) =
    Json.Encode.object 
        [ ("start" ==> encodePosition start)
        , ("end" ==> encodePosition end)
        ]  


encodeLocation :: Location -> Json.Encode.Value
encodeLocation (Location file pos) =
    Json.Encode.object 
        [ ("file" ==> Json.Encode.string (Json.String.fromChars file))
        , ("pos" ==> encodePosition pos)
        ]

encodePosition :: Ann.Position -> Json.Encode.Value
encodePosition (Ann.Position row col) =
    Json.Encode.object 
        [ ("row" ==> Json.Encode.int (fromIntegral row))
        , ("col" ==> Json.Encode.int (fromIntegral col))
        ]


-- DECODERS

decodeVisible :: Json.Decode.Decoder x Visible
decodeVisible =
    Visible
        <$> (Json.Decode.field "active" 
                (Json.Decode.oneOf 
                    [ Just <$> decodeEditor
                    , pure (Nothing)
                    ]
                )
            )
        <*> (Json.Decode.field "visible" 
                (Json.Decode.list decodeEditor)
            )

decodeEditor :: Json.Decode.Decoder x Editor
decodeEditor =
    Editor 
        <$> (Json.Decode.field "path" (Json.String.toChars <$> Json.Decode.string))
        <*> (Json.Decode.field "visibleRegions" (Json.Decode.list decodeRegion))
        <*> (Json.Decode.field "selections" (Json.Decode.list decodeRegion))


decodeLocation :: Json.Decode.Decoder x Location
decodeLocation =
    Location 
        <$> (Json.Decode.field "file" (Json.String.toChars <$> Json.Decode.string))
        <*> (Json.Decode.field "pos" decodePosition)


decodeRegion :: Json.Decode.Decoder x Ann.Region
decodeRegion =
    Ann.Region
        <$> (Json.Decode.field "start" decodePosition)
        <*> (Json.Decode.field "end" decodePosition)


decodePosition :: Json.Decode.Decoder x Ann.Position
decodePosition =
    Ann.Position
        <$> (Json.Decode.field "row" (fromIntegral <$> Json.Decode.int))
        <*> (Json.Decode.field "col" (fromIntegral <$> Json.Decode.int))