{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Editor where

{-| Data for communicating positions with the editor


-}

import qualified Reporting.Annotation as Ann
import qualified Json.Decode
import qualified Json.String
import qualified Json.Encode
import Json.Encode ((==>))
import Control.Applicative ((<|>), (<$>), (<*>))

import StandaloneInstances


data PointLocation =
    PointLocation
        { _pointfile :: FilePath
        , _position :: Ann.Position
        }




-- ENCODERS


encodeRegion :: Ann.Region -> Json.Encode.Value
encodeRegion (Ann.Region start end) =
    Json.Encode.object
        [ ("start" ==> encodePosition start)
        , ("end" ==> encodePosition end)
        ]


encodePosition :: Ann.Position -> Json.Encode.Value
encodePosition (Ann.Position row col) =
    Json.Encode.object
        [ ("line" ==> Json.Encode.int (fromIntegral row))
        , ("column" ==> Json.Encode.int (fromIntegral col))
        ]