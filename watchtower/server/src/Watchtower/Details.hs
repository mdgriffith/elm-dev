{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Details where

import qualified Reporting.Annotation as Ann


data Status
    = Success
    | Failing [ Error ]


data Location =
    Location 
        { _path :: FilePath
        , _range :: Ann.Region
        }

data Position = Position
data Range = Range
data Callgraph = CallGraph
data Error = Error