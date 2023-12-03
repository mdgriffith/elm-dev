{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Json.Encode
  ( set
  , moduleName
  , region
  , Ext.Dev.Json.Encode.maybe
  )
where

import qualified Reporting.Annotation as Ann
import qualified Data.Name as Name
import qualified Data.Set as Set

import qualified Json.Encode
import Json.Encode ((==>))

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Package



{- Primitive Helpers -}


set :: (a -> Json.Encode.Value) -> Set.Set a -> Json.Encode.Value
set encodeValue set =
  Json.Encode.list encodeValue (Set.toList set)




moduleName :: ModuleName.Canonical -> Json.Encode.Value
moduleName (ModuleName.Canonical pkgName modName) =
    Json.Encode.object 
        [ "pkg" ==> Package.encode pkgName
        , "module" ==>  Json.Encode.chars (Name.toChars modName)
        ]   

maybe :: (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
maybe toVal possibly =
  case possibly of
    Nothing -> Json.Encode.null
    Just a -> toVal a

region :: Ann.Region -> Json.Encode.Value
region (Ann.Region start end) =
    Json.Encode.object
        [ ("start" ==> position start)
        , ("end" ==> position end)
        ]


position :: Ann.Position -> Json.Encode.Value
position (Ann.Position row col) =
    Json.Encode.object
        [ ("line" ==> Json.Encode.int (fromIntegral row))
        , ("column" ==> Json.Encode.int (fromIntegral col))
        ]