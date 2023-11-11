{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Json.Encode
  ( set
  , moduleName
  )
where


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