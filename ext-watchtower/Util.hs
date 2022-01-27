{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
  ( encodeName,
    encodeModuleName,
    encodeModulePackage,
  )
where

import Data.Function ((&))
import qualified Data.Name as Name
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package
import Json.Encode
import Json.String

encodeName :: Name.Name -> Json.Encode.Value
encodeName name =
  Json.String.fromChars (Name.toChars name)
    & Json.Encode.string

encodeModuleName :: ModuleName.Canonical -> Json.Encode.Value
encodeModuleName can =
  can
    & ModuleName._module
    & encodeName

encodeModulePackage :: ModuleName.Canonical -> Json.Encode.Value
encodeModulePackage can =
  can
    & ModuleName._package
    & Elm.Package.toJsonString
    & Json.Encode.string
