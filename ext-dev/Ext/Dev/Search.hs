{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Search
  ( search
  , SearchResult(..)
  )
where


{-|

## These modules are all very similar:

Ext.Dev.Find -> Source Position -> Definition
Ext.Dev.Lookup -> Module -> Value.Name -> Definition
Ext.Dev.Search -> Module -> Value.Name -> Maybe { definition : Definition, usages : [ Source Position ]}

-}

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Reporting.Annotation as A
import qualified Parse.Primitives as P
import qualified Watchtower.Editor
import qualified Data.List as List

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Details
import qualified Ext.Dev.Project
import qualified Ext.Dev.Lookup

import qualified Ext.CompileProxy

import Data.Name (Name)
import qualified Data.Map as Map
import Data.Function ((&))

data SearchResult =
    SearchResult
        { _definition :: Ext.Dev.Lookup.LookupResult
        -- , _usages :: [Src.Position]
        }


search :: String -> ModuleName.Raw -> Name -> IO (Maybe SearchResult)
search root mod valueName =
    do
       project <- Ext.CompileProxy.loadProject root

       importers <- Ext.Dev.Project.importersOf project mod

       found <- Ext.Dev.Lookup.lookupDefinition root mod valueName
       pure (SearchResult <$> found)

