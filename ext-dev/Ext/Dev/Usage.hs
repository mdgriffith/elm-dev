{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Usage
  ( usageOfModule
  , UsageSummary(..)
  , encode
  )
where


{-|

Given a module name, report all usages of exposed values from that module
-}

import qualified System.IO

import qualified Control.Monad
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
import qualified Ext.Dev.Find.Canonical

import qualified Ext.CompileProxy

import qualified Json.Encode
import Json.Encode ((==>))

import Data.Name (Name)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Function ((&))

data UsageSummary =
    UsageSummary
        { _path :: FilePath
        , _usages :: [ExposedValue]
        }

data ExposedValue =
    ExposedValue
        { _name :: Name
        , _usedBy :: [ ModuleName.Raw ]
        }



usageOfModule :: String -> Elm.Details.Details -> ModuleName.Raw -> IO (Maybe UsageSummary)
usageOfModule root project mod =
  case Ext.Dev.Project.lookupModulePath project mod of
    Nothing -> pure Nothing
    Just path -> do
      let importers = Ext.Dev.Project.importersOf project mod
      usages <- Control.Monad.foldM (lookupUsage root project mod) Map.empty importers
      pure (Just (UsageSummary path (toExposedValues usages)))


toExposedValues :: Map.Map Name (Set.Set ModuleName.Raw) -> [ExposedValue]
toExposedValues foundMap =
    Map.foldrWithKey
        (\name usedBy exposedValues ->
            ExposedValue name (Set.toList usedBy) : exposedValues
        )
        []
        foundMap







{--}

lookupUsage :: String -> Elm.Details.Details -> ModuleName.Raw -> Map.Map Name (Set.Set ModuleName.Raw) ->  ModuleName.Raw -> IO (Map.Map Name (Set.Set ModuleName.Raw))
lookupUsage root details originalModule foundMap importer = do
    case Ext.Dev.Project.lookupModulePath details importer of 
        Nothing ->
            pure foundMap
        
        Just path -> do
            (Ext.CompileProxy.Single source warnings maybeInterfaces maybeCanonical compiled) <- Ext.CompileProxy.loadSingle root path
            
            case maybeCanonical of
                Nothing -> pure foundMap

                Just canonical ->
                    let 
                        usedValues = Ext.Dev.Find.Canonical.usedValues canonical
                            
                    in
                    Set.fold
                        (\(canMod, valName) innerMap ->
                            if ModuleName._module canMod == originalModule then
                                case Map.lookup valName innerMap of
                                  Nothing ->
                                    Map.insert valName (Set.singleton importer) innerMap

                                  Just existing ->
                                    Map.insert valName (Set.insert importer existing) innerMap
                            else
                                innerMap
                          

                        ) 
                        foundMap
                        usedValues
                        & pure

                 
                 
                    
                    
                    

encode :: UsageSummary -> Json.Encode.Value
encode summary =
    Json.Encode.object
        [ ( "path", Json.Encode.chars (_path summary) )
        , ( "usages", Json.Encode.list encodeExposedValue (_usages summary)) 
        ]

encodeExposedValue :: ExposedValue -> Json.Encode.Value
encodeExposedValue exposedValue =
    Json.Encode.object
        [ ( "name", Json.Encode.name (_name exposedValue))
        , ( "usedBy", Json.Encode.list Json.Encode.name (_usedBy exposedValue))
        ]