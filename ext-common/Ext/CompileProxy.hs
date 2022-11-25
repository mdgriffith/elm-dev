{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.CompileProxy
 ( loadSingle, SingleFileResult(..)
 , loadFileSource
 , compileSrcModule
 , loadCanonicalizeEnv
 , loadProject
 , parse
 , compileToJson
 )
 where

{- This is a proxy for all compilation related functions
   that ensures we can transparently swap compilation providers/methods
   (i.e. Disk vs MemoryCached)
 -}

import Control.Concurrent.MVar
import Ext.Common
import Ext.CompileMode (getMode, CompileMode(..))
import Json.Encode ((==>))
import qualified Control.Monad (foldM_)
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified BackgroundWriter as BW
import qualified Build
import qualified Canonicalize.Module as Canonicalize
import qualified Compile
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Name as Name (Name, toChars)
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set
import qualified Elm.Docs as Docs
import qualified Elm.Details as Details
import qualified Elm.Interface as I
import qualified Elm.Package as Pkg
import qualified Ext.CompileHelpers.Disk
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Elm.ModuleName as ModuleName
import qualified Ext.CompileHelpers.Memory
import qualified Ext.FileProxy as File
import qualified Json.Encode as Encode
import qualified Optimize.Module as Optimize
import qualified Parse.Module as Parse
import qualified Nitpick.PatternMatches
import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Reporting.Error
import qualified Reporting.Error.Syntax
import qualified Reporting.Exit as Exit
import qualified Reporting.Render.Type.Localizer as Localizer
import qualified Reporting.Result
import qualified Reporting.Task as Task
import qualified Reporting.Warning as Warning
import qualified Stuff
import qualified System.Directory as Dir
import System.IO.Unsafe (unsafePerformIO)

import StandaloneInstances
import qualified Canonicalize.Environment
import qualified Canonicalize.Environment.Foreign
import qualified Reporting.Error.Canonicalize
import Data.OneOrMore (OneOrMore(..))
import qualified Canonicalize.Environment.Local
import qualified Ext.Log





type AggregateStatistics = Map.Map CompileMode Double


{-# NOINLINE aggregateStatistics #-}
aggregateStatistics :: MVar AggregateStatistics
aggregateStatistics = unsafePerformIO $
  let blank :: AggregateStatistics = Map.fromList [ (Disk, 0) , (Memory, 0) ]
  in
  newMVar blank


{-# NOINLINE addToAggregate #-}
addToAggregate mode t label = do
  Ext.Log.log Ext.Log.Performance $  label ++ " +" ++ show t
  modifyMVar_ aggregateStatistics (\agg -> pure $ Map.update (\existing -> Just $ existing + t) mode agg )


aggregateSummary :: IO String
aggregateSummary = do
  x <- readMVar aggregateStatistics
  pure $ show x


modeRunner identifier ioDisk ioMemory = do
  -- Ext.Log.log Ext.Log.Performance $ concat ["👁 compileProxy:", identifier ]
  case getMode of
    Disk -> do
      (t, label, result) <- Ext.Common.track_ ("🎻 classic   " ++ identifier) $ ioDisk
      addToAggregate Disk t label
      summary <- aggregateSummary
      Ext.Log.log Ext.Log.Performance $ summary
      File.debugSummary
      pure result
    Memory -> do
      (t, label, result) <- Ext.Common.track_ ("🧠 memcached " ++ identifier) $ ioMemory
      addToAggregate Memory t label
      summary <- aggregateSummary
      Ext.Log.log Ext.Log.Performance $  summary
      File.debugSummary
      pure result
    Race -> do
      results <- Ext.Common.race identifier [ ("🧠 memcached " ++ identifier, ioMemory) , ("🎻 classic   " ++ identifier, ioDisk) ]
      results & zip [Memory, Disk] & mapM_ (\(m, (t, l, r)) -> addToAggregate m t (l ++ " " ++ identifier))
      summary <- aggregateSummary
      Ext.Log.log Ext.Log.Performance $ summary
      File.debugSummary
      (results !! 1) & (\(_,_,x) -> x) & pure


-- Interfaces


compileToJson :: FilePath -> NE.List FilePath -> IO (Either Encode.Value Encode.Value)
compileToJson root paths = do
  res <- modeRunner "compileToJson"
    (Ext.CompileHelpers.Disk.compileToJson root paths)
    (Ext.CompileHelpers.Memory.compileToJson root paths)
  pure res


allPackageArtifacts :: FilePath -> IO CompileHelpers.Artifacts
allPackageArtifacts root =
  modeRunner "allPackageArtifacts"
    (Ext.CompileHelpers.Disk.allPackageArtifacts root)
    (Ext.CompileHelpers.Memory.allPackageArtifacts root)


allInterfaces :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor (Map.Map ModuleName.Raw I.Interface))
allInterfaces root paths =
  Dir.withCurrentDirectory root $
    BW.withScope $ \scope -> Stuff.withRootLock root $
      Task.run $
        do  details    <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
            artifacts  <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details paths

            Task.io $ extractInterfaces $ Build._modules artifacts


loadFileSource :: FilePath -> FilePath -> IO (Either Reporting.Error.Syntax.Error (BS.ByteString, Src.Module))
loadFileSource root path = do
  Dir.withCurrentDirectory root $ do
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right modul -> do
        pure $ Right (source, modul)

      Left err ->
        pure $ Left err

compileSrcModule :: FilePath -> FilePath -> Src.Module -> IO (Maybe Compile.Artifacts)
compileSrcModule root path srcMod =
  Dir.withCurrentDirectory root $ do
    ifacesResult <- allInterfaces root (NE.List path [])
    case ifacesResult of
      Left _ ->
        pure Nothing

      Right ifaces ->
        pure $ toMaybe $ Compile.compile Pkg.dummyName ifaces srcMod

toMaybe :: Either x a -> Maybe a
toMaybe either =
  case either of
    Right a -> Just a
    Left _ -> Nothing



loadCanonicalizeEnv ::
  FilePath ->
  FilePath ->
  Src.Module ->
  IO (Maybe Canonicalize.Environment.Env)
loadCanonicalizeEnv root path srcMod = do
  Dir.withCurrentDirectory root $ do
    ifacesResult <- allInterfaces root (NE.List path [])
    case ifacesResult of
      Left _ ->
        pure Nothing

      Right ifaces -> do
        let home = ModuleName.Canonical Pkg.dummyName $ Src.getName srcMod

        let (_, eitherResult) = Reporting.Result.run $
              Canonicalize.Environment.Local.add srcMod =<<
                Canonicalize.Environment.Foreign.createInitialEnv home ifaces (Src._imports srcMod)

        case eitherResult of
          Left err ->
            pure $ Nothing
          Right (env, _, _) ->
            pure $ Just env

{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!

   We generally do this when we want a mapping of modulenames to filepaths
-}
loadProject :: IO Details.Details
loadProject =
  BW.withScope $ \scope ->
  do  
      let style = Reporting.silent
      root <- getProjectRoot
      result <- Details.load style scope root
      case result of
        Left _ ->
          error $ "Ran into some problem loading elm.json\nTry running `elm make` in: " ++ root

        Right details ->
          return details


parse :: FilePath -> FilePath -> IO (Either Reporting.Error.Syntax.Error Src.Module)
parse root path =
  Dir.withCurrentDirectory root $ do
    source <- File.readUtf8 path
    return $ Parse.fromByteString Parse.Application source


data SingleFileResult =
    Single 
      { _source :: Either Reporting.Error.Syntax.Error Src.Module
      , _warnings :: Maybe [ Warning.Warning ]
      , _canonical :: Maybe Can.Module
      , _compiled :: Maybe (Either Reporting.Error.Error Compile.Artifacts)
      }


-- @TODO this is a disk mode function
loadSingle :: FilePath -> FilePath -> IO SingleFileResult
loadSingle root path =
  Dir.withCurrentDirectory root $ do
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right srcModule ->
        do
        
          ifacesResult <- allInterfaces root (NE.List path [])
          (CompileHelpers.Artifacts packageIfaces globalGraph) <- allPackageArtifacts root
          case ifacesResult of
            Left exit ->
              -- report exit : Exit.Reactor?
              pure
                (Single 
                  (Right srcModule)
                  Nothing
                  Nothing
                  Nothing
                )

            Right localIfaces -> do
                let ifaces = Map.union localIfaces packageIfaces
                let (canWarnings, eitherCanned) = Reporting.Result.run $ Canonicalize.canonicalize Pkg.dummyName ifaces srcModule
                case eitherCanned of
                  Left errs -> 
                    pure
                      (Single 
                        (Right srcModule)
                        (Just canWarnings)
                        Nothing
                        (Just (Left (Reporting.Error.BadNames errs)))
                      ) 

                  Right canModule ->
                      case CompileHelpers.typeCheck srcModule canModule of
                        Left typeErrors ->
                            pure
                              (Single 
                                (Right srcModule)
                                (Just canWarnings)
                                (Just canModule)
                                (Just (Left (Reporting.Error.BadTypes (Localizer.fromModule srcModule) typeErrors)))
                              ) 

                        Right annotations ->
                          do
                            let nitpicks = Nitpick.PatternMatches.check canModule

                            let (optWarnings, eitherLocalGraph) = Reporting.Result.run $ Optimize.optimize annotations canModule
                            case eitherLocalGraph of
                              Left errs ->
                                pure
                                  (Single 
                                    (Right srcModule)
                                    (Just (canWarnings <> optWarnings))
                                    (Just canModule)
                                    (Just (Left (Reporting.Error.BadMains (Localizer.fromModule srcModule) errs)))
                                  ) 

                              Right localGraph -> do
                                pure
                                  (Single 
                                    (Right srcModule)
                                    (Just (canWarnings <> optWarnings))
                                    (Just canModule)
                                    (Just 
                                      (case nitpicks of 
                                        Right () ->
                                            Right (Compile.Artifacts canModule annotations localGraph)
                                        
                                        Left errors ->
                                            Left (Reporting.Error.BadPatterns errors)
                                        
                                      )
                                    )
                                  ) 

      Left err ->
        pure
          (Single 
            (Left err)
            Nothing
            Nothing
            Nothing
          ) 


-- Helpers

extractInterfaces :: [Build.Module] -> IO (Map.Map ModuleName.Raw I.Interface)
extractInterfaces modu = do
  k <- modu
    & mapM (\m ->
      case m of
        Build.Fresh nameRaw ifaces _ ->
          pure $ Just (nameRaw, ifaces)
        Build.Cached name _ mCachedInterface ->
          cachedHelp name mCachedInterface
    )
  pure $ Map.fromList $ justs k


{- Appropriated from Build.loadInterface -}
cachedHelp :: ModuleName.Raw -> MVar Build.CachedInterface -> IO (Maybe (ModuleName.Raw, I.Interface))
cachedHelp name ciMvar = do
  cachedInterface <- takeMVar ciMvar
  case cachedInterface of
    Build.Corrupted ->
      do  putMVar ciMvar cachedInterface
          return Nothing

    Build.Loaded iface ->
      do  putMVar ciMvar cachedInterface
          return (Just (name, iface))

    Build.Unneeded ->
      do  root <- getProjectRoot
          maybeIface <- File.readBinary (Stuff.elmi root name)
          case maybeIface of
            Nothing ->
              do  putMVar ciMvar Build.Corrupted
                  return Nothing

            Just iface ->
              do  putMVar ciMvar (Build.Loaded iface)
                  return (Just (name, iface))