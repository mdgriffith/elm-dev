{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Llamadera (allInterfaces, getProjectRoot, loadProject, loadFileSource, loadSingleArtifacts, formatHaskellValue, hindentPrintValue, debug_) where

import qualified System.Directory as Dir
import Prelude hiding (lookup)


import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar
import Control.Monad (liftM2, unless)

import qualified System.Environment as Env
import qualified System.Process
import qualified Text.Show.Unicode
import qualified Data.Text as T
import Data.Text (Text)


import qualified AST.Optimized as Opt
import qualified AST.Canonical as Can
import qualified AST.Source as Src
import qualified Elm.Interface as I
import qualified Elm.Details as Details
import qualified Elm.Package as Pkg
import qualified Elm.ModuleName as ModuleName
import qualified Compile
import qualified Parse.Module as Parse
import qualified BackgroundWriter as BW
import qualified Ext.FileCached as File
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Reporting.Error
import qualified Reporting.Error.Syntax
import qualified System.Exit
import qualified Build
import qualified Stuff
import System.FilePath as FP ((</>), joinPath, splitDirectories, takeDirectory)
import StandaloneInstances

import Ext.Common


{- INTERFACES -}

allInterfaces :: [FilePath] -> IO (Map.Map ModuleName.Raw I.Interface)
allInterfaces paths = do
  artifactsDeps <- allDepArtifacts
  ifacesProject <-
    case paths of
      [] -> error "Lamdera.Interfaces.all must have at least one path specified!"
      path:paths -> allProjectInterfaces (NE.List path paths)

  pure $ Map.union ifacesProject (_ifaces artifactsDeps)




data Artifacts =
  Artifacts
    { _ifaces :: Map.Map ModuleName.Raw I.Interface
    , _graph :: Opt.GlobalGraph
    }

{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!
-}
allDepArtifacts :: IO Artifacts
allDepArtifacts =
  BW.withScope $ \scope ->
  do  --debug "Loading allDeps"
      let style = Reporting.silent
      root <- getProjectRoot
      result <- Details.load style scope root
      case result of
        Left _ ->
          error $ "Ran into some problem loading elm.json\nTry running `elm make` in: " ++ root

        Right details ->
          do  omvar <- Details.loadObjects root details
              imvar <- Details.loadInterfaces root details
              mdeps <- readMVar imvar
              mobjs <- readMVar omvar
              case liftM2 (,) mdeps mobjs of
                Nothing ->
                  error $ "Ran into some weird problem loading elm.json\nTry running `elm make` in: " ++ root

                Just (deps, objs) ->
                  return $ Artifacts (toInterfaces deps) objs


toInterfaces :: Map.Map ModuleName.Canonical I.DependencyInterface -> Map.Map ModuleName.Raw I.Interface
toInterfaces deps =
  Map.mapMaybe toUnique $ Map.fromListWith OneOrMore.more $
    Map.elems (Map.mapMaybeWithKey getPublic deps)


getPublic :: ModuleName.Canonical -> I.DependencyInterface -> Maybe (ModuleName.Raw, OneOrMore.OneOrMore I.Interface)
getPublic (ModuleName.Canonical _ name) dep =
  case dep of
    I.Public  iface -> Just (name, OneOrMore.one iface)
    I.Private _ _ _ -> Nothing


toUnique :: OneOrMore.OneOrMore a -> Maybe a
toUnique oneOrMore =
  case oneOrMore of
    OneOrMore.One value -> Just value
    OneOrMore.More _ _  -> Nothing




allProjectInterfaces :: NE.List FilePath -> IO (Map.Map ModuleName.Raw I.Interface)
allProjectInterfaces paths =
  BW.withScope $ \scope -> do
    root <- getProjectRoot
    runTaskUnsafe $
      do  details    <- Task.eio Exit.ReactorBadDetails $ Details.load Reporting.silent scope root
          artifacts  <- Task.eio Exit.ReactorBadBuild $ Build.fromPaths Reporting.silent root details paths

          Task.io $ extractInterfaces $ Build._modules artifacts

runTaskUnsafe :: Task.Task Exit.Reactor a -> IO a
runTaskUnsafe task = do
  result <- Task.run task
  case result of
    Right a ->
      return a

    Left exit ->
      do  --Exit.toStderr (Exit.reactorToReport exit)
          error
            "\n-------------------------------------------------\
            \\nError in task, please report this.\
            \\n-------------------------------------------------\
            \\n"

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



{- END INTERFACES -}

{-|

Compile.Artifacts is defined as


  data Artifacts =
    Artifacts
      { _modul :: Can.Module
      , _types :: Map.Map Name.Name Can.Annotation
      , _graph :: Opt.LocalGraph
      }


- Can.Module is the canonicalized AST!
- A map of all the types
- and



-}
loadSingleArtifacts :: FilePath -> FilePath -> IO (Either Reporting.Error.Error Compile.Artifacts)
loadSingleArtifacts root path =
  Dir.withCurrentDirectory root $ do
    ifaces <- allInterfaces [path]
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right modul ->
        pure $ Compile.compile Pkg.dummyName ifaces modul

      Left err ->
        pure $ Left $ Reporting.Error.BadSyntax err


loadFileSource :: FilePath -> FilePath -> IO (Either Reporting.Error.Syntax.Error (BS.ByteString, Src.Module))
loadFileSource root path = do
  Dir.withCurrentDirectory root $ do
    source <- File.readUtf8 path
    case Parse.fromByteString Parse.Application source of
      Right modul -> do
        -- hindentPrintValue "module source" modul
        pure $ Right (source, modul)

      Left err ->
        pure $ Left err




{- Appropriated from worker/src/Artifacts.hs
   WARNING: does not load any user code!!!


We generally do this when we want a mapping of modulenames to filepaths
-}
loadProject :: IO Details.Details
loadProject =
  BW.withScope $ \scope ->
  do  --debug "Loading allDeps"
      let style = Reporting.silent
      root <- getProjectRoot
      result <- Details.load style scope root
      case result of
        Left _ ->
          error $ "Ran into some problem loading elm.json\nTry running `elm make` in: " ++ root

        Right details ->
          return details



{-|

Useful when trying to understand AST values or just unknown values in general.

Requires hindent to be installed; try stack install hindent

Most conveniently used like so;

{-# LANGUAGE BangPatterns #-}

let
  !_ = formatHaskellValue "some sensible label" (blah) :: IO ()
in
blah

The bang pattern forces evaluation and you don't have to worry about the type-context, i.e. not being in IO.

-}
formatHaskellValue label v =
  unsafePerformIO $ do
    hindentPrintValue label v
    pure $ pure ()


hindentPrintValue :: Show a => Text -> a -> IO a
hindentPrintValue label v = do
  let
    input = Text.Show.Unicode.ushow v

  -- if Prelude.length input > 10000
  --   then
  --     atomicPutStrLn $ "❌SKIPPED display, value show > 10,000 chars, here's a clip:\n" <> Prelude.take 1000 input
  --   else do
  (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "hindent" ["--line-length","150"] input
  if Prelude.length stderr > 0
    then
      atomicPutStrLn $
        "\n🔶--------------------------------------------------------------------------------"
          <> T.unpack label
          <> "\n"
          <> stderr
          <> "\n📥 for input: \n"
          <> input

    else
      atomicPutStrLn $
        "\n🔶--------------------------------------------------------------------------------"
          <> T.unpack label
          <> "\n"
          <> stdout


  pure v


hindentFormatValue :: Show a => a -> Text
hindentFormatValue v =
  unsafePerformIO $ do
    (exit, stdout, stderr) <- System.Process.readProcessWithExitCode "hindent" ["--line-length","150"] (Text.Show.Unicode.ushow v)
    pure $ T.pack stdout


debug_ :: String -> IO ()
debug_ str = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> atomicPutStrLn $ "DEBUG: " ++ str ++ "\n"
    Nothing -> pure ()


-- Inversion of `unless` that runs IO only when condition is True
onlyWhen :: Monad f => Bool -> f () -> f ()
onlyWhen condition io =
  unless (not condition) io


withDefault :: a -> Maybe a -> a
withDefault default_ m =
  case m of
    Just v -> v
    Nothing -> default_
