{-# LANGUAGE OverloadedStrings #-}

module Watchtower.State.Discover (discover, discoverTests) where

import qualified Control.Concurrent.STM as STM
import Control.Monad as Monad
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.Dev.Project
import qualified Ext.Log
import qualified Ext.Sentry
import qualified System.Directory as Dir
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Live.Compile
import qualified Watchtower.State.Project
import qualified Watchtower.Websocket
import qualified Ext.Test.Compile
import qualified Ext.Test.Introspect
import qualified Ext.Test.Discover as TestDiscover
import qualified Control.Concurrent.STM as STM
import qualified Data.NonEmptyList as NE
import qualified Elm.ModuleName as ModuleName

discover :: Client.State -> FilePath -> IO ()
discover state root = do
  canonicalRoot <- Dir.canonicalizePath root
  Ext.Log.log Ext.Log.Live ("👀 discover requested: " <> canonicalRoot)
  projects <- Ext.Dev.Project.discover canonicalRoot

  let projectTails = zipWith (\ix proj -> show ix ++ ":" ++ getProjectShorthand canonicalRoot proj) [0..] projects

  if List.null projectTails
    then Ext.Log.log Ext.Log.Live "found no projects"
    else Ext.Log.log Ext.Log.Live (("found projects (" ++ canonicalRoot ++ ")") <> Ext.Log.formatList projectTails)

  Monad.foldM_ (initializeProject state) [] projects
  

initializeProject :: Client.State -> [Client.ProjectCache] -> Ext.Dev.Project.Project -> IO [Client.ProjectCache]
initializeProject state accum project = do
  let flags = CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput
  result <- Watchtower.State.Project.upsert state flags (Ext.Dev.Project._root project) (Ext.Dev.Project._entrypoints project)
  case result of
    Left _ ->
      pure accum
    Right projectCache ->
      do
        -- Populate tests for this project (best-effort)
        discoverTests state projectCache
        pure (projectCache : accum)

-- | Discover test suites for a project and store TestInfo on its cache
discoverTests :: Client.State -> Client.ProjectCache -> IO ()
discoverTests _state (Client.ProjectCache proj _ _ _ mTestVar) = do
  let rootDir = Ext.Dev.Project.getRoot proj
  testFiles <- TestDiscover.discoverTestFiles rootDir
  case testFiles of
    [] -> do
      STM.atomically $ STM.writeTVar mTestVar Nothing
    (x:xs) -> do
      let entrypoints = NE.List x xs
      testIfacesR <- Ext.Test.Compile.compileForDiscovery rootDir entrypoints
      case testIfacesR of
        Left reactorErr -> do
          STM.atomically $ STM.writeTVar mTestVar (Just (Client.TestInfo
            { Client.testSuites = []
            , Client.testFiles = testFiles
            , Client.testResults = Nothing
            , Client.testCompilation = Just (Client.TestError reactorErr)
            }))
        Right ifaces -> do
          suites <- Ext.Test.Introspect.findTests ifaces
          STM.atomically $ STM.writeTVar mTestVar (Just (Client.TestInfo
            { Client.testSuites = suites
            , Client.testFiles = testFiles
            , Client.testResults = Nothing
            , Client.testCompilation = Just Client.TestSuccess
            }))

getProjectShorthand :: FilePath -> Ext.Dev.Project.Project -> FilePath
getProjectShorthand root proj =
  case List.stripPrefix root (Ext.Dev.Project.getRoot proj) of
    Nothing -> "."
    Just "" -> "."
    Just str ->
      str
