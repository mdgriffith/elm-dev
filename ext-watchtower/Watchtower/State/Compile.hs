{-# LANGUAGE OverloadedStrings #-}

module Watchtower.State.Compile (compile, compileRelevantProjects) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.NonEmptyList as NE
import qualified Data.Map.Strict as Map
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.CompileProxy as CompileProxy
import qualified Ext.Dev.Project
import qualified Ext.Dev.Project as Project
import qualified Ext.Sentry as Sentry
import qualified Control.Concurrent.STM as STM
import qualified Data.List as List
import qualified Ext.Common
import qualified Gen.Generate
import Json.Encode ((==>))
import qualified Json.Encode as Json
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir (withCurrentDirectory)
import qualified Watchtower.Live.Client as Client
import qualified Reporting.Warning as Warning
import qualified Watchtower.Server.DevWS as DevWS
-- no docs fetching needed from Ext.Dev; docs come from CompileProxy


compile :: Client.State -> Client.ProjectCache -> [FilePath] -> IO (Either Client.Error CompileHelpers.CompilationResult)
compile state@(Client.State _ _ mFileInfo mPackages _ _) projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot elmJsonRoot entrypoints _srcDirs _shortId) docsInfo flags mCompileResult _) files = do
  Dir.withCurrentDirectory projectRoot $ do
    -- First run code generation
    codegenResult <- Gen.Generate.run
    case codegenResult of
      Right () -> do
        let filesToCompile = NE.append files entrypoints
        -- Then run compilation, passing the optional packages TVar for caching package docs/readme
        compilationResult <- CompileProxy.compile elmJsonRoot entrypoints flags (Just mPackages)

        -- Update the compilation result TVar
        let newResult = case compilationResult of
              Right (result, _fileInfoByPath) -> Client.Success result
              Left exit -> Client.Error (Client.ReactorError exit)
        STM.atomically $ STM.writeTVar mCompileResult newResult

        -- Merge fileInfoByPath into State.fileInfo
        case compilationResult of
          Right (_result, fileInfoByPath) -> do
            STM.atomically $ do
              current <- STM.readTVar mFileInfo
              let merged = Map.foldlWithKey'
                             (\acc path info -> Map.insert path info acc)
                             current
                             fileInfoByPath
              STM.writeTVar mFileInfo merged
            -- packages TVar is now populated during compile pipeline (MemoryCached flow)
          Left _ -> do
            -- On compile failure, remove all FileInfo entries that belong to this project
            STM.atomically $ do
              current <- STM.readTVar mFileInfo
              let filtered = Map.filterWithKey (\path _ -> not (Ext.Dev.Project.contains path proj)) current
              STM.writeTVar mFileInfo filtered

        case compilationResult of
          Right (result, _) -> do
            -- Broadcast success to Dev websocket clients
            case result of
              CompileHelpers.CompiledJs jsBuilder ->
                DevWS.broadcastCompiled state (Client.builderToString jsBuilder)
              CompileHelpers.CompiledHtml _ ->
                pure ()
              CompileHelpers.CompiledSkippedOutput ->
                pure ()
            pure (Right result)
          Left exit -> do
            -- Broadcast error to Dev websocket clients
            let clientErr = Client.ReactorError exit
            let errJson = Client.encodeCompilationResult (Client.Error clientErr)
            DevWS.broadcastCompilationError state errJson
            pure (Left clientErr)
      Left err -> do
        -- Update compile result TVar with the error
        STM.atomically $ STM.writeTVar mCompileResult (Client.Error (Client.GenerationError err))
        -- On generation failure, also clear FileInfo entries for this project
        STM.atomically $ do
          current <- STM.readTVar mFileInfo
          let filtered = Map.filterWithKey (\path _ -> not (Ext.Dev.Project.contains path proj)) current
          STM.writeTVar mFileInfo filtered
        -- Broadcast generation error to Dev websocket clients
        let clientErr = Client.GenerationError err
        let errJson = Client.encodeCompilationResult (Client.Error clientErr)
        DevWS.broadcastCompilationError state errJson
        pure $ Left clientErr


-- | Compile any projects that are relevant to the given file paths.
--   A project is considered relevant if it contains at least one of the provided files.
--   Compilation is performed synchronously here so the caller can rely on
--   fresh results when this function returns.
compileRelevantProjects :: Client.State -> [FilePath] -> IO ()
compileRelevantProjects state@(Client.State _ mProjects _ _ _ _) elmFiles = do
  if elmFiles == []
    then pure ()
    else do
      projects <- STM.readTVarIO mProjects
      let relevant = List.filter (projectTouchesAny elmFiles) projects
      case relevant of
        [] -> pure ()
        _ ->
          Ext.Common.track "compile relevant projects" $ do
            counter <- STM.newTVarIO (List.length relevant)
            let runOne projCache = do
                  compileProjectFiles elmFiles projCache
                  STM.atomically $ do
                    n <- STM.readTVar counter
                    STM.writeTVar counter (n - 1)
            mapM_ (\proj -> Ext.Common.trackedForkIO (runOne proj)) relevant
            STM.atomically $ do
              n <- STM.readTVar counter
              STM.check (n == 0)
  where
    projectTouchesAny :: [FilePath] -> Client.ProjectCache -> Bool
    projectTouchesAny paths (Client.ProjectCache proj _ _ _ _) =
      any (\p -> Ext.Dev.Project.contains p proj) paths

    compileProjectFiles :: [FilePath] -> Client.ProjectCache -> IO ()
    compileProjectFiles paths projCache@(Client.ProjectCache proj _ _ _ _) = do
      let projectFiles = List.filter (\p -> Ext.Dev.Project.contains p proj) paths
      _ <- compile state projCache projectFiles
      pure ()
