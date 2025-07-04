{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live.Compile (compileAll, recompile) where

import qualified Control.Concurrent.STM as STM
import Control.Monad as Monad (foldM, guard, mapM_)
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NE
import qualified Data.NonEmptyList as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Ext.Common
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.CompileProxy
import qualified Ext.Dev
import qualified Ext.Dev.Docs
import qualified Ext.Dev.Project
import qualified Ext.Log
import qualified Ext.Sentry
import Json.Encode ((==>))
import qualified Json.Encode as Json
import qualified Reporting.Exit as Exit
import qualified Reporting.Render.Type.Localizer
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Websocket

-- |
-- Generally called once when the server starts, this will recompile all discovered projects in State
compileAll :: Client.State -> IO ()
compileAll (Client.State mClients mProjects) = do
  Ext.Log.log Ext.Log.Live "🛫 Recompile everything"
  trackedForkIO $
    track "recompile all projects" $ do
      projects <- STM.readTVarIO mProjects
      Monad.mapM_
        (compileProject mClients)
        projects

      Ext.Log.log Ext.Log.Live "🛬 Recompile everything finished"

compileProject :: STM.TVar [Client.Client] -> Client.ProjectCache -> IO ()
compileProject mClients proj@(Client.ProjectCache (Ext.Dev.Project.Project elmJsonRoot projectRoot (NE.List topEntry remainEntry)) docsInfo cache) =
  recompileFile mClients (topEntry, remainEntry, proj)

-- | This is called frequently.
--
-- Generally when a file change has been saved, or the user has changed what their looking at in the editor.
recompile :: Client.State -> [String] -> IO ()
recompile (Client.State mClients mProjects) allChangedFiles = do
  let changedElmFiles = List.filter (\filepath -> ".elm" `List.isSuffixOf` filepath) allChangedFiles
  if (changedElmFiles /= [])
    then do
      projects <- STM.readTVarIO mProjects
      let affectedProjects = Maybe.mapMaybe (toAffectedProject changedElmFiles) projects
      case affectedProjects of
        [] ->
          Ext.Log.log Ext.Log.Live "No affected projects"
        _ ->
          pure ()

      trackedForkIO $
        track "recompile" $ do
          -- send down status for
          Monad.mapM_
            (recompileFile mClients)
            affectedProjects

          -- Get the status of the entire project
          Monad.mapM_
            (recompileProject mClients)
            affectedProjects

          -- send down warnings and docs
          Monad.mapM_
            (sendInfo mClients)
            affectedProjects
    else pure ()

toAffectedProject :: [String] -> Client.ProjectCache -> Maybe (String, [String], Client.ProjectCache)
toAffectedProject changedFiles projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project elmJsonRoot _ entrypoints) docsInfo cache) =
  case changedFiles of
    [] ->
      Nothing
    (top : remain) ->
      if List.any (\f -> Ext.Dev.Project.contains f proj) changedFiles
        then Just (top, remain, projCache)
        else Nothing

recompileProject :: STM.TVar [Client.Client] -> (String, [String], Client.ProjectCache) -> IO ()
recompileProject mClients (_, _, proj@(Client.ProjectCache (Ext.Dev.Project.Project elmJsonRoot _ (NE.List topEntry remainEntry)) docsInfo cache)) =
  recompileFile mClients (topEntry, remainEntry, proj)

recompileFile :: STM.TVar [Client.Client] -> (String, [String], Client.ProjectCache) -> IO ()
recompileFile mClients (top, remain, projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project elmJsonRoot _ entrypoints) docsInfo cache)) =
  do
    let entry = NonEmpty.List top remain
    eitherResult <-
      Ext.CompileProxy.compile
        elmJsonRoot
        entry
        (CompileHelpers.Flags CompileHelpers.Dev CompileHelpers.NoOutput)
    let eitherStatusJson = case eitherResult of
          Right _ -> Right (Json.object ["compiled" ==> Json.bool True])
          Left exit -> Left (Exit.toJson (Exit.reactorToReport exit))

    Ext.Sentry.updateCompileResult cache $
      pure eitherStatusJson

    -- Send compilation status
    case eitherStatusJson of
      Right statusJson -> do
        Client.broadcast
          mClients
          ( Client.ElmStatus
              [ Client.ProjectStatus proj True statusJson docsInfo
              ]
          )
      Left errJson -> do
        Client.broadcast
          mClients
          ( Client.ElmStatus
              [ Client.ProjectStatus proj False errJson docsInfo
              ]
          )

sendInfo :: STM.TVar [Client.Client] -> (String, [String], Client.ProjectCache) -> IO ()
sendInfo mClients (top, remain, projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project elmJsonRoot projectRoot entrypoints) docsInfo cache)) = do
  (Ext.Dev.Info warnings docs) <- Ext.Dev.info elmJsonRoot top

  case warnings of
    Nothing -> pure ()
    Just (sourceMod, warns) ->
      Client.broadcast
        mClients
        (Client.Warnings top (Reporting.Render.Type.Localizer.fromModule sourceMod) warns)

-- case docs of
--   Nothing -> pure ()

--   Just docs ->
--     Client.broadcast mClients
--       (Client.Docs top [ docs ])
