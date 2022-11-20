{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live.Compile (compileAll, recompile) where

{-|-}

import qualified Data.NonEmptyList as NonEmpty
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Lazy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Builder

import qualified Control.Concurrent.STM as STM

import qualified Reporting.Render.Type.Localizer
import Control.Monad as Monad (foldM, guard, mapM_)

import qualified Ext.Sentry
import Ext.Common
import qualified Ext.CompileProxy

import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Websocket

import qualified Ext.Dev.Project
import qualified Ext.Dev.Docs
import qualified Ext.Dev

compileMode = Ext.CompileProxy.compileToJson


{-|
Generally called once when the server starts, this will recompile all discovered projects in State

-}
compileAll :: Client.State -> IO ()
compileAll (Client.State mClients mProjects) = do

  Ext.Common.log "🛫" "Recompile everything"
  trackedForkIO $
    track "recompile all projects" $ do
      projects <- STM.readTVarIO mProjects
      Monad.mapM_
        (compileProject mClients)
        projects

      Ext.Common.log "🛬"  "Recompile everything finished"


compileProject :: STM.TVar [Client.Client] -> Client.ProjectCache -> IO ()
compileProject mClients proj@(Client.ProjectCache (Ext.Dev.Project.Project projectRoot entrypoints) cache) =
  case entrypoints of
    [] ->
      do
        Ext.Common.log "Skipping compile, no entrypoint" projectRoot
        pure ()

    topEntry : remainEntry ->
        recompileFile mClients (topEntry, remainEntry, proj)





{-| This is called frequently.

Generally when a file change has been saved, or the user has changed what their looking at in the editor.



-}
recompile :: Client.State -> [String] -> IO ()
recompile (Client.State mClients mProjects) allChangedFiles = do
  let changedElmFiles = List.filter (\filepath -> ".elm" `List.isSuffixOf` filepath ) allChangedFiles
  if (changedElmFiles /= [])
    then do
      
      projects <- STM.readTVarIO mProjects
      let affectedProjects = Maybe.mapMaybe (toAffectedProject changedElmFiles) projects
      case affectedProjects of
          [] -> 
              Ext.Common.log "No affected projects" "!"
          _ ->
              pure ()
      
      trackedForkIO $
        track "recompile" $ do
          
          -- send down status for 
          Monad.mapM_
            (recompileFile mClients)
            affectedProjects
          
          -- send down warnings and docs
          Monad.mapM_
            (sendInfo mClients)
            affectedProjects

          -- Get the status of the entire project
          -- Monad.foldM
          --   (recompileProject mClients)
          --   affectedProjects


    else
        pure ()





toAffectedProject :: [String] -> Client.ProjectCache -> Maybe (String, [String], Client.ProjectCache)
toAffectedProject changedFiles projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot entrypoints) cache) =
      case changedFiles of
        [] ->
          Nothing

        (top : remain) ->
          if List.any (\f -> Ext.Dev.Project.contains f proj) changedFiles then
            Just (top, remain, projCache)

          else
              Nothing




recompileFile ::  STM.TVar [Client.Client] -> (String, [String], Client.ProjectCache) -> IO ()
recompileFile mClients ( top, remain , projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot entrypoints) cache)) =
    do

        let entry = NonEmpty.List top remain

        -- Compile all changed files
        eitherStatusJson <-
          compileMode
            projectRoot
            entry

        Ext.Sentry.updateCompileResult cache $
          pure eitherStatusJson

        -- Send compilation status
        case eitherStatusJson of
          Right statusJson ->
            do 
              Ext.Common.log "File successfully compiles" "!"
              pure ()

          Left errJson ->
            do
              Ext.Common.log "Changed file failed" "!"
              Client.broadcast mClients
                (Client.ElmStatus [ Client.ProjectStatus proj False errJson ])

              pure ()


sendInfo ::  STM.TVar [Client.Client] -> (String, [String], Client.ProjectCache) -> IO ()
sendInfo mClients ( top, remain , projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot entrypoints) cache)) =
    do
        (Ext.Dev.Info warnings docs) <- Ext.Dev.info projectRoot top

        case warnings of
          Nothing -> pure ()
          
          Just (sourceMod, warns) -> do
            Ext.Common.log ("Sending down " <> show (length warnings) <> " warnings") "!"
            Client.broadcast mClients
              (Client.Warnings top (Reporting.Render.Type.Localizer.fromModule sourceMod) warns)
            pure ()

        case docs of
          Nothing -> pure ()

          Just docs -> do
            Ext.Common.log "Sending down docs" "!"
            Client.broadcast mClients
              (Client.Docs top [ docs ])
            pure ()

  