{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Questions where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad as Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Char8
import Data.Function ((&))
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Develop.Generate.Help
import qualified Ext.Common
import qualified Ext.Sentry
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Reporting.Annotation
import qualified System.FilePath as Path
import Snap.Core hiding (path)
import qualified Snap.Util.CORS
import qualified Watchtower.Annotate
import qualified Watchtower.Details
import qualified Watchtower.Find
import qualified Watchtower.Live
import qualified Watchtower.Project


-- One off questions and answers you might have/want.
data Question
  = CallgraphPlease FilePath Name.Name
  | ListMissingSignaturesPlease FilePath
  | SignaturePlease FilePath Name.Name
  | FindDefinitionPlease Watchtower.Details.PointLocation
  | FindAllInstancesPlease Watchtower.Details.PointLocation
  | Discover FilePath
  | Status

serve :: Watchtower.Live.State -> Snap ()
serve state =
  do
    route
      [ ("/:action", actionHandler state)
      ]

questionHandler :: Watchtower.Live.State -> Question -> Snap ()
questionHandler state question =
  Snap.Util.CORS.applyCORS Snap.Util.CORS.defaultOptions $
    do
      answer <- liftIO (ask state question)

      writeBuilder answer


actionHandler :: Watchtower.Live.State -> Snap ()
actionHandler state =
  do
    maybeAction <- getParam "action"
    case maybeAction of
      Just "status" ->
        questionHandler state Status

      Just "discover" ->
        do
          maybeFile <- getQueryParam "dir"
          case maybeFile of
            Nothing ->
              writeBS "Needs a directory parameter"
            Just file -> do
              questionHandler state (Discover (Data.ByteString.Char8.unpack file))
      Just "list-missing-signatures" ->
        do
          maybeFile <- getQueryParam "file"
          case maybeFile of
            Nothing ->
              writeBS "Needs location"
            Just file -> do
              let strPath = Data.ByteString.Char8.unpack file
              if Path.takeExtension strPath == ".elm" then 
                questionHandler state (ListMissingSignaturesPlease strPath)
              else 
                writeBS ("Requested missing signatures, but this isn't an elm file" <> file)
      Just "signature" ->
        do
          maybeFile <- getQueryParam "file"
          maybeName <- getQueryParam "name"
          case (maybeFile, maybeName) of
            (Just file, Just name) ->
              questionHandler
                state
                ( SignaturePlease
                    (Data.ByteString.Char8.unpack file)
                    (Name.fromChars (Data.ByteString.Char8.unpack name))
                )
            _ ->
              writeBS "Needs location"
      Just "callgraph" ->
        do
          maybeFile <- getQueryParam "file"
          maybeName <- getQueryParam "name"
          case (maybeFile, maybeName) of
            (Just file, Just name) ->
              questionHandler
                state
                ( CallgraphPlease
                    (Data.ByteString.Char8.unpack file)
                    (Name.fromChars (Data.ByteString.Char8.unpack name))
                )
            _ ->
              writeBS "Needs location"
      Just "definition" ->
        do
          maybeLocation <- getPointLocation
          case maybeLocation of
            Nothing ->
              writeBS "Needs location"
            Just location ->
              questionHandler state (FindDefinitionPlease location)

      -- Just "instances" ->
      --     do
      --         maybeLocation   <- getLocation
      --         case maybeLocation of
      --             Nothing ->
      --                 writeBS "Needs location"

      --             Just location ->
      --                 questionHandler "." (FindAllInstancesPlease location)

      _ ->
        writeBS "Wha?"

getPointLocation :: Snap (Maybe Watchtower.Details.PointLocation)
getPointLocation =
  do
    maybeFilePath <- getQueryParam "file"
    maybePosition <- getPosition

    let maybeLocation =
          case (maybeFilePath, maybePosition) of
            (Just path, Just position) ->
              Just
                ( Watchtower.Details.PointLocation
                    (Data.ByteString.Char8.unpack path)
                    position
                )
            _ ->
              Nothing

    pure maybeLocation

getPosition :: Snap (Maybe Reporting.Annotation.Position)
getPosition =
  do
    maybeRow <- getQueryParam "line"
    maybeCol <- getQueryParam "char"
    let position =
          ( \rowString colString ->
              case (Data.ByteString.Char8.readInt rowString, Data.ByteString.Char8.readInt colString) of
                (Just (rowInt, _), Just (colInt, _)) ->
                  let row = fromIntegral rowInt
                      col = fromIntegral colInt
                   in Just (Reporting.Annotation.Position row col)
                _ ->
                  Nothing
          )
            <$> maybeRow <*> maybeCol

    pure (Maybe.fromMaybe Nothing position)




ask :: Watchtower.Live.State -> Question -> IO Data.ByteString.Builder.Builder
ask state question =
  case question of
    Status ->
      allProjectStatuses state

    Discover dir ->
      do
        Ext.Common.debug $ "Discover: " ++ show dir
        Watchtower.Project.discover dir
          & fmap (\projects -> Json.Encode.encodeUgly (Json.Encode.list Watchtower.Project.encodeProjectJson projects))
    CallgraphPlease path name ->
      do
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
        Watchtower.Annotate.callgraph root path name
          & fmap Json.Encode.encodeUgly
    ListMissingSignaturesPlease path ->
      do
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
        Ext.Common.log "✍️  list signatures" (show (Path.takeFileName path))
        compiling <- Watchtower.Live.projectIsCompiling path state
        if compiling then
            Watchtower.Annotate.listMissingAnnotations root path
              & fmap Json.Encode.encodeUgly
        else
            allProjectStatuses state

    SignaturePlease path name ->
      do
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
        Watchtower.Annotate.annotation root path name
          & fmap Json.Encode.encodeUgly
    FindDefinitionPlease location ->
      let path =
            case location of
              Watchtower.Details.PointLocation f _ ->
                f
       in do
            root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
            Watchtower.Find.definitionAndPrint root location
              & fmap Json.Encode.encodeUgly
    FindAllInstancesPlease location ->
      pure (Data.ByteString.Builder.byteString ("NOTDONE"))


allProjectStatuses (Watchtower.Live.State clients mProjects) =
    do
      projects <- STM.readTVarIO mProjects
      projectStatuses <- 
          Monad.foldM 
            (\gathered (Watchtower.Live.ProjectCache proj sentry) ->
              do 
                result <- Ext.Sentry.getCompileResult sentry
                pure 
                  (Json.Encode.object 
                    [ "project" ==> Watchtower.Project.encodeProjectJson proj
                    , "success" ==> 
                        (case result of
                            Right _ ->
                              Json.Encode.bool True
                            Left _ ->
                              Json.Encode.bool False
                        )
                    , "status" ==>
                        (case result of
                            Right json ->
                              json
                            Left json ->
                              json
                        )
                    ] : gathered
                  )
            )
            []
            projects

      pure 
        (Json.Encode.encode 
          (Json.Encode.list 
              (\a -> a)
              projectStatuses
          )
        )
