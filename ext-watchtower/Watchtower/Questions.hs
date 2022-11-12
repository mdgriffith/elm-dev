{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Questions where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad as Monad
import qualified Data.ByteString as BS
import qualified Data.NonEmptyList as NE
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
import qualified Reporting.Doc
import qualified Reporting.Render.Type
import qualified Reporting.Render.Type.Localizer
import qualified Reporting.Warning as Warning

import qualified Elm.Docs as Docs

import Snap.Core hiding (path)
import qualified Snap.Util.CORS
import qualified Watchtower.Editor
import qualified Watchtower.Live
import qualified Ext.Dev.Project
import qualified Watchtower.Live.Client as Client

import qualified Ext.Dev.Find

import qualified Build
import qualified Ext.Dev.Docs

import qualified Ext.CompileProxy


-- One off questions and answers you might have/want.
data Question
  = FindDefinitionPlease Watchtower.Editor.PointLocation
  | FindAllInstancesPlease Watchtower.Editor.PointLocation
  | Docs DocsType
  | Discover FilePath
  | Status
  | Warnings FilePath
  | TimingParse FilePath
  | ServerHealth

data DocsType 
    = FromFiles [FilePath]
    -- | ForPackage  

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


unpackStringList string =
    fmap 
      (Data.ByteString.Char8.unpack) 
      (Data.ByteString.Char8.split ',' string)


actionHandler :: Watchtower.Live.State -> Snap ()
actionHandler state =
  do
    maybeAction <- getParam "action"
    case maybeAction of
      Just "status" ->
        questionHandler state Status

      Just "docs" ->
        do
          maybeFiles <- getQueryParam "files"
          case maybeFiles of
            Nothing ->
              writeBS "Needs a file parameter"
            Just filesString -> do
              let fileList = unpackStringList filesString
              questionHandler state (Docs (FromFiles fileList))

      Just "health" ->
        questionHandler state ServerHealth

      Just "warnings" ->
        do
          maybeFile <- getQueryParam "file"
          case maybeFile of
            Nothing ->
              writeBS "Needs a file parameter"
            Just file -> do
              questionHandler state (Warnings (Data.ByteString.Char8.unpack file))

      Just "parse" ->
        do
          maybeFile <- getQueryParam "file"
          case maybeFile of
            Nothing ->
              writeBS "Needs a file parameter"
            Just file -> do
              questionHandler state (TimingParse (Data.ByteString.Char8.unpack file))

      Just "discover" ->
        do
          maybeFile <- getQueryParam "dir"
          case maybeFile of
            Nothing ->
              writeBS "Needs a directory parameter"
            Just file -> do
              questionHandler state (Discover (Data.ByteString.Char8.unpack file))


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
        writeBS "Wha??"

getPointLocation :: Snap (Maybe Watchtower.Editor.PointLocation)
getPointLocation =
  do
    maybeFilePath <- getQueryParam "file"
    maybePosition <- getPosition

    let maybeLocation =
          case (maybeFilePath, maybePosition) of
            (Just path, Just position) ->
              Just
                ( Watchtower.Editor.PointLocation
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
    ServerHealth -> 
      pure (Json.Encode.encodeUgly (Json.Encode.chars "Roger dodger, ready to roll, in the pipe, five-by-five."))

    Status ->
      allProjectStatuses state

    Docs (FromFiles files) ->
      case files of
        [] ->
            pure (Json.Encode.encodeUgly (Json.Encode.chars "At least one file should be provided."))
        
        (top : remaining) ->
            do 
                eitherArtifacts <- Ext.CompileProxy.loadSingleArtifacts "/Users/matthewgriffith/projects/blissfully/development/ironzion/packages/frontend/" top
                case eitherArtifacts of
                    Left err ->
                        pure (Json.Encode.encodeUgly (Json.Encode.chars "Failed to get artifacts"))

                    Right artifacts ->
                        case  Ext.Dev.Docs.fromArtifacts artifacts of
                          Left err ->
                              -- do 
                              --     Reporting.Error.Docs.toReports
                              pure (Json.Encode.encodeUgly (Json.Encode.chars "Doc Artifacts are dumb"))
                          
                          Right docs ->
                              pure (Json.Encode.encodeUgly (Docs.encode (Docs.toDict [ docs ])))

                        
    TimingParse path ->
      do
        Ext.Common.debug $ "Parsing: " ++ show path
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
        Ext.Common.track "parsing"
          (do
              result <- Ext.CompileProxy.parse root path
              case result of
                Right _ ->
                    Ext.Common.debug $ "parsed succssfully"

                Left _ ->
                    Ext.Common.debug $ "parsing failed"
          )
        pure (Json.Encode.encodeUgly (Json.Encode.chars "The parser has been run, my liege"))

    Warnings path ->
      do
        Ext.Common.debug $ "Warnings: " ++ show path
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
        eitherErrorOrWarnings <- Ext.CompileProxy.warnings root path

        let jsonResult = case eitherErrorOrWarnings of
              Right (mod, warnings) ->
                  Json.Encode.encodeUgly
                    (Json.Encode.list
                        (Watchtower.Live.encodeWarning (Reporting.Render.Type.Localizer.fromModule mod))
                        warnings
                    )
              Left () ->
                  Json.Encode.encodeUgly (Json.Encode.chars "Parser error")

        pure jsonResult

    Discover dir ->
      do
        Ext.Common.debug $ "Discover: " ++ show dir
        Ext.Dev.Project.discover dir
          & fmap (\projects -> Json.Encode.encodeUgly (Json.Encode.list Ext.Dev.Project.encodeProjectJson projects))


    FindDefinitionPlease location ->
      let path =
            case location of
              Watchtower.Editor.PointLocation f _ ->
                f
       in do
            root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
            Ext.Dev.Find.definition root location
              & fmap Json.Encode.encodeUgly

    FindAllInstancesPlease location ->
      pure (Data.ByteString.Builder.byteString ("NOTDONE"))



allProjectStatuses (Client.State clients mProjects) =
    do
      projects <- STM.readTVarIO mProjects
      projectStatuses <-
          Monad.foldM
            (\gathered (Client.ProjectCache proj sentry) ->
              do
                result <- Ext.Sentry.getCompileResult sentry
                pure
                  (Json.Encode.object
                    [ "project" ==> Ext.Dev.Project.encodeProjectJson proj
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

