{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Questions where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad as Monad
import qualified Control.Exception as Exception
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
import qualified Stuff

import qualified Build
import qualified Ext.Dev
import qualified Ext.Dev.Docs
import qualified Ext.Dev.Find

import qualified Ext.CompileProxy
import qualified Ext.Log
import qualified Watchtower.Editor


-- One off questions and answers you might have/want.
data Question
  = FindDefinitionPlease Watchtower.Editor.PointLocation
  | FindAllInstancesPlease
      { _location :: Watchtower.Editor.PointLocation,
        _includeDeclaration :: Bool
      }
  | Docs DocsType
  | Discover FilePath
  | Status
  | Warnings FilePath
  | TimingParse FilePath
  | ServerHealth

data DocsType 
    = FromFile FilePath
    | ForPackage PackageDetails

data PackageDetails =
  PackageDetails 
    { _owner :: String
    , _package :: String
    , _version :: String
    , _page :: PackagePage
    }


data PackagePage = ReadMe | DocsJson

pageToFile :: PackagePage -> String
pageToFile page =
  case page of
    ReadMe -> "README.md"
    DocsJson -> "docs.json"


serve :: Watchtower.Live.State -> Snap ()
serve state =
  do
    route
      [ ("/docs/:owner/:package/:version/:readme", docsHandler state)
      , ("/:action", actionHandler state)
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



docsHandler :: Watchtower.Live.State -> Snap ()
docsHandler state =
  do
    maybeOwner <- getParam "owner"
    maybePackage <- getParam "package"
    maybeVersion <- getParam "version"
    maybeReadme <- getParam "readme"
    case (maybeOwner, maybePackage, maybeVersion, maybeReadme) of
      (Just owner, Just package, Just version, Just "docs.json") -> do
          let details = PackageDetails (Data.ByteString.Char8.unpack owner) (Data.ByteString.Char8.unpack package) (Data.ByteString.Char8.unpack version) DocsJson
          questionHandler state (Docs (ForPackage details))
      
      (Just owner, Just package, Just version, Just "README.md") -> do
          let details = PackageDetails (Data.ByteString.Char8.unpack owner) (Data.ByteString.Char8.unpack package) (Data.ByteString.Char8.unpack version) ReadMe
          questionHandler state (Docs (ForPackage details))

      _ ->
          writeBS "Wha??"

     

actionHandler :: Watchtower.Live.State -> Snap ()
actionHandler state =
  do
    maybeAction <- getParam "action"
    case maybeAction of
      Just "status" ->
        questionHandler state Status

      Just "docs" ->
        do
          maybeFiles <- getQueryParam "file"
          case maybeFiles of
            Nothing ->
              writeBS "Needs a file parameter"

            Just fileString -> do
              questionHandler state (Docs (FromFile (Data.ByteString.Char8.unpack fileString)))

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

      Just "instances" -> do
        maybeLocation <- getPointLocation
        maybeIncludeDecl <- getQueryParam "decl"
        let includeDeclaration = maybeIncludeDecl == Just "true"
        case maybeLocation of
            Nothing ->
                writeBS "Needs location"

            Just location ->
                questionHandler state (FindAllInstancesPlease location includeDeclaration)

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

    Docs (FromFile path) ->
      do
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
        maybeDocs <- Ext.Dev.docs root path
        case maybeDocs of
          Nothing ->
            pure (Json.Encode.encodeUgly (Json.Encode.chars "Docs are not available"))

          Just docs ->
            pure (Json.Encode.encodeUgly (Docs.encode (Docs.toDict [ docs ])))
               

    Docs (ForPackage (PackageDetails owner package version page)) ->
      do
        elmHome <- Stuff.getElmHome
        let filepath = elmHome Path.</> "0.19.1" Path.</> "packages" Path.</> owner Path.</> package Path.</> version Path.</> pageToFile page
        Data.ByteString.Builder.byteString <$> BS.readFile filepath
        
                        
    TimingParse path ->
      do
        Ext.Log.log Ext.Log.Questions $ "Parsing: " ++ show path
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
        Ext.Common.track "parsing"
          (do
              result <- Ext.CompileProxy.parse root path
              case result of
                Right _ ->
                    Ext.Log.log Ext.Log.Questions $ "parsed succssfully"

                Left _ ->
                    Ext.Log.log Ext.Log.Questions $ "parsing failed"
          )
        pure (Json.Encode.encodeUgly (Json.Encode.chars "The parser has been run, my liege"))

    Warnings path ->
      do
        Ext.Log.log Ext.Log.Questions $ "Warnings: " ++ show path
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
        eitherErrorOrWarnings <- Ext.Dev.warnings root path

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
        Ext.Log.log Ext.Log.Questions $ "Discover: " ++ show dir
        Ext.Dev.Project.discover dir
          & fmap (\projects -> Json.Encode.encodeUgly (Json.Encode.list Ext.Dev.Project.encodeProjectJson projects))


    FindDefinitionPlease location@(Watchtower.Editor.PointLocation path _) -> do
      root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
      maybeDefinition <- Ext.Dev.Find.definition root location

      maybeDefinition 
          & maybe Json.Encode.null (encodeWrappingObject "definition" . Watchtower.Editor.encodePointRegion) 
          & Json.Encode.encodeUgly
          & pure 

    FindAllInstancesPlease location@(Watchtower.Editor.PointLocation path _) inclDecl -> do
      root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
      instances <- Ext.Dev.Find.instances root location inclDecl

      instances
        & fmap Watchtower.Editor.encodePointRegion
        & Json.Encode.array
        & encodeWrappingObject "instances"
        & Json.Encode.encodeUgly
        & pure


encodeWrappingObject :: String -> Json.Encode.Value -> Json.Encode.Value
encodeWrappingObject name value =
  Json.Encode.object [ name ==> value ]


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

