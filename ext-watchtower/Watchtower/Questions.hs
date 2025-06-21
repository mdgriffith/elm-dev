{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Questions where

import qualified Build
import Control.Applicative ((<$>), (<*>))
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import Control.Monad.Trans (MonadIO (liftIO))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Char8
import Data.Function ((&))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Develop.Generate.Help
import qualified Elm.Docs as Docs
import qualified Ext.Common
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Ext.CompileProxy
import qualified Ext.Dev
import qualified Ext.Dev.CallGraph
import qualified Ext.Dev.Docs
import qualified Ext.Dev.Explain
import qualified Ext.Dev.Find
import qualified Ext.Dev.InScope
import qualified Ext.Dev.Project
import qualified Ext.Log
import qualified Ext.Project.Find
import qualified Ext.Sentry
import qualified Gen.Generate
import Json.Encode ((==>))
import qualified Json.Encode
import Language.Haskell.TH (doE)
import qualified Reporting.Annotation
import qualified Reporting.Doc
import qualified Reporting.Exit as Exit
import qualified Reporting.Render.Type
import qualified Reporting.Render.Type.Localizer
import qualified Reporting.Warning as Warning
import Snap.Core hiding (path)
import qualified Snap.Util.CORS
import qualified Stuff
import qualified System.Directory as Dir (withCurrentDirectory)
import qualified System.FilePath as Path
import qualified Terminal.Dev.Error
import qualified Terminal.Dev.Out as Out
import qualified Watchtower.Editor
import qualified Watchtower.Live
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.MCP
import qualified Watchtower.State.Compile
import qualified Watchtower.State.Discover
import qualified Watchtower.State.Project

-- One off questions and answers you might have/want.
data Question
  = Discover FilePath
  | Make MakeDetails
  | FindDefinitionPlease Watchtower.Editor.PointLocation
  | FindAllInstancesPlease Watchtower.Editor.PointLocation
  | Explain Watchtower.Editor.PointLocation
  | Docs DocsType
  | CallGraph FilePath
  | InScopeFile FilePath
  | InScopeProject FilePath
  | Status
  | Warnings FilePath
  | TimingParse FilePath
  | ServerHealth

data MakeDetails = MakeDetails
  { _cwd :: FilePath,
    _makeEntrypoints :: NE.List String,
    _makeDebug :: Maybe Bool,
    _makeOptimize :: Maybe Bool
  }

data DocsType
  = FromFile FilePath
  | ForPackage PackageDetails
  | ForProject FilePath

data PackageDetails = PackageDetails
  { _owner :: String,
    _package :: String,
    _version :: String,
    _page :: PackagePage
  }

data PackagePage = ReadMe | DocsJson

pageToFile :: PackagePage -> String
pageToFile page =
  case page of
    ReadMe -> "README.md"
    DocsJson -> "docs.json"

serve :: Watchtower.Live.State -> Snap ()
serve state = do
  mcpState <- liftIO $ do
    initialState <- Watchtower.MCP.newMCPState
    return $ Watchtower.MCP.registerCommand initialState (Watchtower.MCP.uptimeCommand initialState)

  route
    [ ("/docs/:owner/:package/:version/:readme", docsHandler state),
      ("/mcp/:action", Watchtower.MCP.handleMCP mcpState),
      ("/:action", actionHandler state)
    ]

questionHandler :: Watchtower.Live.State -> Question -> Snap ()
questionHandler state question =
  Snap.Util.CORS.applyCORS Snap.Util.CORS.defaultOptions $ do
    answer <- liftIO (ask state question)
    writeBuilder answer

unpackStringList :: Data.ByteString.Char8.ByteString -> [[Char]]
unpackStringList string =
  fmap
    Data.ByteString.Char8.unpack
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
      Just "health" ->
        questionHandler state ServerHealth
      Just "make" -> do
        maybeCwd <- getQueryParam "cwd"
        maybeEntryPoints <- getQueryParam "entrypoints"
        maybeDebug <- getQueryParam "debug"
        maybeOptimize <- getQueryParam "optimize"
        case (maybeCwd, maybeEntryPoints) of
          (Just cwd, Just entrypoints) -> do
            let entrypointList = Data.ByteString.Char8.split ',' entrypoints
            case entrypointList of
              (topEntrypoint : rest) -> do
                let makeDetails =
                      MakeDetails
                        (Data.ByteString.Char8.unpack cwd)
                        ( NE.List
                            (Data.ByteString.Char8.unpack topEntrypoint)
                            (map Data.ByteString.Char8.unpack rest)
                        )
                        (fmap (== "true") maybeDebug)
                        (fmap (== "true") maybeOptimize)
                questionHandler state (Make makeDetails)
              _ ->
                writeBS "Needs at least one entrypoint"
          _ ->
            writeBS "Needs a cwd and entrypoints parameter"
      Just "docs" ->
        do
          maybeFiles <- getQueryParam "file"
          maybeEntryPoint <- getQueryParam "entrypoint"
          case maybeEntryPoint of
            Just entrypoint ->
              questionHandler state (Docs (ForProject (Data.ByteString.Char8.unpack entrypoint)))
            Nothing ->
              case maybeFiles of
                Nothing ->
                  writeBS "Needs a file or an entrypoint parameter"
                Just fileString -> do
                  questionHandler state (Docs (FromFile (Data.ByteString.Char8.unpack fileString)))
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
      Just "callgraph" ->
        do
          maybeFile <- getQueryParam "file"
          case maybeFile of
            Nothing ->
              writeBS "Needs a file parameter"
            Just file -> do
              questionHandler state (CallGraph (Data.ByteString.Char8.unpack file))
      Just "scope" ->
        do
          maybeFile <- getQueryParam "file"
          case maybeFile of
            Nothing ->
              writeBS "Needs a file parameter"
            Just file -> do
              questionHandler state (InScopeFile (Data.ByteString.Char8.unpack file))
      Just "project" ->
        do
          maybeFile <- getQueryParam "file"
          case maybeFile of
            Nothing ->
              writeBS "Needs a file parameter"
            Just file -> do
              questionHandler state (InScopeProject (Data.ByteString.Char8.unpack file))
      Just "definition" ->
        do
          maybeLocation <- getPointLocation
          case maybeLocation of
            Nothing ->
              writeBS "Needs location"
            Just location ->
              questionHandler state (FindDefinitionPlease location)
      Just "explain" ->
        do
          maybeLocation <- getPointLocation
          case maybeLocation of
            Nothing ->
              writeBS "Needs location"
            Just location ->
              questionHandler state (Explain location)

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
            <$> maybeRow
            <*> maybeCol

    pure (Maybe.fromMaybe Nothing position)

ask :: Watchtower.Live.State -> Question -> IO Data.ByteString.Builder.Builder
ask state question =
  case question of
    ServerHealth ->
      pure (Json.Encode.encodeUgly (Json.Encode.chars "Roger dodger, ready to roll, in the pipe, five-by-five."))
    Status ->
      allProjectStatuses state
    Discover dir -> do
      Watchtower.State.Discover.discover state dir Map.empty
      allProjectStatuses state
    Make (MakeDetails cwd entrypoints debug optimize) -> do
      let flags =
            CompileHelpers.Flags
              ( case optimize of
                  Just True -> CompileHelpers.Prod
                  _ ->
                    case debug of
                      Just True -> CompileHelpers.Debug
                      _ -> CompileHelpers.Dev
              )
              (CompileHelpers.OutputTo CompileHelpers.Js)
      projectCache <- Watchtower.State.Project.upsert state flags cwd entrypoints
      compilationResult <- Watchtower.State.Compile.compile flags projectCache
      case compilationResult of
        Left (Watchtower.State.Compile.ReactorError reactorExit) ->
          pure (Out.asJsonUgly (Left (Terminal.Dev.Error.ExitReactor reactorExit)))
        Left (Watchtower.State.Compile.GenerationError err) ->
          pure (Json.Encode.encodeUgly (Json.Encode.chars err))
        Right (CompileHelpers.CompiledJs js) -> do
          putStrLn "Success, returning JS"
          pure (Data.ByteString.Builder.byteString "// success\n" <> js)
        Right (CompileHelpers.CompiledHtml html) -> do
          putStrLn "Success, returning HTML"
          pure html
        Right CompileHelpers.CompiledSkippedOutput -> do
          putStrLn "Success, returning skipped output"
          pure (Json.Encode.encodeUgly (Json.Encode.chars "// success"))
    Docs (ForProject entrypoint) -> do
      maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing ->
          pure (Out.asJsonUgly (Left Terminal.Dev.Error.CouldNotFindRoot))
        Just root -> do
          let moduleNames = NE.List (Name.fromChars entrypoint) []
          compilationResult <- Ext.CompileProxy.loadAndEnsureCompiled root (Just moduleNames)
          case compilationResult of
            Left err ->
              pure (Out.asJsonUgly (Left (Terminal.Dev.Error.CompilationError err)))
            Right details -> do
              maybeDocs <- Ext.CompileProxy.compileToDocs root moduleNames details
              case maybeDocs of
                Left err ->
                  pure (Out.asJsonUgly (Left (Terminal.Dev.Error.ExitReactor err)))
                Right docs ->
                  pure (Out.asJsonUgly (Right (Docs.encode docs)))
    Docs (FromFile path) ->
      do
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
        maybeDocs <- Ext.Dev.docs root path
        case maybeDocs of
          Nothing ->
            pure (Json.Encode.encodeUgly (Json.Encode.chars "Docs are not available"))
          Just docs ->
            pure (Json.Encode.encodeUgly (Docs.encode (Docs.toDict [docs])))
    Docs (ForPackage (PackageDetails owner package version page)) ->
      do
        elmHome <- Stuff.getElmHome
        let filepath = elmHome Path.</> "0.19.1" Path.</> "packages" Path.</> owner Path.</> package Path.</> version Path.</> pageToFile page
        Data.ByteString.Builder.byteString <$> BS.readFile filepath
    TimingParse path ->
      do
        Ext.Log.log Ext.Log.Questions $ "Parsing: " ++ show path
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
        Ext.Common.track
          "parsing"
          ( do
              result <- Ext.CompileProxy.parse root path
              case result of
                Right _ ->
                  Ext.Log.log Ext.Log.Questions "parsed succssfully"
                Left _ ->
                  Ext.Log.log Ext.Log.Questions "parsing failed"
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
                  ( Json.Encode.list
                      (Watchtower.Live.encodeWarning (Reporting.Render.Type.Localizer.fromModule mod))
                      warnings
                  )
              Left () ->
                Json.Encode.encodeUgly (Json.Encode.chars "Parser error")

        pure jsonResult
    InScopeProject file ->
      do
        Ext.Log.log Ext.Log.Questions $ "Scope: " ++ show file
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot file state)
        maybeScope <- Ext.Dev.InScope.project root file
        case maybeScope of
          Nothing ->
            pure (Json.Encode.encodeUgly (Json.Encode.chars "No scope"))
          Just scope ->
            pure (Json.Encode.encodeUgly (Ext.Dev.InScope.encodeProjectScope scope))
    InScopeFile file ->
      do
        Ext.Log.log Ext.Log.Questions $ "Scope: " ++ show file
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot file state)
        maybeScope <- Ext.Dev.InScope.file root file
        case maybeScope of
          Nothing ->
            pure (Json.Encode.encodeUgly (Json.Encode.chars "No scope"))
          Just scope ->
            pure (Json.Encode.encodeUgly (Ext.Dev.InScope.encodeFileScope scope))
    CallGraph file ->
      do
        Ext.Log.log Ext.Log.Questions $ "Callgraph: " ++ show file
        root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot file state)
        maybeCallgraph <- Ext.Dev.CallGraph.callgraph root file
        case maybeCallgraph of
          Nothing ->
            pure (Json.Encode.encodeUgly (Json.Encode.chars "No callgraph"))
          Just callgraph ->
            pure (Json.Encode.encodeUgly (Ext.Dev.CallGraph.encode callgraph))
    Explain location ->
      let path =
            case location of
              Watchtower.Editor.PointLocation f _ ->
                f
       in do
            root <- fmap (Maybe.fromMaybe ".") (Watchtower.Live.getRoot path state)
            maybeExplanation <- Ext.Dev.Explain.explainAtLocation root location
            case maybeExplanation of
              Nothing ->
                Json.Encode.null
                  & Json.Encode.encodeUgly
                  & pure
              Just explanation ->
                Ext.Dev.Explain.encode explanation
                  & Json.Encode.encodeUgly
                  & pure
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
      pure (Data.ByteString.Builder.byteString "NOTDONE")

isSuccess :: Either Json.Encode.Value Json.Encode.Value -> Bool
isSuccess (Left _) = True
isSuccess (Right _) = False

flattenJsonStatus :: Either Json.Encode.Value Json.Encode.Value -> Json.Encode.Value
flattenJsonStatus (Left json) = json
flattenJsonStatus (Right json) = json

allProjectStatuses :: Client.State -> IO Data.ByteString.Builder.Builder
allProjectStatuses (Client.State clients mProjects) =
  do
    projects <- STM.readTVarIO mProjects
    projectStatuses <-
      Monad.foldM
        ( \gathered (Client.ProjectCache proj docsInfo sentry) -> do
            jsonStatusResult <- Ext.Sentry.getCompileResult sentry
            let projectStatus =
                  Client.ProjectStatus
                    proj
                    (isSuccess jsonStatusResult)
                    (flattenJsonStatus jsonStatusResult)
            pure $ projectStatus : gathered
        )
        []
        projects

    pure
      ( Client.encodeOutgoing
          (Client.ElmStatus projectStatuses)
      )
