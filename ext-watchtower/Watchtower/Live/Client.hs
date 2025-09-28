{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live.Client
  ( Client (..),
    ClientId,
    ProjectRoot,
    State (..),
    PackageInfo (..),
    PackageModule (..),
    Urls (..),
    FileInfo (..),
    ProjectCache (..),
    ProjectStatus (..),
    FileWatchType,
    GetExistingProjectError (..),
    Error (..),
    CompilationResult (..),
    encodeCompilationResult,
    logFileInfoKeys,
    getAllStatuses,
    getRoot,
    getProjectRoot,
    getClientData,
    getFileInfo,
    getAllFileInfos,
    getExistingProject,
    Outgoing (..),
    encodeOutgoing,
    outgoingToLog,
    Incoming (..),
    decodeIncoming,
    encodeWarning,
    getFileInfoFromModuleName,
    broadcast,
    broadcastTo,
    broadcastToMany,
    matchingProject,
    isWatchingFileForWarnings,
    isWatchingFileForDocs,
    emptyWatch,
    watchProjects,
    watchTheseFilesOnly,
    builderToString,
    watchedFiles,
    toOldJSON
  )
where

-- \| This could probably be renamed Live.State or something.  Client is a little weird but :shrug:
--
--

import qualified Control.Concurrent.STM as STM
import Control.Monad as Monad (foldM, guard)
import Data.Aeson (ToJSON (toJSON))
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Elm.Docs as Docs
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Ext.Common
import qualified Ext.Dev.Project
import qualified Ext.Log
import qualified Ext.Sentry
import qualified Gen.Config
import qualified Json.Decode
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String
import qualified Reporting.Annotation as Ann
import qualified Reporting.Doc
import qualified Reporting.Render.Type
import qualified Reporting.Render.Type.Localizer
import qualified Reporting.Warning as Warning
import qualified Reporting.Exit
import qualified System.FilePath as FilePath
import qualified Watchtower.Editor
import qualified Watchtower.Websocket
import qualified Ext.CompileHelpers.Generic



data Urls = Urls
  { urlsLsp :: Maybe String,
    urlsMcp :: Maybe String,
    urlsDevHttp :: String,
    urlsDevWebsocket :: String
  }


data State = State
  { clients :: STM.TVar [Client],
    projects ::
      STM.TVar
        [ProjectCache],
    fileInfo :: STM.TVar (Map.Map FilePath FileInfo),
    packages :: STM.TVar (Map.Map Pkg.Name PackageInfo),
    urls :: Urls
  }

data ProjectCache = ProjectCache
  { project :: Ext.Dev.Project.Project,
    docsInfo :: Gen.Config.DocsConfig,
    flags :: Ext.CompileHelpers.Generic.Flags,
    compileResult :: STM.TVar CompilationResult,
    testResults :: STM.TVar (Maybe TestResults)
  }

data CompilationResult =
    NotCompiled
  | Success Ext.CompileHelpers.Generic.CompilationResult
  | Error Error

data Error
  = ReactorError Reporting.Exit.Reactor
  | GenerationError String

toOldJSON :: CompilationResult -> Json.Encode.Value
toOldJSON NotCompiled =
  Json.Encode.object ["compiled" ==> Json.Encode.bool False]
toOldJSON (Success result) =
  Json.Encode.object ["compiled" ==> Json.Encode.bool True]
toOldJSON (Error (ReactorError exit)) =
  Reporting.Exit.toJson (Reporting.Exit.reactorToReport exit)
toOldJSON (Error (GenerationError err)) =
  Json.Encode.chars err


-- New JSON encoder (Aeson) that includes generated JS/HTML when available
encodeCompilationResult :: CompilationResult -> Aeson.Value
encodeCompilationResult NotCompiled =
  Aeson.object [ "compiled" .= False ]
encodeCompilationResult (Success result) =
  case result of
    Ext.CompileHelpers.Generic.CompiledJs builder ->
      let codeText = builderToString builder in
      Aeson.object
        [ "compiled" .= True
        , "code" .= codeText
        ]
    Ext.CompileHelpers.Generic.CompiledHtml builder ->
      let codeText = builderToString builder in
      Aeson.object
        [ "compiled" .= True
        , "code" .= codeText
        ]
    Ext.CompileHelpers.Generic.CompiledSkippedOutput ->
      Aeson.object
        [ "compiled" .= True
        , "code" .= Aeson.Null
        ]
encodeCompilationResult (Error (ReactorError exit)) =
  let v = Reporting.Exit.toJson (Reporting.Exit.reactorToReport exit)
      lbs = Data.ByteString.Builder.toLazyByteString (Json.Encode.encodeUgly v)
  in case Aeson.eitherDecode lbs of
       Left _ -> Aeson.object [ "compiled" .= False, "code" .= T.pack "", "error" .= T.pack "reactor" ]
       Right val -> val
encodeCompilationResult (Error (GenerationError err)) =
  Aeson.String (T.pack err)


-- Client

type ClientId = T.Text

type Client = Watchtower.Websocket.Client Watching

data FileInfo = FileInfo
  { warnings :: [Warning.Warning]
  , docs :: Maybe Docs.Module
  , localizer :: Maybe Reporting.Render.Type.Localizer.Localizer
  , sourceAst :: Maybe Src.Module
  , canonicalAst :: Maybe Can.Module
  , typeAt :: Maybe (Map.Map Ann.Region Can.Annotation) -- ELM DEV: types at regions
  }

-- Packages tracked in memory for docs and metadata
data PackageInfo = PackageInfo
  { name :: Pkg.Name
  , readme :: Maybe String
  , packageModules :: Map.Map ModuleName.Raw PackageModule
  }

data PackageModule = PackageModule
  { packageModuleDocs :: Docs.Module
  }

-- Render a compact availability summary for a given FileInfo
-- Example output: "[wdlsct]" where each letter is present or replaced by a space
formatFileInfoSummary :: FileInfo -> String
formatFileInfoSummary fi =
  let w = if List.null (warnings fi) then ' ' else 'w'
      d = if Maybe.isJust (docs fi) then 'd' else ' '
      l = if Maybe.isJust (localizer fi) then 'l' else ' '
      s = if Maybe.isJust (sourceAst fi) then 's' else ' '
      c = if Maybe.isJust (canonicalAst fi) then 'c' else ' '
      t = if Maybe.isJust (typeAt fi) then 't' else ' '
   in "[" ++ [w, d, l, s, c, t] ++ "]"

getFileInfo :: FilePath -> State -> IO (Maybe FileInfo)
getFileInfo path (State _ _ mFileInfo _ _) = do
  fileInfo <- STM.readTVarIO mFileInfo
  pure (Map.lookup path fileInfo)

-- Read the entire FileInfo map
getAllFileInfos :: State -> IO (Map.Map FilePath FileInfo)
getAllFileInfos (State _ _ mFileInfo _ _) = STM.readTVarIO mFileInfo

-- Given a project and a canonical module name, try to find the first matching FileInfo
-- by generating potential file paths using the project's srcDirs and the module's Raw name.
getFileInfoFromModuleName :: ProjectCache -> ModuleName.Canonical -> State -> IO (Maybe FileInfo)
getFileInfoFromModuleName (ProjectCache proj _ _ _ _) (ModuleName.Canonical _pkg rawName) state@(State _ _ mFileInfo _ _) = do
  fileInfoMap <- STM.readTVarIO mFileInfo
  let moduleRelPath = ModuleName.toFilePath rawName ++ ".elm"
  let srcDirs = Ext.Dev.Project._srcDirs proj
  let candidates = fmap (\dir -> FilePath.normalise (dir FilePath.</> moduleRelPath)) srcDirs
  pure (List.foldl (\acc p -> case acc of
                                Nothing -> Map.lookup p fileInfoMap
                                justVal -> justVal
                      ) Nothing candidates)

logFileInfoKeys :: State -> IO ()
logFileInfoKeys (State _ _ mFileInfo mPackages _) = do
  infoMap <- STM.readTVarIO mFileInfo
  let keys = Map.keys infoMap
  mapM_
    (\(path, fi) ->
      Ext.Log.log Ext.Log.Live (path ++ ": " ++ formatFileInfoSummary fi)
    )
    (Map.toList infoMap)
  -- Also log package info summary: package name and number of modules
  packagesMap <- STM.readTVarIO mPackages
  mapM_
    (\(pkgName, pkgInfo) ->
      Ext.Log.log Ext.Log.Live (
        "pkg " ++ Pkg.toChars pkgName ++ ": " ++ show (Map.size (packageModules pkgInfo)) ++ " modules"
      )
    )
    (Map.toList packagesMap)

emptyWatch :: Watching
emptyWatch =
  Watching Set.empty Map.empty

data Watching = Watching
  { watchingProjects :: Set.Set ProjectRoot,
    watchingFiles :: Map.Map FilePath FileWatchType
  }

data FileWatchType = FileWatchType
  { watchForWarnings :: Bool, -- missing type signatures/unused stuff
    watchForDocs :: Bool
  }

type ProjectRoot = FilePath



{- Websocket messages
-}
data Incoming
  = Discover FilePath (Map.Map FilePath FileWatchType)
  | Changed FilePath
  | Watched (Map.Map FilePath FileWatchType)
  | EditorViewingUpdated [ViewingInEditorDetails]
  | EditorJumpToRequested FilePath Ann.Region

data Outgoing
  = ElmStatus [ProjectStatus]
  | Warnings FilePath Reporting.Render.Type.Localizer.Localizer [Warning.Warning]
  | Docs FilePath [Docs.Module]
  | EditorViewing [ViewingInEditorDetails]
  | EditorJumpTo FilePath Ann.Region
  | Tests FilePath TestResults

data ViewingInEditorDetails = ViewingInEditorDetails
  { _viewingFilepath :: FilePath,
    _viewingRegions :: [Ann.Region],
    _viewingActiveEditor :: Bool,
    _unsavedChanges :: Bool
  }
  deriving (Show)

watchedFiles :: Watching -> Map.Map FilePath FileWatchType
watchedFiles (Watching _ files) =
  files

getClientData :: ClientId -> State -> IO (Maybe Watching)
getClientData clientId (State mClients _ _ _ _) = do
  clients <- STM.atomically $ STM.readTVar mClients

  pure
    ( List.foldl
        ( \found client ->
            case found of
              Nothing ->
                if Watchtower.Websocket.matchId clientId client
                  then Just (Watchtower.Websocket.clientData client)
                  else Nothing
              _ ->
                found
        )
        Nothing
        clients
    )

watchProjects :: [ProjectRoot] -> Watching -> Watching
watchProjects newRoots (Watching watchingProjects watchingFiles) =
  Watching (Set.union watchingProjects (Set.fromList newRoots)) watchingFiles

watchTheseFilesOnly :: Map.Map FilePath FileWatchType -> Watching -> Watching
watchTheseFilesOnly newFileWatching (Watching watchingProjects watchingFiles) =
  Watching watchingProjects newFileWatching

isWatchingProject :: Ext.Dev.Project.Project -> Watching -> Bool
isWatchingProject proj (Watching watchingProjects watchingFiles) =
  Set.member (Ext.Dev.Project._root proj) watchingProjects

isWatchingFileForWarnings :: FilePath -> Watching -> Bool
isWatchingFileForWarnings file (Watching watchingProjects watchingFiles) =
  case Map.lookup file watchingFiles of
    Nothing -> False
    Just (FileWatchType _ watchForWarnings) ->
      watchForWarnings

isWatchingFileForDocs :: FilePath -> Watching -> Bool
isWatchingFileForDocs file (Watching watchingProjects watchingFiles) =
  case Map.lookup file watchingFiles of
    Nothing -> False
    Just (FileWatchType watchForDocs _) ->
      watchForDocs

getRoot :: FilePath -> State -> IO (Maybe FilePath)
getRoot path (State _ mProjects _ _ _) =
  do
    projects <- STM.readTVarIO mProjects
    pure (getRootHelp path projects Nothing)

getRootHelp :: FilePath -> [ProjectCache] -> Maybe FilePath -> Maybe [Char]
getRootHelp path projects found =
  case projects of
    [] -> found
    (ProjectCache project _ _ _ _) : remain ->
      if Ext.Dev.Project.contains path project
        then case found of
          Nothing ->
            getRootHelp path remain (Just (Ext.Dev.Project._root project))
          Just root ->
            if List.length (Ext.Dev.Project._root project) > List.length root
              then getRootHelp path remain (Just (Ext.Dev.Project._root project))
              else getRootHelp path remain found
        else getRootHelp path remain found

getProjectRoot :: ProjectCache -> FilePath
getProjectRoot (ProjectCache proj _ _ _ _) =
  Ext.Dev.Project.getRoot proj



data GetExistingProjectError
  = NoProjectsRegistered
  | ProjectNotFound FilePath


-- Sorts projects by root path length, with shortest root first
sortProjects :: [ProjectCache] -> [ProjectCache]
sortProjects projects =
  List.sortBy (\(ProjectCache p1 _ _ _ _) (ProjectCache p2 _ _ _ _) -> compare (List.length (Ext.Dev.Project._root p2)) (List.length (Ext.Dev.Project._root p1))) projects

getExistingProject :: FilePath -> State -> IO (Either GetExistingProjectError (ProjectCache, [ProjectCache]))
getExistingProject path (State _ mProjects _ _ _) = do
  projects <- STM.readTVarIO mProjects
  case projects of
    [] -> pure $ Left NoProjectsRegistered
    _ -> do
      let containingProjects = List.filter (\(ProjectCache project _ _ _ _) -> Ext.Dev.Project.contains path project) projects
      pure $ case sortProjects containingProjects of
        [] -> Left (ProjectNotFound path)
        (first : rest) ->
          Right (first, rest)

matchingProject :: ProjectCache -> ProjectCache -> Bool
matchingProject (ProjectCache one _ _ _ _) (ProjectCache two _ _ _ _) =
  Ext.Dev.Project.equal one two

getAllStatuses :: State -> IO [ProjectStatus]
getAllStatuses state@(State _ mProjects _ _ _) =
  do
    projects <- STM.readTVarIO mProjects

    Monad.foldM
      ( \statuses proj ->
          do
            status <- getStatus proj
            pure (status : statuses)
      )
      []
      projects

getStatus :: ProjectCache -> IO ProjectStatus
getStatus (ProjectCache proj docsInfo _ mCompileResult _) =
  do
    result <- STM.readTVarIO mCompileResult
    let successful =
          case result of
            Success _ -> True
            _ -> False
    let json = toOldJSON result
    pure (ProjectStatus proj successful json docsInfo)

outgoingToLog :: Outgoing -> String
outgoingToLog outgoing =
  case outgoing of
    ElmStatus projectStatusList ->
      "Status: " ++ Ext.Common.formatList (fmap projectStatusToString projectStatusList)
    Warnings _ _ warnings ->
      show (length warnings) <> " warnings"
    Docs _ _ ->
      "Docs"
    EditorViewing _ ->
      "EditorViewing"
    EditorJumpTo _ _ ->
      "EditorJumpTo"
    Tests _ _ ->
      "Tests"

projectStatusToString :: ProjectStatus -> String
projectStatusToString (ProjectStatus proj success json docs) =
  if success
    then "Success: ../" ++ FilePath.takeBaseName (Ext.Dev.Project.getRoot proj)
    else "Failing: ../" ++ FilePath.takeBaseName (Ext.Dev.Project.getRoot proj)

data ProjectStatus = ProjectStatus
  { _project :: Ext.Dev.Project.Project,
    _success :: Bool,
    _json :: Json.Encode.Value,
    _docs :: Gen.Config.DocsConfig
  }

encodeOutgoing :: Outgoing -> Data.ByteString.Builder.Builder
encodeOutgoing out =
  Json.Encode.encodeUgly $
    case out of
      ElmStatus statuses ->
        Json.Encode.object
          [ "msg" ==> Json.Encode.string (Json.String.fromChars "Status"),
            "details"
              ==> Json.Encode.list
                ( \(ProjectStatus project success status docs) ->
                    Json.Encode.object
                      [ "root"
                          ==> Json.Encode.string
                            ( Json.String.fromChars
                                (Ext.Dev.Project._root project)
                            ),
                        "projectRoot"
                          ==> Json.Encode.string
                            ( Json.String.fromChars
                                (Ext.Dev.Project._projectRoot project)
                            ),
                        "entrypoints"
                          ==> Json.Encode.list
                            (Json.Encode.string . Json.String.fromChars)
                            (NE.toList (Ext.Dev.Project._entrypoints project)),
                        "status" ==> status,
                        "docs" ==> encodeDocsConfig docs
                      ]
                )
                statuses
          ]
      Warnings path localizer warnings ->
        Json.Encode.object
          [ "msg" ==> Json.Encode.string (Json.String.fromChars "Warnings"),
            "details"
              ==> Json.Encode.object
                [ "filepath" ==> Json.Encode.string (Json.String.fromChars path),
                  "warnings"
                    ==> Json.Encode.list
                      (encodeWarning localizer)
                      warnings
                ]
          ]
      Docs path docs ->
        Json.Encode.object
          [ "msg" ==> Json.Encode.string (Json.String.fromChars "Docs"),
            "details"
              ==> Json.Encode.object
                [ "filepath" ==> Json.Encode.string (Json.String.fromChars path),
                  "docs"
                    ==> Docs.encode (Docs.toDict docs)
                ]
          ]
      EditorViewing details ->
        Json.Encode.object
          [ "msg" ==> Json.Encode.string (Json.String.fromChars "EditorVisibilityChanged"),
            "details"
              ==> Json.Encode.object
                [ "visible"
                    ==> Json.Encode.list
                      ( \detail ->
                          Json.Encode.object
                            [ "filepath"
                                ==> Json.Encode.string
                                  (Json.String.fromChars (_viewingFilepath detail)),
                              "regions"
                                ==> Json.Encode.list Watchtower.Editor.encodeRegion (_viewingRegions detail),
                              "active" ==> Json.Encode.bool (_viewingActiveEditor detail),
                              "unsavedChanges" ==> Json.Encode.bool (_unsavedChanges detail)
                            ]
                      )
                      details
                ]
          ]
      EditorJumpTo path region ->
        Json.Encode.object
          [ "msg" ==> Json.Encode.string (Json.String.fromChars "EditorJumpTo"),
            "details"
              ==> Json.Encode.object
                [ "path" ==> Json.Encode.string (Json.String.fromChars path),
                  "region" ==> Watchtower.Editor.encodeRegion region
                ]
          ]
      Tests path results ->
        Json.Encode.object
          [ "msg" ==> Json.Encode.string (Json.String.fromChars "Tests"),
            "details" ==> encodeTestResults path results
          ]

{- Decoding -}

decodeIncoming :: Json.Decode.Decoder T.Text Incoming
decodeIncoming =
  Json.Decode.field "msg" Json.Decode.string
    >>= ( \msg ->
            case msg of
              "Changed" ->
                Changed
                  <$> ( Json.Decode.field
                          "details"
                          (Json.Decode.field "path" (Json.String.toChars <$> Json.Decode.string))
                      )
              "Discover" ->
                Json.Decode.field
                  "details"
                  ( Discover
                      <$> Json.Decode.field "root" (Json.String.toChars <$> Json.Decode.string)
                      <*> Json.Decode.field "watching" decodeWatched
                  )
              "Watched" ->
                Watched
                  <$> Json.Decode.field
                    "details"
                    decodeWatched
              "EditorVisibilityChanged" ->
                EditorViewingUpdated
                  <$> Json.Decode.field
                    "details"
                    ( Json.Decode.field
                        "visible"
                        ( Json.Decode.list
                            ( ViewingInEditorDetails
                                <$> Json.Decode.field "filepath" (Json.String.toChars <$> Json.Decode.string)
                                <*> Json.Decode.field "regions" (Json.Decode.list Watchtower.Editor.decodeRegion)
                                <*> Json.Decode.field "active" Json.Decode.bool
                                <*> Json.Decode.field "unsavedChanges" Json.Decode.bool
                            )
                        )
                    )
              "EditorJumpToRequested" ->
                Json.Decode.field
                  "details"
                  ( EditorJumpToRequested
                      <$> Json.Decode.field "path" (Json.String.toChars <$> Json.Decode.string)
                      <*> Json.Decode.field "region" Watchtower.Editor.decodeRegion
                  )
              _ ->
                Json.Decode.failure "Unknown msg"
        )

decodeWatched :: Json.Decode.Decoder T.Text (Map.Map FilePath FileWatchType)
decodeWatched =
  fmap
    Map.fromList
    ( Json.Decode.list
        ( ( \path warns docs ->
              ( path,
                FileWatchType warns docs
              )
          )
            <$> Json.Decode.field "path" (Json.String.toChars <$> Json.Decode.string)
            <*> Json.Decode.field "warnings" Json.Decode.bool
            <*> Json.Decode.field "docs" Json.Decode.bool
        )
    )

{- Encoding -}

encodeStatus (Ext.Dev.Project.Project root projectRoot entrypoints _srcDirs, js) =
  Json.Encode.object
    [ "root" ==> Json.Encode.string (Json.String.fromChars root),
      "projectRoot" ==> Json.Encode.string (Json.String.fromChars projectRoot),
      "entrypoints" ==> Json.Encode.list (Json.Encode.string . Json.String.fromChars) (NE.toList entrypoints),
      "status" ==> js
    ]

encodeWarning :: Reporting.Render.Type.Localizer.Localizer -> Warning.Warning -> Json.Encode.Value
encodeWarning localizer warning =
  case warning of
    Warning.UnusedImport region name ->
      Json.Encode.object
        [ "warning" ==> Json.Encode.chars "UnusedImport",
          "region"
            ==> Watchtower.Editor.encodeRegion region,
          "name"
            ==> Json.Encode.chars (Name.toChars name)
        ]
    Warning.UnusedVariable region defOrPattern name ->
      Json.Encode.object
        [ "warning" ==> Json.Encode.chars "UnusedVariable",
          "region"
            ==> Watchtower.Editor.encodeRegion region,
          "context"
            ==> ( case defOrPattern of
                    Warning.Def -> Json.Encode.chars "def"
                    Warning.Pattern -> Json.Encode.chars "pattern"
                ),
          "name"
            ==> Json.Encode.name name
        ]
    Warning.MissingTypeAnnotation region name type_ ->
      Json.Encode.object
        [ "warning" ==> Json.Encode.chars "MissingAnnotation",
          "region"
            ==> Watchtower.Editor.encodeRegion region,
          "name"
            ==> Json.Encode.chars (Name.toChars name),
          "signature"
            ==> Json.Encode.chars
              ( Reporting.Doc.toString
                  (Reporting.Render.Type.canToDoc localizer Reporting.Render.Type.None type_)
              )
        ]

{- Broadcasting -}

builderToString :: Data.ByteString.Builder.Builder -> T.Text
builderToString =
  T.decodeUtf8 . Data.ByteString.Lazy.toStrict . Data.ByteString.Builder.toLazyByteString

broadcastAll :: STM.TVar [Client] -> Outgoing -> IO ()
broadcastAll allClients outgoing =
  Watchtower.Websocket.broadcastWith
    allClients
    (\c -> True)
    ( builderToString $
        encodeOutgoing outgoing
    )

broadcastTo :: STM.TVar [Client] -> ClientId -> Outgoing -> IO ()
broadcastTo allClients id outgoing =
  do
    Ext.Log.log Ext.Log.Live (outgoingToLog outgoing)
    Watchtower.Websocket.broadcastWith
      allClients
      ( Watchtower.Websocket.matchId id
      )
      ( builderToString $
          encodeOutgoing outgoing
      )

broadcastToMany :: STM.TVar [Client] -> (Client -> Bool) -> Outgoing -> IO ()
broadcastToMany allClients shouldBroadcast outgoing =
  do
    Ext.Log.log Ext.Log.Live (outgoingToLog outgoing)
    Watchtower.Websocket.broadcastWith
      allClients
      shouldBroadcast
      ( builderToString $
          encodeOutgoing outgoing
      )

broadcast :: STM.TVar [Client] -> Outgoing -> IO ()
broadcast mClients msg =
  case msg of
    ElmStatus projectStatusList ->
      do
        broadcastToMany
          mClients
          ( \client ->
              let clientData = Watchtower.Websocket.clientData client

                  affectedProjectsThatWereListeningTo =
                    List.filter
                      ( \(ProjectStatus proj _ _ _) ->
                          isWatchingProject proj clientData
                      )
                      projectStatusList
               in -- This isn't entirely correct as it'll give all project statuses to this client
                  -- but :shrug: for now.
                  case affectedProjectsThatWereListeningTo of
                    [] -> False
                    _ ->
                      True
          )
          msg
    Warnings file localizer warnings ->
      do
        broadcastToMany
          mClients
          ( \client ->
              -- This lookup was failing
              -- possibly because the filepath format differs from the map?
              -- let
              --     clientData = Watchtower.Websocket.clientData client
              -- in
              -- isWatchingFileForWarnings file clientData
              True
          )
          msg
    Docs file docs ->
      do
        broadcastToMany
          mClients
          ( \client ->
              -- This lookup was failing
              -- possibly because the filepath format differs from the map?
              -- let
              --     clientData = Watchtower.Websocket.clientData client
              -- in
              -- isWatchingFileForDocs file clientData
              True
          )
          msg
    EditorViewing _ ->
      do
        broadcastToMany
          mClients
          (\client -> True)
          msg
    EditorJumpTo _ _ ->
      do
        broadcastToMany
          mClients
          (\client -> True)
          msg
    Tests _ _ ->
      do
        broadcastToMany mClients (\_ -> True) msg

-- Helper function to encode DocsConfig using Json.Encode functions
-- Needs to be kept in sync with the values in Config
-- We're in this situation because the elm compiler has it's own json encoder library.
encodeDocsConfig :: Gen.Config.DocsConfig -> Json.Encode.Value
encodeDocsConfig (Gen.Config.DocsConfig modules guides interactive) =
  Json.Encode.object $
    Maybe.catMaybes
      [ fmap (\m -> ("modules", Json.Encode.list (Json.Encode.string . Json.String.fromChars . T.unpack) m)) modules,
        fmap (\g -> ("guides", Json.Encode.list (Json.Encode.string . Json.String.fromChars . T.unpack) g)) guides,
        fmap (\i -> ("interactive", Json.Encode.list (Json.Encode.string . Json.String.fromChars . T.unpack) i)) interactive
      ]

-- Test results types and encoding
data TestResults = TestResults
  { _total :: Int
  , _passed :: Int
  , _failed :: Int
  , _failures :: [String]
  }

encodeTestResults :: FilePath -> TestResults -> Json.Encode.Value
encodeTestResults path (TestResults total passed failed failures) =
  Json.Encode.object
    [ "filepath" ==> Json.Encode.string (Json.String.fromChars path)
    , "total" ==> Json.Encode.int total
    , "passed" ==> Json.Encode.int passed
    , "failed" ==> Json.Encode.int failed
    , "failures" ==> Json.Encode.list Json.Encode.string (map Json.String.fromChars failures)
    ]
