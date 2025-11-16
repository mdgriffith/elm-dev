{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live.Client
  ( Client (..),
    initState,
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
    TestResults (..),
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
    toOldJSON,
    getStatus,
    -- Session helpers
    registerSession,
    unregisterSession,
    setFocusedProjectId,
    getFocusedProjectId
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
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
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
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified System.Directory as Dir
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Watchtower.Editor
import qualified Watchtower.Websocket
import qualified Ext.CompileHelpers.Generic
import qualified Watchtower.Server.LSP.EditorsOpen as EditorsOpen



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
    urls :: Urls,
    sessions :: STM.TVar (Map.Map T.Text Int),
    projectsBeingEdited :: STM.TVar EditorsOpen.EditorsOpen
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


initState :: Urls -> IO State
initState urls = do
  State
    <$> Watchtower.Websocket.clientsInit
    <*> STM.newTVarIO []
    <*> STM.newTVarIO Map.empty
    <*> STM.newTVarIO Map.empty
    <*> pure urls
    <*> STM.newTVarIO Map.empty
    <*> STM.newTVarIO EditorsOpen.empty

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
getFileInfo path (State _ _ mFileInfo _ _ _ _) = do
  fileInfo <- STM.readTVarIO mFileInfo
  pure (Map.lookup path fileInfo)

-- Read the entire FileInfo map
getAllFileInfos :: State -> IO (Map.Map FilePath FileInfo)
getAllFileInfos (State _ _ mFileInfo _ _ _ _) = STM.readTVarIO mFileInfo

-- Given a project and a canonical module name, try to find the first matching FileInfo
-- by generating potential file paths using the project's srcDirs and the module's Raw name.
getFileInfoFromModuleName :: ProjectCache -> ModuleName.Canonical -> State -> IO (Maybe FileInfo)
getFileInfoFromModuleName (ProjectCache proj _ _ _ _) (ModuleName.Canonical _pkg rawName) state@(State _ _ mFileInfo _ _ _ _) = do
  fileInfoMap <- STM.readTVarIO mFileInfo
  let moduleRelPath = ModuleName.toFilePath rawName ++ ".elm"
  let srcDirs = Ext.Dev.Project._srcDirs proj
  let candidates = fmap (\dir -> FilePath.normalise (dir </> moduleRelPath)) srcDirs
  pure (List.foldl (\acc p -> case acc of
                                Nothing -> Map.lookup p fileInfoMap
                                justVal -> justVal
                      ) Nothing candidates)

logFileInfoKeys :: State -> IO ()
logFileInfoKeys (State _ _ mFileInfo mPackages _ _ _) = do
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
getClientData clientId (State mClients _ _ _ _ _ _) = do
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
getRoot path (State _ mProjects _ _ _ _ _) =
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
getExistingProject path (State _ mProjects _ _ _ _ _) = do
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
getAllStatuses state@(State _ mProjects _ _ _ _ _) =
  do
    projects <- STM.readTVarIO mProjects

    Monad.foldM
      ( \statuses proj ->
          do
            status <- getStatus proj state
            pure (status : statuses)
      )
      []
      projects

-- Discover project modules (name and full path) by scanning src dirs
discoverProjectModules :: Ext.Dev.Project.Project -> IO [(String, FilePath)]
discoverProjectModules proj = do  
  Monad.foldM
    ( \acc dir -> do
        xs <- listElmFiles dir dir
        pure (acc <> xs)
    )
    []
    (Ext.Dev.Project._srcDirs proj)
  
  where
    -- listElmFiles takes the srcDir root and the current directory being scanned.
    -- It returns pairs of (Elm module name, absolute file path).
    listElmFiles :: FilePath -> FilePath -> IO [(String, FilePath)]
    listElmFiles srcDir dir = do
      isDir <- Dir.doesDirectoryExist dir
      if not isDir
        then pure []
        else do
          names <- Dir.listDirectory dir
          let paths = fmap (dir </>) names
          (subdirs, files) <-
            Monad.foldM
              ( \(ds, fs) p -> do
                  isSub <- Dir.doesDirectoryExist p
                  if isSub
                    then pure (p : ds, fs)
                    else pure (ds, if List.isSuffixOf ".elm" p then (p : fs) else fs)
              )
              ([], [])
              paths
          let filteredSubs = filter (not . shouldSkip) subdirs
          nested <- Monad.foldM (\acc d -> do xs <- listElmFiles srcDir d; pure (acc <> xs)) [] filteredSubs
          let filePairs =
                fmap
                  ( \full ->
                      let rel = FilePath.makeRelative srcDir full
                          noExt = FilePath.dropExtension rel
                          modName = fmap (\c -> if c == FilePath.pathSeparator then '.' else c) noExt
                       in (modName, full)
                  )
                  files
          pure (filePairs <> nested)

    shouldSkip :: FilePath -> Bool
    shouldSkip path =
      List.isInfixOf "node_modules" path
        || case FilePath.takeFileName path of
             ('.' : _) -> True
             _ -> False

-- Parse direct package dependency names from elm.json contents
directDepsFromElmJson :: Maybe String -> [String]
directDepsFromElmJson Nothing = []
directDepsFromElmJson (Just contents) =
  case Aeson.eitherDecode (BL8.pack contents) :: Either String Aeson.Value of
    Right (Aeson.Object obj) ->
      case KeyMap.lookup (AesonKey.fromString "dependencies") obj of
        Just (Aeson.Object depsObj) ->
          let directObj =
                case KeyMap.lookup (AesonKey.fromString "direct") depsObj of
                  Just (Aeson.Object d) -> d
                  _ -> depsObj
           in fmap (T.unpack . AesonKey.toText) (KeyMap.keys directObj)
        _ -> []
    _ -> []

-- For each dependency name, list module names if present in tracked packages
packageModulesForDeps :: [(Pkg.Name, PackageInfo)] -> [String] -> [(String, [String])]
packageModulesForDeps pkgList depNames =
  fmap
    ( \dep ->
        let found =
              List.find (\(pkgName, _) -> Pkg.toChars pkgName == dep) pkgList
         in case found of
              Nothing -> (dep, [])
              Just (_, pinfo) ->
                let names = fmap (ModuleName.toChars) (Map.keys (packageModules pinfo))
                 in (dep, names)
    )
    depNames

getStatus :: ProjectCache -> State -> IO ProjectStatus
getStatus (ProjectCache proj docsInfo _ mCompileResult _) (State _ _ _ mPackages _ _ _) =
  do
    result <- STM.readTVarIO mCompileResult
    let successful =
          case result of
            Success _ -> True
            _ -> False
    let json = toOldJSON result
    let elmJsonPath = Ext.Dev.Project._projectRoot proj </> "elm.json"
    exists <- Dir.doesFileExist elmJsonPath
    elmJsonContents <-
      if exists
        then Just <$> readFile elmJsonPath
        else pure Nothing
    modules <- discoverProjectModules proj
    packagesMap <- STM.readTVarIO mPackages
    let pkgList = Map.toList packagesMap
    let depNames = directDepsFromElmJson elmJsonContents
    let packages = packageModulesForDeps pkgList depNames
    pure (ProjectStatus proj successful json docsInfo elmJsonContents modules packages)

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
projectStatusToString (ProjectStatus proj success _ _ _ _ _) =
  if success
    then "Success: ../" ++ FilePath.takeBaseName (Ext.Dev.Project.getRoot proj)
    else "Failing: ../" ++ FilePath.takeBaseName (Ext.Dev.Project.getRoot proj)

data ProjectStatus = ProjectStatus
  { _project :: Ext.Dev.Project.Project,
    _success :: Bool,
    _json :: Json.Encode.Value,
    _docs :: Gen.Config.DocsConfig,
    _elmJson :: Maybe String,
    _modules :: [(String, FilePath)],
    _packages :: [(String, [String])]
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
                ( \(ProjectStatus project success status docs elmJson modules packages) ->
                    Json.Encode.object
                      [ "shortId" ==> Json.Encode.int (Ext.Dev.Project._shortId project),
                        "root"
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
                        "docs" ==> encodeDocsConfig docs,
                        "elmJson"
                          ==> ( case elmJson of
                                  Just contents -> Json.Encode.chars contents
                                  Nothing -> Json.Encode.null
                              ),
                        "modules"
                          ==> Json.Encode.list
                                ( \(name, path) ->
                                    Json.Encode.object
                                      [ "name" ==> Json.Encode.chars name
                                      , "path" ==> Json.Encode.chars path
                                      ]
                                )
                                modules,
                        "packages"
                          ==> Json.Encode.object
                                ( fmap
                                    ( \(pkgName, modNames) ->
                                        ( Json.String.fromChars pkgName
                                        , Json.Encode.list Json.Encode.chars modNames
                                        )
                                    )
                                    packages
                                )
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

encodeStatus (Ext.Dev.Project.Project root projectRoot entrypoints _srcDirs shortId, js) =
  Json.Encode.object
    [ "shortId" ==> Json.Encode.int shortId,
      "root" ==> Json.Encode.string (Json.String.fromChars root),
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
                      ( \(ProjectStatus proj _ _ _ _ _ _) ->
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

-- Session helpers

registerSession :: State -> T.Text -> IO ()
registerSession (State _ mProjects _ _ _ mSessions _) connId = do
  -- We don't need to do anything because we only need to track the session id
  -- If they've specifically asked for a specific project.
  pure ()
  
unregisterSession :: State -> T.Text -> IO ()
unregisterSession (State _ _ _ _ _ mSessions _) connId = do
  STM.atomically $ do
    sessionsMap <- STM.readTVar mSessions
    STM.writeTVar mSessions (Map.delete connId sessionsMap)

setFocusedProjectId :: State -> T.Text -> Int -> IO Bool
setFocusedProjectId (State _ mProjects _ _ _ mSessions _) connId pid = do
  projects <- STM.readTVarIO mProjects
  let exists = any (\(ProjectCache proj _ _ _ _) -> Ext.Dev.Project._shortId proj == pid) projects
  if exists
    then do
      STM.atomically $ do
        sessionsMap <- STM.readTVar mSessions
        STM.writeTVar mSessions (Map.insert connId pid sessionsMap)
      pure True
    else pure False

getFocusedProjectId :: State -> T.Text -> IO (Maybe Int)
getFocusedProjectId (State _ _ _ _ _ mSessions _) connId = do
  sessionsMap <- STM.readTVarIO mSessions
  pure (Map.lookup connId sessionsMap)
