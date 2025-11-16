{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.Dev (routes) where

import qualified Control.Concurrent.STM as STM
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Elm.Docs as Docs
import qualified Ext.Dev
import qualified Gen.Generate
import qualified Gen.Javascript
import qualified Json.Encode
import qualified Watchtower.Live as Live
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Server.LSP.EditorsOpen as EditorsOpen
import qualified Watchtower.Server.JSONRPC as JSONRPC
import qualified Watchtower.State.Project
import qualified Watchtower.State.Compile
import qualified Modify.Inject.Loader
import qualified Ext.CompileHelpers.Generic as CompileHelpers
import qualified Data.NonEmptyList as NE
import qualified Data.List as List
import qualified Reporting.Exit
import qualified Ext.Log
import Snap.Core
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import qualified Ext.FileCache
import System.FilePath ((</>), (<.>))
import qualified File
import qualified GHC.Stats as RT
import qualified System.Mem as Mem
import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word64)
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import qualified Stuff
import qualified Control.Exception


-- Convert bytes to megabytes (MiB)
bytesToMb :: (Integral a) => a -> Double
bytesToMb b = (fromIntegral b) / (1024.0 * 1024.0)

nearestPow2 :: Double -> Int
nearestPow2 x =
  let candidates = [1,2,4,8,16,32,64,128,256,512]
   in if x <= 0 then 1 else List.minimumBy (Ord.comparing (\p -> abs (fromIntegral p - x))) candidates

clamp :: Ord a => a -> a -> a -> a
clamp lo hi v = max lo (min hi v)

-- Track memory spikes around compile
{-# NOINLINE compileStartRef #-}
compileStartRef :: IORef (Maybe (Word64, Word64))
compileStartRef = unsafePerformIO (newIORef Nothing)

{-# NOINLINE compileSpikeRef #-}
compileSpikeRef :: IORef (Maybe (Word64, Word64))
compileSpikeRef = unsafePerformIO (newIORef Nothing)

markCompileStart :: IO ()
markCompileStart = do
  enabled <- RT.getRTSStatsEnabled
  if not enabled
    then pure ()
    else do
      s <- RT.getRTSStats
      writeIORef compileStartRef (Just (RT.max_mem_in_use_bytes s, RT.max_live_bytes s))

markCompileEnd :: IO ()
markCompileEnd = do
  enabled <- RT.getRTSStatsEnabled
  if not enabled
    then pure ()
    else do
      s <- RT.getRTSStats
      mPre <- readIORef compileStartRef
      case mPre of
        Nothing -> writeIORef compileSpikeRef Nothing
        Just (preMem, preLive) -> do
          let dm = RT.max_mem_in_use_bytes s - preMem
              dl = RT.max_live_bytes s - preLive
          writeIORef compileSpikeRef (Just (dm, dl))

-- Rolling samples of per-thread allocation (MB) observed at last GC
{-# NOINLINE allocPerThreadMbRef #-}
allocPerThreadMbRef :: IORef [Double]
allocPerThreadMbRef = unsafePerformIO (newIORef [])

{-# NOINLINE liveAfterGcMbRef #-}
liveAfterGcMbRef :: IORef [Double]
liveAfterGcMbRef = unsafePerformIO (newIORef [])

recordGcSampleFrom :: RT.RTSStats -> IO ()
recordGcSampleFrom s = do
  let gc = RT.gc s
      threads = max 1 (fromIntegral (RT.gcdetails_threads gc) :: Double)
      perThreadMb = bytesToMb (RT.gcdetails_allocated_bytes gc) / threads
  xs <- readIORef allocPerThreadMbRef
  let xs' = take 200 (perThreadMb : xs)
  writeIORef allocPerThreadMbRef xs'
  ys <- readIORef liveAfterGcMbRef
  let liveMb = bytesToMb (RT.gcdetails_live_bytes gc)
      ys' = take 200 (liveMb : ys)
  writeIORef liveAfterGcMbRef ys'

recordPostCompileSample :: IO ()
recordPostCompileSample = do
  enabled <- RT.getRTSStatsEnabled
  when enabled $ do
    s <- RT.getRTSStats
    recordGcSampleFrom s

-- Track last RTS totals to compute rates
{-# NOINLINE lastTotalsRef #-}
lastTotalsRef :: IORef (Maybe (Integer, Integer, Integer))
lastTotalsRef = unsafePerformIO (newIORef Nothing)

minorGcRatePerSecond :: RT.RTSStats -> IO (Maybe Double)
minorGcRatePerSecond s = do
  let totalGcs = RT.gcs s
      major = RT.major_gcs s
      minor = totalGcs - major
      elapsed = RT.elapsed_ns s -- since start
  prev <- readIORef lastTotalsRef
  writeIORef lastTotalsRef (Just (toInteger totalGcs, toInteger major, toInteger elapsed))
  case prev of
    Nothing -> pure Nothing
    Just (pTotal, pMajor, pElapsed) -> do
      let pMinor :: Integer
          pMinor = pTotal - pMajor
          dMinor :: Integer
          dMinor = toInteger minor - pMinor
          dElapsedNs :: Integer
          dElapsedNs = toInteger elapsed - pElapsed
      if dElapsedNs <= 0 || dMinor < 0
        then pure Nothing
        else pure (Just (fromIntegral dMinor / (fromIntegral dElapsedNs / 1.0e9)))

quantile :: Double -> [Double] -> Maybe Double
quantile q xs =
  case List.sort xs of
    [] -> Nothing
    ys ->
      let n = length ys
          i = max 0 (min (n - 1) (floor (q * fromIntegral (n - 1))))
       in Just (ys !! i)

-- | JSON decoders/encoders live in Watchtower.Server.JSONRPC; we mirror MCP style here

-- Parameters for js method
data JsParams = JsParams
  { jsDir :: FilePath
  , jsFile :: FilePath
  , jsDebug :: Maybe Bool
  , jsOptimize :: Maybe Bool
  }

instance JSON.FromJSON JsParams where
  parseJSON = JSON.withObject "JsParams" $ \o ->
    JsParams <$> o JSON..: "dir"
             <*> o JSON..: "file"
             <*> o JSON..:? "debug"
             <*> o JSON..:? "optimize"

-- Parameters for log method
data LogParams = LogParams
  { logDir :: FilePath
  , module_ :: String
  , identifier :: String
  , payload :: JSON.Value
  }

instance JSON.FromJSON LogParams where
  parseJSON = JSON.withObject "LogParams" $ \o ->
    LogParams
      <$> o JSON..: "dir"
      <*> o JSON..: "module"
      <*> o JSON..: "identifier"
      <*> o JSON..: "payload"

-- Parameters for interactive method
data InteractiveParams = InteractiveParams
  { interactiveDir :: FilePath
  , file :: FilePath
  }

instance JSON.FromJSON InteractiveParams where
  parseJSON = JSON.withObject "InteractiveParams" $ \o ->
    InteractiveParams <$> o JSON..: "dir" <*> o JSON..: "file"

-- | Snap routes for dev endpoints (non-JSON-RPC)
routes :: Live.State -> [(BS.ByteString, Snap ())]
routes state =
  [ ("/dev/js", jsHandler state)
  , ("/dev/interactive", interactiveHandler state)
  , ("/dev/log", logHandler)
  , ("/dev/package", packageHandler state)
  , ("/dev/fileChanged", fileChangedHandler state)
  , ("/dev/memory", memoryHandler state)
  , ("/projectList", projectListHandler state)
  , ("/dev/service/status", serviceStatusHandler state)
  ]

-- Helpers

-- Return the compileResult JSON for a project rooted by dir
getProjectStatus :: Live.State -> FilePath -> IO (Either String JSON.Value)
getProjectStatus liveState root = do
  existing <- Client.getExistingProject root liveState
  case existing of
    Left Client.NoProjectsRegistered ->
      pure $ Left "No projects registered"
    Left (Client.ProjectNotFound _) ->
      pure $ Left "Project not found"
    Right (Client.ProjectCache _ _ _ mCompileResult _, _) -> do
      result <- STM.readTVarIO mCompileResult
      pure (Right (Client.encodeCompilationResult result))

-- Snap handlers

jsHandler :: Live.State -> Snap ()
jsHandler state = do
  mDir <- getParam "dir"
  mFile <- getParam "file"
  mDebug <- getParam "debug"
  mOptimize <- getParam "optimize"
  mReport <- getParam "report"
  case (mDir, mFile) of
    (Nothing, _) -> problemSnap 400 "bad_request" (Text.pack "Missing query param: dir")
    (_, Nothing) -> problemSnap 400 "bad_request" (Text.pack "Missing query param: file")
    (Just dirBs, Just fileBs) -> do
      let dir = Text.unpack (Data.Text.Encoding.decodeUtf8 dirBs)
      let file = Text.unpack (Data.Text.Encoding.decodeUtf8 fileBs)
      let debug = parseBoolParamWithDefault False mDebug
      let optimize = parseBoolParamWithDefault False mOptimize
      let report = parseReportParam mReport
      e <- liftIO (compile state dir file debug optimize report)
      case e of
        Left errOut -> do
          modifyResponse $ setResponseCode 422
          case errOut of
            ErrorJson errJson -> do
              modifyResponse $ setContentType "application/json"
              writeLBS (JSON.encode errJson)
            ErrorTerminal msg -> do
              modifyResponse $ setContentType "text/plain; charset=utf-8"
              writeBS (Data.Text.Encoding.encodeUtf8 msg)
        Right jsBuilder -> do
          modifyResponse $ setContentType "application/javascript; charset=utf-8"
          writeBuilder jsBuilder

interactiveHandler :: Live.State -> Snap ()
interactiveHandler state = do
  mDir <- getParam "dir"
  mFile <- getParam "file"
  case (mDir, mFile) of
    (Nothing, _) -> problemSnap 400 "bad_request" (Text.pack "Missing query param: dir")
    (_, Nothing) -> problemSnap 400 "bad_request" (Text.pack "Missing query param: file")
    (Just dirBs, Just fileBs) -> do
      let dir = Text.unpack (Data.Text.Encoding.decodeUtf8 dirBs)
      let file = Text.unpack (Data.Text.Encoding.decodeUtf8 fileBs)
      value <- liftIO (interactiveDocs state dir file)
      modifyResponse $ setContentType "application/json"
      writeLBS (JSON.encode value)

logHandler :: Snap ()
logHandler = do
  body <- readRequestBody 1048576 -- 1MB
  case decodeAsValue body of
    Left _ -> problemSnap 400 "bad_request" (Text.pack "Invalid JSON body")
    Right _ -> do
      modifyResponse $ setContentType "application/json"
      writeLBS (JSON.encode (JSON.object ["ok" JSON..= True]))

-- Return package elm.json and README, given ?name=owner/project@version or owner/project/version
packageHandler :: Live.State -> Snap ()
packageHandler _state = do
  mName <- getParam "name"
  case mName of
    Nothing -> problemSnap 400 "bad_request" (Text.pack "Missing query param: name")
    Just nameBs -> do
      let nameStr = Text.unpack (Data.Text.Encoding.decodeUtf8 nameBs)
      case parsePkg nameStr of
        Nothing ->
          problemSnap 400 "bad_request" (Text.pack "Expected name like owner/project@version or owner/project/version")
        Just (owner, project, version) -> do
          elmHome <- liftIO Stuff.getElmHome
          let base = elmHome </> "0.19.1" </> "packages" </> owner </> project </> version
          readmeBytes <- liftIO (readFileSafe (base </> "README.md"))
          elmJsonBytes <- liftIO (readFileSafe (base </> "elm.json"))
          docsBytes <- liftIO (readFileSafe (base </> "docs.json"))
          let docsVal =
                case docsBytes of
                  Nothing -> JSON.Null
                  Just bs ->
                    case JSON.eitherDecodeStrict bs of
                      Left _ -> JSON.Null
                      Right v -> v
          let readmeVal =
                case readmeBytes of
                  Nothing -> JSON.Null
                  Just bs -> JSON.String (T.decodeUtf8 bs)
          let elmJsonVal =
                case elmJsonBytes of
                  Nothing -> JSON.Null
                  Just bs ->
                    case JSON.eitherDecodeStrict bs of
                      Left _ -> JSON.Null
                      Right v -> v
          modifyResponse $ setContentType "application/json"
          writeLBS
            ( JSON.encode
                ( JSON.object
                    [ "name" JSON..= (owner <> "/" <> project)
                    , "version" JSON..= version
                    , "readme" JSON..= readmeVal
                    , "elmJson" JSON..= elmJsonVal
                    , "docs" JSON..= docsVal
                    ]
                )
            )

  where
    parsePkg :: String -> Maybe (String, String, String)
    parsePkg str =
      case break (== '@') str of
        (beforeAt, '@' : v) ->
          case splitSlash beforeAt of
            Just (o, p) -> Just (o, p, v)
            Nothing -> Nothing
        _ ->
          case splitSlashVersion str of
            Just (o, p, v) -> Just (o, p, v)
            Nothing -> Nothing

    splitSlash :: String -> Maybe (String, String)
    splitSlash s =
      case break (== '/') s of
        (o, '/' : p) | not (null o) && not (null p) -> Just (o, p)
        _ -> Nothing

    splitSlashVersion :: String -> Maybe (String, String, String)
    splitSlashVersion s =
      case splitSlash s of
        Just (o, rest) ->
          case break (== '/') rest of
            (p, '/' : v) | not (null p) && not (null v) -> Just (o, p, v)
            _ -> Nothing
        _ -> Nothing
    
-- Safely read a file; return empty bytes if missing or unreadable
readFileSafe :: FilePath -> IO (Maybe BS.ByteString)
readFileSafe path = do
  r <- Control.Exception.try (BS.readFile path) :: IO (Either Control.Exception.IOException BS.ByteString)
  case r of
    Left _ -> pure Nothing
    Right bs -> pure (Just bs)

-- Memory summary
memoryHandler :: Live.State -> Snap ()
memoryHandler state = do
  mGc <- getParam "gc"
  let doGc = parseBoolParamWithDefault False mGc
  when doGc (liftIO Mem.performMajorGC)
  vfs <- liftIO Ext.FileCache.fileCacheStats
  let (vfsCount, vfsBytes, vfsOverhead) = vfs
  -- Server state sizes (counts only; not bytes):
  (projCount, fileInfoCount, packageCount) <- liftIO $ do
    projects <- STM.readTVarIO (Client.projects state)
    fileInfo <- STM.readTVarIO (Client.fileInfo state)
    packages <- STM.readTVarIO (Client.packages state)
    pure (length projects, Map.size fileInfo, Map.size packages)

  -- RTS stats (enabled only with +RTS -T). If disabled, report enabled: false
  rtsJson <- liftIO $ do
    enabled <- RT.getRTSStatsEnabled
    if not enabled
      then pure (JSON.object ["enabled" JSON..= False])
      else do
        s <- RT.getRTSStats
        let gc = RT.gc s
        spike <- readIORef compileSpikeRef
        recordGcSampleFrom s
        let spikeJson = case spike of
              Nothing -> JSON.Null
              Just (dm, dl) -> JSON.object
                [ "delta_max_mem_in_use_mb" JSON..= bytesToMb dm
                , "delta_max_live_mb" JSON..= bytesToMb dl
                ]
        -- Heuristic: choose an -A (MB) near the live bytes seen after last major GC,
        -- but not exceeding max_live; clamp to [1,128] and snap to nearest power of two.
        let liveMb = bytesToMb (RT.gcdetails_live_bytes gc)
            maxLiveMb = bytesToMb (RT.max_live_bytes s)
            target = max 1 (min liveMb maxLiveMb)
            suggestedA = clamp 1 512 (nearestPow2 target)
            suggestedStr = if target > 512 then "Over 512mb!" else show suggestedA ++ "mb"
        xs <- readIORef allocPerThreadMbRef
        ys <- readIORef liveAfterGcMbRef
        minorRate <- minorGcRatePerSecond s
        let p95 = quantile 0.95 xs -- Maybe Double
            pmax = if null xs then Nothing else Just (maximum xs)
            safety = 1.25 :: Double
            suggestedFromP95 = fmap (\v -> nearestPow2 (clamp 1 512 (safety * v))) p95 -- Maybe Int
            p95Str = fmap (\v -> show (nearestPow2 v) ++ "mb") p95
            pmaxStr = fmap (\v -> show (nearestPow2 v) ++ "mb") pmax
            arenaBaseMb :: Double
            arenaBaseMb = fromIntegral suggestedA
            p95OverMb = fmap (\v -> max 0 (v - arenaBaseMb)) p95
            pmaxOverMb = fmap (\v -> max 0 (v - arenaBaseMb)) pmax
        pure (JSON.object
          [ "enabled" JSON..= True
          , "gcs" JSON..= RT.gcs s
          , "major_gcs" JSON..= RT.major_gcs s
          , "allocated_mb" JSON..= bytesToMb (RT.allocated_bytes s)
          , "cumulative_live_mb" JSON..= bytesToMb (RT.cumulative_live_bytes s)
          , "max_live_mb" JSON..= bytesToMb (RT.max_live_bytes s)
          , "max_mem_in_use_mb" JSON..= bytesToMb (RT.max_mem_in_use_bytes s)
          , "compile_spike" JSON..= spikeJson
          , "suggested_arena" JSON..= suggestedStr
          , "alloc_per_thread_samples" JSON..= length xs
          , "hints" JSON..= JSON.object
              [ "arena_too_small" JSON..= JSON.object
                  [ "minor_gc_rate_per_sec" JSON..= maybe JSON.Null JSON.toJSON (minorRate)
                  , "arena_overage_mb" JSON..= JSON.object
                      [ "p95" JSON..= maybe JSON.Null JSON.toJSON p95OverMb
                      , "max" JSON..= maybe JSON.Null JSON.toJSON pmaxOverMb
                      ]
                  ]
              ]
          , "per_thread" JSON..= JSON.object
              [ "allocated_mb_p95" JSON..= maybe JSON.Null (JSON.toJSON . id) (quantile 0.95 xs)
              , "allocated_mb_max" JSON..= maybe JSON.Null (JSON.toJSON . id) pmax
              , "allocated_mb_last" JSON..= (if null xs then JSON.Null else JSON.toJSON (head xs))
              , "suggested_arena_p95" JSON..= maybe JSON.Null (\s -> JSON.String (Text.pack s)) p95Str
              , "suggested_arena_max" JSON..= maybe JSON.Null (\s -> JSON.String (Text.pack s)) pmaxStr
              ]
          , "global" JSON..= JSON.object
              [ "currently_used_mb" JSON..= bytesToMb (RT.gcdetails_live_bytes gc)
              , "p95_used_mb" JSON..= maybe JSON.Null (JSON.toJSON . id) (quantile 0.95 ys)
              , "max_used_mb" JSON..= (if null ys then JSON.Null else JSON.toJSON (maximum ys))
              ]
          , "gc" JSON..= JSON.object
              [ "allocated_mb" JSON..= bytesToMb (RT.gcdetails_allocated_bytes gc)
              , "live_mb" JSON..= bytesToMb (RT.gcdetails_live_bytes gc)
              , "large_objects_mb" JSON..= bytesToMb (RT.gcdetails_large_objects_bytes gc)
              , "compact_mb" JSON..= bytesToMb (RT.gcdetails_compact_bytes gc)
              , "slop_mb" JSON..= bytesToMb (RT.gcdetails_slop_bytes gc)
              , "mem_in_use_mb" JSON..= bytesToMb (RT.gcdetails_mem_in_use_bytes gc)
              ]
          ])

  modifyResponse $ setContentType "application/json"
  writeLBS (JSON.encode (JSON.object
    [ "gc_performed" JSON..= doGc
    , "vfs" JSON..= JSON.object
        [ "entries" JSON..= vfsCount
        , "mb" JSON..= bytesToMb vfsBytes
        , "overhead" JSON..= vfsOverhead
        ]
    , "server" JSON..= JSON.object
        [ "projects" JSON..= projCount
        , "fileInfo" JSON..= fileInfoCount
        , "packages" JSON..= packageCount
        ]
    , "rts" JSON..= rtsJson
    ]))

-- Notify server that an absolute file path changed
fileChangedHandler :: Live.State -> Snap ()
fileChangedHandler state = do
  mPath <- getParam "path"
  case mPath of
    Nothing -> problemSnap 400 "bad_request" (Text.pack "Missing query param: path")
    Just pathBs -> do
      liftIO (Ext.Log.log Ext.Log.Live ("---- File changed: " <> show pathBs))
      let path = Text.unpack (Data.Text.Encoding.decodeUtf8 pathBs)
      ok <- liftIO (File.exists path)
      if not ok
        then problemSnap 404 "not_found" (Text.pack "File not found")
        else do
          -- 1) Read from FS and write to cache
          contents <- liftIO (File.readUtf8 path)
          _ <- liftIO (Ext.FileCache.writeUtf8 path contents)

          -- 2) Compile relevant projects
          _ <- liftIO (Watchtower.State.Compile.compileRelevantProjects state [path])

          -- 3) Broadcasts are handled inside compile; nothing to do here
          modifyResponse $ setContentType "application/json"
          writeLBS (JSON.encode (JSON.object ["ok" JSON..= True]))

-- Return the list of projects in the same JSON shape as websocket "Status" message
projectListHandler :: Live.State -> Snap ()
projectListHandler state = do
  statuses <- liftIO (Client.getAllStatuses state)
  modifyResponse $ setContentType "application/json"
  liftIO (Ext.Log.log Ext.Log.Live ("Project list: " <> show (length statuses) <> " projects"))
  writeBuilder (Client.encodeOutgoing (Client.ElmStatus statuses))

-- Report current service status: sessions and editors open
serviceStatusHandler :: Live.State -> Snap ()
serviceStatusHandler state = do
  sessionsMap <- liftIO (STM.readTVarIO (Client.sessions state))
  editorsOpen <- liftIO (STM.readTVarIO (Client.projectsBeingEdited state))
  let editorsJson =
        case editorsOpen of
          EditorsOpen.EditorsOpen m -> JSON.toJSON m
  modifyResponse $ setContentType "application/json"
  writeLBS
    ( JSON.encode
        ( JSON.object
            [ "sessions" JSON..= sessionsMap
            , "editorsOpen" JSON..= editorsJson
            ]
        )
    )

-- Return raw JS as a Builder for efficient streaming to Snap
data Report = ReportJson | ReportTerminal

data ErrorOutput = ErrorJson JSON.Value | ErrorTerminal Text.Text

compile :: Live.State -> FilePath -> FilePath -> Bool -> Bool -> Report -> IO (Either ErrorOutput Builder.Builder)
compile state root file debug optimize report = do
  let wsUrl = Client.urlsDevWebsocket (Client.urls state)
      desired = CompileHelpers.getMode debug optimize
      flags = CompileHelpers.Flags
                desired
                (CompileHelpers.OutputTo CompileHelpers.Js)
      hotJs = Modify.Inject.Loader.hotJs wsUrl


  -- Find an existing project whose elm.json root equals the given root; otherwise create it
  existingProjects <- STM.readTVarIO (Client.projects state)
  eProj <- Watchtower.State.Project.upsert state flags root (NE.List file [])
  case eProj of
    Left upErr -> do
      let detail =
            case upErr of
              Watchtower.State.Project.NoElmJson r ->
                Text.pack ("No elm.json found at root: " <> r)
              Watchtower.State.Project.EntrypointNotFound ps ->
                Text.pack ("Entrypoint does not exist: " <> List.intercalate ", " ps)
              Watchtower.State.Project.EntrypointOutsideSourceDirs p srcs ->
                Text.pack ("Entrypoint is outside source-directories: " <> p <> " (srcDirs: " <> List.intercalate ", " srcs <> ")")
      pure (Left (ErrorTerminal detail))
    Right projCache -> do
      liftIO markCompileStart
      compiledR <- Watchtower.State.Compile.compile state projCache [file]
      liftIO markCompileEnd
      -- Record a GC sample after each compile completes
      recordPostCompileSample
      Ext.Log.log Ext.Log.Live ("Recompiled")
      case compiledR of
        Left clientErr -> do
          Ext.Log.log Ext.Log.Live ("Compilation error")
          case report of
            ReportJson -> do
              let errJson = Client.encodeCompilationResult (Client.Error clientErr)
              pure (Left (ErrorJson errJson))
            ReportTerminal -> do
              let msg = case clientErr of
                          Client.ReactorError exit -> Text.pack (Reporting.Exit.toAnsiString (Reporting.Exit.reactorToReport exit))
                          Client.GenerationError err -> Text.pack err
              pure (Left (ErrorTerminal msg))
        Right result ->
          case result of
            CompileHelpers.CompiledJs jsBuilder -> do
              Ext.Log.log Ext.Log.Live ("Compiled JS")
              pure (Right (hotJs <> jsBuilder))

            CompileHelpers.CompiledHtml _ -> do
              Ext.Log.log Ext.Log.Live ("HTML output not supported on /dev/js")
              pure (Left (ErrorTerminal (Text.pack "HTML output not supported on /dev/js")))
                
            CompileHelpers.CompiledSkippedOutput -> do
              Ext.Log.log Ext.Log.Live ("Compilation skipped")
              pure (Left (ErrorTerminal (Text.pack "Compilation skipped")))
          

-- Perform the same logic as Questions.Interactive branch
interactiveDocs :: Live.State -> FilePath -> FilePath -> IO JSON.Value
interactiveDocs liveState cwd filepath = do
  maybeDocs <- Ext.Dev.docs cwd filepath
  case maybeDocs of
    Nothing -> pure (JSON.toJSON (Text.pack "Docs are not available"))
    Just docs -> do
      let docsJson = Docs.encode (Docs.toDict [docs])
      let docsProject = Json.Encode.object [("project", docsJson), ("viewers", Json.Encode.array [])]
      let flags = Json.Encode.object [("flags", docsProject)]
      let input = LBS.toStrict (Builder.toLazyByteString (Json.Encode.encodeUgly flags))
      result <- Gen.Javascript.run Gen.Javascript.interactiveJs input
      case result of
        Left err -> pure (JSON.toJSON err)
        Right output -> do
          case JSON.eitherDecodeStrict (Data.Text.Encoding.encodeUtf8 (Text.pack output)) of
            Left err -> pure (JSON.toJSON err)
            Right (Gen.Generate.GeneratedFiles _generatedFiles) -> do
              -- Return the docs JSON as Aeson.Value to mirror Questions.Interactive behavior
              let lbs = Builder.toLazyByteString (Json.Encode.encodeUgly docsJson)
              case JSON.eitherDecode lbs of
                Left e -> pure (JSON.String (Text.pack e))
                Right v -> pure v

-- Utilities

parseBoolParamWithDefault :: Bool -> Maybe BS.ByteString -> Bool
parseBoolParamWithDefault def m =
  case fmap (Text.toLower . Data.Text.Encoding.decodeUtf8) m of
    Just "1" -> True
    Just "true" -> True
    Just "yes" -> True
    Just "0" -> False
    Just "false" -> False
    Just "no" -> False
    _ -> def

parseReportParam :: Maybe BS.ByteString -> Report
parseReportParam m =
  case fmap (Text.toLower . Data.Text.Encoding.decodeUtf8) m of
    Just "json" -> ReportJson
    _ -> ReportTerminal

problemSnap :: Int -> BS.ByteString -> Text.Text -> Snap ()
problemSnap code problemCode detail = do
  modifyResponse $ setResponseCode code
  modifyResponse $ setContentType "application/json"
  writeLBS (JSON.encode (JSON.object
    [ "type" JSON..= (Text.pack "about:blank")
    , "status" JSON..= code
    , "code" JSON..= (Data.Text.Encoding.decodeUtf8 problemCode)
    , "detail" JSON..= detail
    ]))

-- Explicitly-typed JSON decode helper to avoid inline annotations at call-site
decodeAsValue :: LBS.ByteString -> Either String JSON.Value
decodeAsValue = JSON.eitherDecode


