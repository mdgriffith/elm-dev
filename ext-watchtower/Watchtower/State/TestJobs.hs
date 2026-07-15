{-# LANGUAGE OverloadedStrings #-}

module Watchtower.State.TestJobs
  ( Registry
  , JobId
  , JobStatus(..)
  , JobOutcome(..)
  , Completion(..)
  , ActionResult(..)
  , JobView(..)
  , newRegistry
  , globalRegistry
  , submit
  , submitWith
  , waitFor
  , waitForWith
  , lookupJob
  , lookupJobWith
  , listJobs
  , listJobsWith
  , listJobsForRoot
  , listJobsForRootWith
  , cancel
  , cancelWith
  , cancelAllWith
  , jobExpiresNoLaterThan
  ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Timeout as Timeout


type JobId = Text


data JobStatus
  = JobQueued
  | JobRunning
  | JobCompleted
  | JobFailed
  | JobCancelled
  deriving (Eq, Show)


data JobOutcome
  = TestsPassed
  | TestsFailed
  deriving (Eq, Show)


data Completion = Completion
  { completionOutcome :: JobOutcome
  , completionBody :: Text
  , completionPassed :: Int
  , completionFailed :: Int
  , completionTotal :: Int
  }
  deriving (Eq, Show)


data ActionResult
  = ActionCompleted Completion
  | ActionFailed Text
  deriving (Eq, Show)


data JobView = JobView
  { jobId :: JobId
  , jobRoot :: FilePath
  , jobStatus :: JobStatus
  , jobMessage :: Text
  , jobCreatedAt :: UTCTime
  , jobStartedAt :: Maybe UTCTime
  , jobCompletedAt :: Maybe UTCTime
  , jobCompletion :: Maybe Completion
  , jobFailure :: Maybe Text
  }
  deriving (Eq, Show)


data Entry = Entry
  { entryView :: JobView
  , entryThread :: Maybe Concurrent.ThreadId
  }


data RegistryState = RegistryState
  { nextId :: Int
  , entries :: Map.Map JobId Entry
  }


data Registry = Registry
  { registryState :: STM.TVar RegistryState
  , runLock :: MVar.MVar ()
  }


retentionSeconds :: NominalDiffTime
retentionSeconds = 60 * 60


maxTerminalJobsPerRoot :: Int
maxTerminalJobsPerRoot = 20


newRegistry :: IO Registry
newRegistry = do
  state <- STM.newTVarIO (RegistryState 1 Map.empty)
  lock <- MVar.newMVar ()
  pure (Registry state lock)


{-# NOINLINE globalRegistry #-}
globalRegistry :: Registry
globalRegistry = unsafePerformIO newRegistry


submit :: FilePath -> ((Text -> IO ()) -> IO ActionResult) -> IO JobView
submit = submitWith globalRegistry


submitWith :: Registry -> FilePath -> ((Text -> IO ()) -> IO ActionResult) -> IO JobView
submitWith registry root action = Exception.mask $ \restore -> do
  now <- getCurrentTime
  pruneAt registry now
  (newJobId, initialView) <- STM.atomically $ do
    current <- STM.readTVar (registryState registry)
    let newJobId = "test-" <> Text.pack (show (nextId current))
        view = JobView
          { jobId = newJobId
          , jobRoot = root
          , jobStatus = JobQueued
          , jobMessage = "Waiting to start"
          , jobCreatedAt = now
          , jobStartedAt = Nothing
          , jobCompletedAt = Nothing
          , jobCompletion = Nothing
          , jobFailure = Nothing
          }
        updated = current
          { nextId = nextId current + 1
          , entries = Map.insert newJobId (Entry view Nothing) (entries current)
          }
    STM.writeTVar (registryState registry) updated
    pure (newJobId, view)

  startGate <- MVar.newEmptyMVar
  threadId <- Concurrent.forkIO $ do
    MVar.takeMVar startGate
    runWorker registry newJobId (restore . action)
  STM.atomically $ modifyEntry registry newJobId $ \entry ->
    entry { entryThread = Just threadId }
  MVar.putMVar startGate ()
  pure initialView


runWorker :: Registry -> JobId -> ((Text -> IO ()) -> IO ActionResult) -> IO ()
runWorker registry targetId action =
  MVar.withMVar (runLock registry) $ \_ -> do
    now <- getCurrentTime
    shouldRun <- STM.atomically $ do
      current <- STM.readTVar (registryState registry)
      case Map.lookup targetId (entries current) of
        Just entry | jobStatus (entryView entry) == JobQueued -> do
          let view = (entryView entry)
                { jobStatus = JobRunning
                , jobMessage = "Starting tests"
                , jobStartedAt = Just now
                }
          STM.writeTVar (registryState registry) current
            { entries = Map.insert targetId (entry { entryView = view }) (entries current) }
          pure True
        _ -> pure False
    Monad.when shouldRun $ do
      result <- Exception.try (action (setProgress registry targetId))
      finishedAt <- getCurrentTime
      case result of
        Right (ActionCompleted completion) ->
          finishIfActive registry targetId finishedAt JobCompleted "Tests completed" (Just completion) Nothing
        Right (ActionFailed message) ->
          finishIfActive registry targetId finishedAt JobFailed "Test execution failed" Nothing (Just message)
        Left err ->
          case Exception.fromException err of
            Just Exception.ThreadKilled ->
              finishIfActive registry targetId finishedAt JobCancelled "Cancelled" Nothing Nothing
            _ ->
              finishIfActive registry targetId finishedAt JobFailed "Test execution failed" Nothing (Just (Text.pack (Exception.displayException (err :: Exception.SomeException))))


setProgress :: Registry -> JobId -> Text -> IO ()
setProgress registry targetId message =
  STM.atomically $ modifyEntry registry targetId $ \entry ->
    if jobStatus (entryView entry) == JobRunning
      then entry { entryView = (entryView entry) { jobMessage = message } }
      else entry


finishIfActive :: Registry -> JobId -> UTCTime -> JobStatus -> Text -> Maybe Completion -> Maybe Text -> IO ()
finishIfActive registry targetId finishedAt status message completion failure = do
  STM.atomically $ modifyEntry registry targetId $ \entry ->
    if isActive (jobStatus (entryView entry))
      then entry
        { entryView = (entryView entry)
            { jobStatus = status
            , jobMessage = message
            , jobCompletedAt = Just finishedAt
            , jobCompletion = completion
            , jobFailure = failure
            }
        , entryThread = Nothing
        }
      else entry
  pruneAt registry finishedAt


modifyEntry :: Registry -> JobId -> (Entry -> Entry) -> STM.STM ()
modifyEntry registry targetId update = do
  current <- STM.readTVar (registryState registry)
  case Map.lookup targetId (entries current) of
    Nothing -> pure ()
    Just entry ->
      STM.writeTVar (registryState registry) current
        { entries = Map.insert targetId (update entry) (entries current) }


waitFor :: JobId -> Int -> IO (Maybe JobView)
waitFor = waitForWith globalRegistry


waitForWith :: Registry -> JobId -> Int -> IO (Maybe JobView)
waitForWith registry targetId seconds
  | seconds <= 0 = lookupJobWith registry targetId
  | otherwise = do
      waited <- Timeout.timeout (seconds * 1000000) $ STM.atomically $ do
        current <- STM.readTVar (registryState registry)
        case Map.lookup targetId (entries current) of
          Nothing -> pure Nothing
          Just entry ->
            if isTerminal (jobStatus (entryView entry))
              then pure (Just (entryView entry))
              else STM.retry
      case waited of
        Just result -> pure result
        Nothing -> lookupJobWith registry targetId


lookupJob :: JobId -> IO (Maybe JobView)
lookupJob = lookupJobWith globalRegistry


lookupJobWith :: Registry -> JobId -> IO (Maybe JobView)
lookupJobWith registry targetId = do
  now <- getCurrentTime
  pruneAt registry now
  current <- STM.readTVarIO (registryState registry)
  pure (entryView <$> Map.lookup targetId (entries current))


listJobs :: IO [JobView]
listJobs = listJobsWith globalRegistry


listJobsWith :: Registry -> IO [JobView]
listJobsWith registry = do
  now <- getCurrentTime
  pruneAt registry now
  current <- STM.readTVarIO (registryState registry)
  pure (sortNewest (map entryView (Map.elems (entries current))))


listJobsForRoot :: FilePath -> IO [JobView]
listJobsForRoot = listJobsForRootWith globalRegistry


listJobsForRootWith :: Registry -> FilePath -> IO [JobView]
listJobsForRootWith registry root =
  filter ((== root) . jobRoot) <$> listJobsWith registry


cancel :: JobId -> IO (Maybe JobView)
cancel = cancelWith globalRegistry


cancelWith :: Registry -> JobId -> IO (Maybe JobView)
cancelWith registry targetId = do
  now <- getCurrentTime
  result <- STM.atomically $ do
    current <- STM.readTVar (registryState registry)
    case Map.lookup targetId (entries current) of
      Nothing -> pure Nothing
      Just entry ->
        if isTerminal (jobStatus (entryView entry))
          then pure (Just (entryView entry, Nothing))
          else do
            let view = (entryView entry)
                  { jobStatus = JobCancelled
                  , jobMessage = "Cancelled"
                  , jobCompletedAt = Just now
                  }
                updatedEntry = entry { entryView = view, entryThread = Nothing }
            STM.writeTVar (registryState registry) current
              { entries = Map.insert targetId updatedEntry (entries current) }
            pure (Just (view, entryThread entry))
  case result of
    Nothing -> pure Nothing
    Just (view, maybeThread) -> do
      mapM_ Concurrent.killThread maybeThread
      pruneAt registry now
      pure (Just view)


cancelAllWith :: Registry -> IO ()
cancelAllWith registry = do
  jobs <- listJobsWith registry
  mapM_ (cancelWith registry . jobId) (filter (isActive . jobStatus) jobs)


pruneAt :: Registry -> UTCTime -> IO ()
pruneAt registry now = STM.atomically $ do
  current <- STM.readTVar (registryState registry)
  let freshEntries = Map.filter (not . expired now . entryView) (entries current)
      activeEntries = Map.filter (isActive . jobStatus . entryView) freshEntries
      terminalEntries = Map.filter (isTerminal . jobStatus . entryView) freshEntries
      roots = List.nub (map (jobRoot . entryView) (Map.elems terminalEntries))
      retainedForRoot root =
        take maxTerminalJobsPerRoot
          (List.sortOn (Down . terminalSortTime . entryView)
            (filter ((== root) . jobRoot . entryView) (Map.elems terminalEntries)))
      retainedTerminal = Map.fromList
        [ (jobId (entryView entry), entry)
        | root <- roots
        , entry <- retainedForRoot root
        ]
  STM.writeTVar (registryState registry) current
    { entries = Map.union activeEntries retainedTerminal }


expired :: UTCTime -> JobView -> Bool
expired now view =
  case jobCompletedAt view of
    Nothing -> False
    Just completedAt -> diffUTCTime now completedAt > retentionSeconds


terminalSortTime :: JobView -> UTCTime
terminalSortTime view =
  case jobCompletedAt view of
    Just completedAt -> completedAt
    Nothing -> jobCreatedAt view


sortNewest :: [JobView] -> [JobView]
sortNewest = List.sortOn (Down . jobCreatedAt)


jobExpiresNoLaterThan :: JobView -> Maybe UTCTime
jobExpiresNoLaterThan view = fmap (addUTCTime retentionSeconds) (jobCompletedAt view)


isActive :: JobStatus -> Bool
isActive status = status == JobQueued || status == JobRunning


isTerminal :: JobStatus -> Bool
isTerminal = not . isActive
