{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.Common where

import Control.Concurrent
import Control.Concurrent.MVar

import qualified Data.List as List
import Text.Read (readMaybe)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout, hClose, openTempFile)
import System.Exit (exitFailure)
import qualified System.FilePath as FP
import qualified System.Directory as Dir
import qualified System.Environment as Env
import Control.Monad (unless)
import qualified Data.Text as T

import Control.Exception ()
import Formatting (fprint, (%), sformat)
import Formatting.Clock (timeSpecs)
import System.Clock (Clock(..), getTime)


-- Re-exports
import qualified Data.Function


-- Copy of combined internals of Project.getRoot as it seems to notoriously cause cyclic wherever imported
-- Extended to accept a PROJECT env var, helpful for testing
getProjectRoot :: IO FilePath
getProjectRoot = do
  projectM <- Env.lookupEnv "PROJECT"
  projectDir <-
    case projectM of
      Just project -> pure project
      Nothing -> Dir.getCurrentDirectory

  res <- findHelp "elm.json" (FP.splitDirectories projectDir)
  case res of
    Just filepath -> pure filepath
    Nothing -> do
      binName <- Env.getProgName
      putStrLn $ "Cannot find an elm.json! Make sure you're in a project folder, or run `" <> binName <> " init` to start a new one."
      debug $ "projectDir: " <> projectDir
      exitFailure


findHelp :: FilePath -> [String] -> IO (Maybe FilePath)
findHelp name dirs =
  if Prelude.null dirs then
    return Nothing

  else
    do  exists_ <- Dir.doesFileExist (FP.joinPath dirs </> name)
        if exists_
          then return (Just (FP.joinPath dirs))
          else findHelp name (Prelude.init dirs)


-- Safe find the project root from an arbitrary fle path
getProjectRootFor :: FilePath -> IO (Maybe FilePath)
getProjectRootFor path = do
  findHelp "elm.json" (FP.splitDirectories $ FP.takeDirectory path)


{- Helpers -}


withDefault :: a -> Maybe a -> a
withDefault default_ m =
  case m of
    Just v -> v
    Nothing -> default_


justs :: [Maybe a] -> [a]
justs xs = [ x | Just x <- xs ]


{- Debugging
-}


indent :: Int -> String -> String
indent i str =
    List.replicate i ' ' ++ str

logList :: String -> [String] -> IO ()
logList label strs =
  Ext.Common.log label
    (formatList strs)

formatList :: [String] -> String
formatList strs =
  List.foldr (\tail gathered ->  gathered ++ indent 4 tail ++ "\n") "\n" strs


log :: String -> String -> IO ()
log label str = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> atomicPutStrLn $ label ++ ": " ++ str ++ "\n"
    Nothing -> pure ()


debug :: String -> IO ()
debug str = do
  debugM <- Env.lookupEnv "LDEBUG"
  case debugM of
    Just _ -> atomicPutStrLn $ "DEBUG: " ++ str
    Nothing -> pure ()


withDebug :: IO a -> IO ()
withDebug io = do
  Env.setEnv "LDEBUG" "1"
  io
  Env.unsetEnv "LDEBUG"


{-# NOINLINE printLock #-}
printLock :: MVar ()
printLock = unsafePerformIO $ newMVar ()


{- Print debugging in a concurrent setting can be painful sometimes due to output
becoming interpolated. This `putStrLn` alternative uses an MVar to ensure all
printouts are atomic and un-garbled.
-}
atomicPutStrLn :: String -> IO ()
atomicPutStrLn str =
  withMVar printLock (\_ -> hPutStr stdout (str <> "\n") >> hFlush stdout)


{- Wrap an IO in basic runtime information
   Note: this is a very naive implementation and may not always work right,
   i.e. if the IO value is not fully evaluated
-}
-- track :: _ -> IO a -> IO a
track label io = do
  m <- getTime Monotonic
  p <- getTime ProcessCPUTime
  t <- getTime ThreadCPUTime
  res <- io
  m_ <- getTime Monotonic
  p_ <- getTime ProcessCPUTime
  t_ <- getTime ThreadCPUTime
  atomicPutStrLn $ T.unpack $
    sformat ("⏱  " % label % ": " % timeSpecs % " " % timeSpecs % " " % timeSpecs % "\n") m m_ p p_ t t_
  pure res


-- track_ label io = do
--   m <- getTime Monotonic
--   p <- getTime ProcessCPUTime
--   t <- getTime ThreadCPUTime
--   res <- io
--   m_ <- getTime Monotonic
--   p_ <- getTime ProcessCPUTime
--   t_ <- getTime ThreadCPUTime
--   let result :: T.Text = sformat ("⏱  " % label % ": " % timeSpecs % " " % timeSpecs % " " % timeSpecs % "\n") m m_ p p_ t t_
--   pure $
--     ( result
--     , res
--     )

track__ label io = do
  m <- getTime Monotonic
  p <- getTime ProcessCPUTime
  t <- getTime ThreadCPUTime
  res <- io
  m_ <- getTime Monotonic
  p_ <- getTime ProcessCPUTime
  t_ <- getTime ThreadCPUTime
  let result :: T.Text = sformat (timeSpecs) m m_
      -- millisM :: Maybe Double = result & T.splitOn " " & Prelude.head & T.unpack & readMaybe
      millisM :: Maybe Double =
        case result & T.splitOn " " of
          v:"ms":_ -> v & T.unpack & readMaybe
          v:"us":_ -> v & T.unpack & readMaybe & fmap (/ 1000)
          _      -> error $ "woah there! unhandled track__ result: " <> T.unpack result

  case millisM of
    Just millis -> pure $ ( millis , label, res )
    _ -> error $ "impossible? couldn't get millis from '" <> T.unpack result <> "' on tracked label: " <> label


race ios = do
  results <- mapM (\(label, io) -> track__ label io ) ios
  -- resultsMvar <- traverse (\(label, io) -> fork $ track__ label io ) ios
  -- results <- traverse readMVar resultsMvar
  -- atomicPutStrLn $ "🏃 race results:\n" <> (fmap (T.pack . show . fst) results & T.intercalate "" & T.unpack)
  pure results


-- fork :: IO a -> IO (MVar a)
-- fork work =
--   do  mvar <- newEmptyMVar
--       _ <- forkIO $ putMVar mvar =<< work
--       return mvar



{- GHCI thread management

Developing threaded processes in GHCI can be rather tricky, as threads are directly
invoked from the main GHCI thread, so they don't die unless you kill GHCI and reload,
a rather slow process.

In order to get closer to the holy grail of "":r + kill + reload threads", useful
when working on and testing a daemon, the `trackedForkIO` function is a drop-in
replacement for `forkIO`, which paired with `killTrackedThreads` lets us cleanup
after a `:r` and avoid issues like a socket port already being in use!

-}

trackedForkIO :: IO () -> IO ()
trackedForkIO io = do
  threadId <- forkIO io
  trackGhciThread threadId


trackGhciThread :: ThreadId -> IO ()
trackGhciThread threadId =
  modifyMVar_ ghciThreads
    (\threads -> do
      -- debug $ "Tracking GHCI thread:" ++ show threadId
      pure $ threadId:threads
    )


killTrackedThreads :: IO ()
killTrackedThreads = do
  modifyMVar_ ghciThreads
    (\threads -> do
      case threads of
        [] -> do
          debug $ "No tracked GHCI threads to kill."
          pure []
        threads -> do
          debug $ "Killing tracked GHCI threads: " ++ show threads
          mapM killThread threads
          pure []
    )
  -- Reset our current directory back to the original ghci root, this allows
  -- our TH `$(bsToExp ...)` expressions with relative paths to recompile properly
  root <- readMVar ghciRoot
  debug $ "resetting ghci dir for rebuild: " <> show root
  Dir.setCurrentDirectory root
  threadDelay (10 * 1000) -- 10ms to settle as Dir.* stuff behaves oddly


{-# NOINLINE ghciThreads #-}
ghciThreads :: MVar [ThreadId]
ghciThreads = unsafePerformIO $ newMVar []


{-# NOINLINE ghciRoot #-}
ghciRoot :: MVar FilePath
ghciRoot = unsafePerformIO $ do
  dir <- Dir.getCurrentDirectory
  newMVar dir

-- Inversion of `unless` that runs IO only when condition is True
onlyWhen :: Monad f => Bool -> f () -> f ()
onlyWhen condition io =
  unless (not condition) io


-- Re-exports

(&) = (Data.Function.&)
(</>) = (FP.</>)
withCurrentDirectory = Dir.withCurrentDirectory
