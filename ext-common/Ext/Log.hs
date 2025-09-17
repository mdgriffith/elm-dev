{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.Log 
    ( Flag(..)
    , isActive
    , ifActive
    , Ext.Log.log
    , TempChannel(..)
    , logTemp
    , logTempBytes
    , with
    , withAllBut
    , withPrintLockIf
    , formatList
    )
    where

import Control.Concurrent
import Control.Concurrent.MVar

import qualified System.Environment as Env
import qualified Control.Monad as Monad
import qualified Data.List as List
import System.IO.Unsafe as Unsafe (unsafePerformIO)
import qualified System.IO as IO
import qualified Data.ByteString as BS

{-|

    Performance - Performance information related to compiling

    Live - Messages sent back and forth through the Watchtower websocket.

    VerboseServer - Tell the snap server to be verbose

-}
data Flag 
    = WithLabels
    | Performance
    | PerformanceTiming
    | Live
    | Questions
    | Test
    | VerboseServer
    | MemoryCache
    | FileProxy
    | FileWatch
    | ElmCompilerInfo
    | ElmCompilerError
    | Misc
    | LSP
    | Deps
    deriving (Eq)


all :: [ Flag ]
all =
    [ WithLabels
    , Performance
    , PerformanceTiming
    , Live
    , Questions
    , Test
    , VerboseServer
    , FileProxy
    , FileWatch
    , ElmCompilerInfo
    , ElmCompilerError
    , MemoryCache
    , Misc
    , LSP

    , Deps
    ]


toString :: Flag -> String
toString flag =
    case flag of 
        WithLabels ->
            "ElmDevWithLabels"

        Performance -> 
            "ElmDevPerformance"

        PerformanceTiming ->
            "ElmDevPerformanceTiming"

        Live -> 
            "ElmDevLive"

        Questions ->
            "ElmDevQuestions"

        Test ->
            "ElmDevTest"

        VerboseServer ->
            "ElmDevVerboseServer"

        FileProxy ->
            "ElmDevFileProxy"

        FileWatch ->
            "ElmDevFileWatch"

        ElmCompilerInfo ->
            "ElmDevElmCompilerInfo"

        ElmCompilerError ->
            "ElmDevElmCompilerError"

        Misc ->
            "ElmDevMisc"

        MemoryCache ->
            "ElmDevMemoryCache"

        LSP ->
            "ElmDevLSP"

        Deps ->
            "ElmDevDeps"

toLabel :: Flag -> String
toLabel flag =
    case flag of 
        WithLabels ->
            ""

        Performance -> 
            "🚀"

        PerformanceTiming ->
            "⏱️"
        
        Live -> 
            "🛰️"

        Questions ->
            "❓"

        Test ->
            "🧪"

        VerboseServer ->
            "💁"
        
        FileProxy ->
            "📁"

        FileWatch ->
            "👁️"

        ElmCompilerInfo ->
            ""
        
        ElmCompilerError ->
            ""

        Misc ->
            "⭐"

        MemoryCache ->
            "🧠"

        LSP ->
            "🔌"

        Deps ->
            "🔍"

{-# NOINLINE envLock #-}
envLock :: MVar ()
envLock = Unsafe.unsafePerformIO $ newMVar ()

{-# NOINLINE printLock #-}
printLock :: MVar ()
printLock = Unsafe.unsafePerformIO $ newMVar ()

with :: [ Flag ] -> IO a ->  IO a
with flags io = do
  -- Use tryTakeMVar to avoid blocking if we can't get the lock immediately
  maybeLock <- tryTakeMVar envLock
  case maybeLock of
    Nothing -> do
      -- If we can't get the lock, just run the IO without setting flags
      io
    Just _ -> do
      -- We got the lock, set flags and run IO
      Monad.mapM_ (\flag -> Env.setEnv (toString flag) "1") flags
      result <- io
      Monad.mapM_ (\flag -> Env.unsetEnv (toString flag)) flags
      putMVar envLock () -- Release the lock
      return result

withAllBut :: [ Flag ] -> IO a ->  IO a
withAllBut but io = do
  let flags = List.filter (\flag -> not $ List.elem flag but) Ext.Log.all
  with flags io


data PrintMode = StdOut | TempFile

mode :: PrintMode
mode = StdOut

log :: Flag -> String -> IO ()
log flag message = do
  debugM <- Env.lookupEnv (toString flag)
  case debugM of
    Just _ -> do 
        withLabels <- isActive WithLabels
        -- Use tryTakeMVar for printLock as well
        maybePrintLock <- tryTakeMVar printLock
        case maybePrintLock of
          Nothing -> pure () -- Skip printing if we can't get the lock
          Just _ -> do
            case mode of
              StdOut -> do
                IO.hPutStr IO.stdout ((if withLabels then toLabel flag ++ ": " else "") ++ message ++ "\n")
                IO.hFlush IO.stdout
              TempFile -> do
                appendFile "/tmp/lsp-debug.log" ((if withLabels then toLabel flag ++ ": " else "") ++ message ++ "\n")
            putMVar printLock () -- Release the lock
    Nothing -> pure ()


-- Always-on temp-file logging for binary-safe streams (e.g. LSP/MCP proxy)
data TempChannel = TempLSP | TempMCP deriving (Eq)

tempChannelPath :: TempChannel -> FilePath
tempChannelPath chan =
  case chan of
    TempLSP -> "/tmp/elm-dev-lsp.log"
    TempMCP -> "/tmp/elm-dev-mcp.log"

-- String helper
logTemp :: TempChannel -> String -> IO ()
logTemp chan msg =
  withMVar printLock (\_ -> do
    handle <- IO.openFile (tempChannelPath chan) IO.AppendMode
    IO.hSetBuffering handle IO.NoBuffering
    IO.hPutStr handle msg
    IO.hPutStr handle "\n"
    IO.hFlush handle
    IO.hClose handle
  )

-- ByteString (binary-safe) helper
logTempBytes :: TempChannel -> BS.ByteString -> IO ()
logTempBytes chan bytes =
  withMVar printLock (\_ -> do
    handle <- IO.openFile (tempChannelPath chan) IO.AppendMode
    IO.hSetBinaryMode handle True
    IO.hSetBuffering handle IO.NoBuffering
    BS.hPut handle bytes
    IO.hPutStr handle "\n"
    IO.hFlush handle
    IO.hClose handle
  )



logList :: Flag -> String -> IO ()
logList flag message = do
  debugM <- Env.lookupEnv (toString flag)
  case debugM of
    Just _ -> do 
        withLabels <- isActive WithLabels
        atomicPutStrLn $ (if withLabels then (toLabel flag) ++ ": " else "") ++ message ++ "\n"
    Nothing -> pure ()



isActive :: Flag -> IO Bool
isActive flag = do
  found <- Env.lookupEnv (toString flag)
  case found of
    Just _ -> pure True
    Nothing -> pure False

ifActive :: Flag -> (() -> IO ()) -> IO ()
ifActive flag io = do
  flagIsActive <- isActive flag
  
  if flagIsActive then
    io ()
  else
    pure ()


withPrintLockIf :: Flag -> IO.Handle -> (() -> IO ()) -> IO ()
withPrintLockIf flag handle fn = do 
  flag <- isActive flag
  if flag then 
    withMVar printLock (\_ -> fn () >> IO.hPutStr handle "\n" >> IO.hFlush handle)
  else
    pure ()


{- Print debugging in a concurrent setting can be painful sometimes due to output
becoming interpolated. This `putStrLn` alternative uses an MVar to ensure all
printouts are atomic and un-garbled.
-}
atomicPutStrLn :: String -> IO ()
atomicPutStrLn str =
  withMVar printLock (\_ -> IO.hPutStr IO.stdout (str <> "\n") >> IO.hFlush IO.stdout)


formatList :: [String] -> String
formatList =
  List.foldr (\tail gathered -> gathered ++ indent 4 tail ++ "\n") "\n"


indent :: Int -> String -> String
indent i str =
  List.replicate i ' ' ++ str