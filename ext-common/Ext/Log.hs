{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Ext.Log 
    ( Flag(..)
    , isActive
    , ifActive
    , Ext.Log.log
    , with
    , withAllBut
    , withPrintLockIf
    )
    where

import Control.Concurrent
import Control.Concurrent.MVar

import qualified System.Environment as Env
import qualified Control.Monad as Monad
import qualified Data.List as List
import System.IO.Unsafe as Unsafe (unsafePerformIO)
import qualified System.IO as IO
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
    | VerboseServer
    | MemoryCache
    | FileProxy
    | FileWatch
    | ElmCompilerInfo
    | ElmCompilerError
    | Misc
    deriving (Eq)


all :: [ Flag ]
all =
    [ WithLabels
    , Performance
    , PerformanceTiming
    , Live
    , Questions
    , VerboseServer
    , FileProxy
    , FileWatch
    , ElmCompilerInfo
    , ElmCompilerError
    , MemoryCache
    , Misc
    ]


toString :: Flag -> String
toString flag =
    case flag of 
        Performance -> 
            "ElmDevPerformance"

        PerformanceTiming ->
            "ElmDevPerformanceTiming"
        
        Live -> 
            "ElmDevLive"

        Questions ->
            "ElmDevQuestions"

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


with :: [ Flag ] -> IO a ->  IO ()
with flags io = do
  Monad.mapM_ (\flag -> Env.setEnv (toString flag) "1") flags
  io
  Monad.mapM_ (\flag -> Env.unsetEnv (toString flag)) flags


withAllBut :: [ Flag ] -> IO a ->  IO ()
withAllBut but io = do
  let flags = List.filter (\flag -> not $ List.elem flag but) Ext.Log.all
  with flags io




log :: Flag -> String -> IO ()
log flag message = do
  debugM <- Env.lookupEnv (toString flag)
  case debugM of
    Just _ -> do 
        withLabels <- isActive WithLabels
        atomicPutStrLn $ (if withLabels then (toLabel flag) ++ ": " else "") ++ message 
    Nothing -> pure ()




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


{-# NOINLINE printLock #-}
printLock :: MVar ()
printLock = Unsafe.unsafePerformIO $ newMVar ()


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