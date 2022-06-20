{-# LANGUAGE OverloadedStrings #-}

module Ext.Filewatch where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.FoldDebounce as Debounce
import Control.Monad (forever)
import qualified Data.List as List
import Ext.Common
import qualified System.FSNotify

watch :: FilePath -> ([FilePath] -> IO ()) -> IO ()
watch root action =
  trackedForkIO $
     System.FSNotify.withManager $ \mgr -> do
      trigger <-
        Debounce.new
          Debounce.Args
            { Debounce.cb = (\events -> action events),
              Debounce.fold = (\l v -> List.nub $ v : l),
              Debounce.init = []
            }
          Debounce.def
            { Debounce.delay = 10000, -- 10ms
              Debounce.alwaysResetTimer = True
            }

      -- start a watching job (in the background)
      System.FSNotify.watchTree
        mgr -- manager
        root -- directory to watch
        shouldTrigger -- predicate
        (\event ->
            Debounce.send trigger (getEventFilePath event)
        )

      -- sleep forever (until interrupted)
      forever $ threadDelay 1000000


getEventFilePath :: System.FSNotify.Event -> FilePath
getEventFilePath event =
  case event of
    System.FSNotify.Added filepath _ _ -> filepath
    System.FSNotify.Modified filepath _ _ -> filepath
    System.FSNotify.Removed filepath _ _ -> filepath
    System.FSNotify.Unknown filepath _ _ -> filepath


shouldTrigger :: System.FSNotify.Event -> Bool
shouldTrigger e =
    let 
        path = getEventFilePath event
    in
    not (List.isInfixOf ".git" path)
        && not (List.isInfixOf "elm-stuff" path)
        && not (List.isInfixOf "node_modules" path)