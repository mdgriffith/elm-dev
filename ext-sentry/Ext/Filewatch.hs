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
        (const True) -- predicate
        ( \e -> do
            let f = case e of
                  System.FSNotify.Added f _ _ -> f
                  System.FSNotify.Modified f _ _ -> f
                  System.FSNotify.Removed f _ _ -> f
                  System.FSNotify.Unknown f _ _ -> f

                shouldRefresh =
                  do
                    not (List.isInfixOf ".git" f)
                    && not (List.isInfixOf "elm-stuff" f)
                    && not (List.isInfixOf "node_modules" f)

            if shouldRefresh
              then do
                Debounce.send trigger f
              else pure ()
        )

      -- sleep forever (until interrupted)
      forever $ threadDelay 1000000
