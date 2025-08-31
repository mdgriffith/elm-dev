{-# LANGUAGE TemplateHaskell #-}
module Ext.Test.Templates.Loader
  ( runnerElm
  , runnerJs
  , nodeRunnerJs
  , exampleElm
  ) where

import qualified Data.ByteString as BS
import qualified Language.Haskell.TH as TH
import qualified Data.FileEmbed as FileEmbed
import System.FilePath ((</>))


runnerElm :: BS.ByteString
runnerElm = $( FileEmbed.bsToExp =<< TH.runIO (BS.readFile ("ext-common" </> "Ext" </> "Test" </> "templates" </> "Runner.elm")) )


runnerJs :: BS.ByteString
runnerJs = $( FileEmbed.bsToExp =<< TH.runIO (BS.readFile ("ext-common" </> "Ext" </> "Test" </> "templates" </> "runner.js")) )



nodeRunnerJs :: BS.ByteString
nodeRunnerJs = $( FileEmbed.bsToExp =<< TH.runIO (BS.readFile ("ext-common" </> "Ext" </> "Test" </> "templates" </> "node-runner.js")) )



exampleElm :: BS.ByteString
exampleElm = $( FileEmbed.bsToExp =<< TH.runIO (BS.readFile ("ext-common" </> "Ext" </> "Test" </> "templates" </> "Example.elm")) )


version :: Int
version = 15