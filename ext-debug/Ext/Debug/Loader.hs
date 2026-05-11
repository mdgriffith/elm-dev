{-# LANGUAGE TemplateHaskell #-}
module Ext.Debug.Loader
  ( runtimeJs
  , debuggerElm
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import System.FilePath ((</>))


runtimeJsTemplate :: BS.ByteString
runtimeJsTemplate = $( do
  let path = "ext-debug" </> "runtime" </> "elm-dev-debugger.js"
  TH.addDependentFile path
  FileEmbed.bsToExp =<< TH.runIO (BS.readFile path)
  )


debuggerAppJsTemplate :: BS.ByteString
debuggerAppJsTemplate = $( do
  let path = "ext-debug" </> "app" </> "compiled" </> "elm-dev-debugger-app.js"
  TH.addDependentFile path
  FileEmbed.bsToExp =<< TH.runIO (BS.readFile path)
  )


debuggerAppJs :: BS.ByteString
debuggerAppJs =
  T.encodeUtf8 $
    T.replace
      (T.pack "}(this));")
      (T.pack "}(typeof window !== 'undefined' ? window : this));")
      (T.decodeUtf8 debuggerAppJsTemplate)


debuggerElm :: BS.ByteString
debuggerElm = $( do
  let path = "ext-debug" </> "app" </> "src" </> "ElmDev" </> "Debugger.elm"
  TH.addDependentFile path
  FileEmbed.bsToExp =<< TH.runIO (BS.readFile path)
  )


runtimeJs :: B.Builder
runtimeJs =
  B.byteString debuggerAppJs <> B.byteString runtimeJsTemplate
