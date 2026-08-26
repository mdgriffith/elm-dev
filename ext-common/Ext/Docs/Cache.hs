{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.Docs.Cache
  ( CachedModule(..)
  , cacheDirectory
  , modulesDirectory
  , modulePath
  , fingerprintBytes
  , writeModule
  , readModule
  , matchesInputs
  , matchesBuild
  ) where

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import qualified Elm.Docs as Docs
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Version as ElmVersion
import qualified ElmDevVersion
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO

cacheSchemaVersion :: Int
cacheSchemaVersion = 2

extractorVersion :: Int
extractorVersion = 1

data CachedModule = CachedModule
  { cachedModuleName :: String
  , cachedSourcePath :: FilePath
  , cachedSourceHash :: String
  , cachedDependencies :: [String]
  , cachedUiSourceMap :: Bool
  , cachedBuildId :: Word64
  , cachedInterfaceHash :: String
  , cachedDocs :: Docs.Module
  }

cacheDirectory :: FilePath -> FilePath
cacheDirectory root =
  root FilePath.</> "elm-stuff" FilePath.</> ElmVersion.toChars ElmVersion.compiler FilePath.</> "elm-dev-docs"

modulesDirectory :: FilePath -> FilePath
modulesDirectory root =
  cacheDirectory root FilePath.</> "modules"

modulePath :: FilePath -> ModuleName.Raw -> FilePath
modulePath root moduleName =
  modulesDirectory root FilePath.</> ModuleName.toHyphenPath moduleName FilePath.<.> "json"

writeModule :: FilePath -> FilePath -> BS.ByteString -> [ModuleName.Raw] -> Bool -> Word64 -> String -> Docs.Module -> IO Bool
writeModule root sourcePath source dependencies uiSourceMap buildId interfaceHash docsModule@(Docs.Module moduleName _ _ _ _ _) =
  case docsJsonValue docsModule of
    Nothing -> pure False
    Just docsValue ->
      tryWrite $ do
        let value =
              Aeson.object
                [ "schemaVersion" .= cacheSchemaVersion
                , "extractorVersion" .= extractorVersion
                , "elmVersion" .= ElmVersion.toChars ElmVersion.compiler
                , "elmDevVersion" .= ElmDevVersion.version
                , "module" .= ModuleName.toChars moduleName
                , "source" .= relativeSourcePath root sourcePath
                , "sourceHash" .= hashStrict source
                 , "dependencies" .= map ModuleName.toChars dependencies
                 , "uiSourceMap" .= uiSourceMap
                 , "buildId" .= buildId
                 , "interfaceHash" .= interfaceHash
                 , "docs" .= docsValue
                ]
        writeAtomically (modulePath root moduleName) (LBS.toStrict (Aeson.encode value))

readModule :: FilePath -> ModuleName.Raw -> IO (Maybe CachedModule)
readModule root expectedName = do
  maybeBytes <- readBytes (modulePath root expectedName)
  pure $ do
    bytes <- maybeBytes
    value <- either (const Nothing) Just (Aeson.eitherDecodeStrict' bytes)
    either (const Nothing) Just (Aeson.parseEither (parseCachedModule expectedName) value)

matchesInputs :: CachedModule -> BS.ByteString -> [ModuleName.Raw] -> Bool -> Word64 -> String -> Bool
matchesInputs cached source dependencies uiSourceMap buildId interfaceHash =
  cachedSourceHash cached == hashStrict source
    && cachedDependencies cached == map ModuleName.toChars dependencies
    && cachedUiSourceMap cached == uiSourceMap
    && cachedBuildId cached == buildId
    && cachedInterfaceHash cached == interfaceHash

matchesBuild :: FilePath -> CachedModule -> FilePath -> BS.ByteString -> [ModuleName.Raw] -> Bool -> Word64 -> String -> Bool
matchesBuild root cached sourcePath source dependencies uiSourceMap buildId interfaceHash =
  cachedSourcePath cached == relativeSourcePath root sourcePath
    && matchesInputs cached source dependencies uiSourceMap buildId interfaceHash

parseCachedModule :: ModuleName.Raw -> Aeson.Value -> Aeson.Parser CachedModule
parseCachedModule expectedName =
  Aeson.withObject "cached Elm module docs" $ \object -> do
    schema <- object .: "schemaVersion"
    extractor <- object .: "extractorVersion"
    elmVersion <- object .: "elmVersion"
    elmDevVersion <- object .: "elmDevVersion"
    moduleName <- object .: "module"
    sourcePath <- object .: "source"
    sourceHash <- object .: "sourceHash"
    dependencies <- object .: "dependencies"
    uiSourceMap <- object .: "uiSourceMap"
    buildId <- object .: "buildId"
    interfaceHash <- object .: "interfaceHash"
    docsValue <- object .: "docs"
    if schema /= cacheSchemaVersion
        || extractor /= extractorVersion
        || elmVersion /= ElmVersion.toChars ElmVersion.compiler
        || elmDevVersion /= ElmDevVersion.version
        || moduleName /= ModuleName.toChars expectedName
      then fail "stale docs cache entry"
      else
        case decodeDocsModule expectedName docsValue of
          Nothing -> fail "invalid cached Elm docs"
          Just docsModule ->
            pure (CachedModule moduleName sourcePath sourceHash dependencies uiSourceMap buildId interfaceHash docsModule)

docsJsonValue :: Docs.Module -> Maybe Aeson.Value
docsJsonValue docsModule@(Docs.Module moduleName _ _ _ _ _) =
  let documentation = Map.singleton moduleName docsModule
      bytes = Builder.toLazyByteString (Encode.encodeUgly (Docs.encode documentation))
  in either (const Nothing) Just (Aeson.eitherDecode bytes)

decodeDocsModule :: ModuleName.Raw -> Aeson.Value -> Maybe Docs.Module
decodeDocsModule moduleName value = do
  documentation <- either (const Nothing) Just (Decode.fromByteString Docs.decoder (LBS.toStrict (Aeson.encode value)))
  Map.lookup moduleName documentation

relativeSourcePath :: FilePath -> FilePath -> FilePath
relativeSourcePath root sourcePath =
  FilePath.normalise $
    if FilePath.isAbsolute sourcePath
      then FilePath.makeRelative root sourcePath
      else sourcePath

hashStrict :: BS.ByteString -> String
hashStrict = hashLazy . LBS.fromStrict

fingerprintBytes :: BS.ByteString -> String
fingerprintBytes = hashStrict

hashLazy :: LBS.ByteString -> String
hashLazy = SHA.showDigest . SHA.sha256

tryWrite :: IO () -> IO Bool
tryWrite action = do
  result <- Exception.try action
  case result of
    Left (_ :: Exception.IOException) -> pure False
    Right () -> pure True

readBytes :: FilePath -> IO (Maybe BS.ByteString)
readBytes path = do
  result <- Exception.try (BS.readFile path)
  case result of
    Left (_ :: Exception.IOException) -> pure Nothing
    Right bytes -> pure (Just bytes)

writeAtomically :: FilePath -> BS.ByteString -> IO ()
writeAtomically path bytes = do
  let directory = FilePath.takeDirectory path
  Directory.createDirectoryIfMissing True directory
  (temporary, handle) <- IO.openBinaryTempFile directory (FilePath.takeFileName path ++ ".tmp")
  let cleanup = do
        Exception.catch (IO.hClose handle) (\(_ :: Exception.IOException) -> pure ())
        Exception.catch (Directory.removeFile temporary) (\(_ :: Exception.IOException) -> pure ())
  Exception.onException
    (do
      BS.hPut handle bytes
      IO.hFlush handle
      IO.hClose handle
      Directory.renameFile temporary path
    )
    cleanup
