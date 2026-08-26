{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.Docs.Cache
  ( CachedModule(..)
  , Manifest(..)
  , ManifestEntry(..)
  , cacheDirectory
  , modulesDirectory
  , modulePath
  , manifestPath
  , fingerprintBytes
  , writeModule
  , updateManifest
  , readModule
  , readManifest
  , matchesInputs
  , matchesBuild
  ) where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import qualified Elm.Docs as Docs
import qualified Elm.ModuleName as ModuleName
import qualified Data.Name as Name
import qualified Elm.Version as ElmVersion
import qualified ElmDevVersion
import qualified Json.Decode as Decode
import qualified Json.Encode as Encode
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified System.FileLock as FileLock

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

data Manifest = Manifest
  { manifestProjectConfigHash :: String
  , manifestResolvedPackages :: [(String, String)]
  , manifestEntries :: [ManifestEntry]
  }
  deriving (Eq, Show)

data ManifestEntry = ManifestEntry
  { manifestModuleName :: String
  , manifestSourcePath :: FilePath
  , manifestModuleFile :: FilePath
  , manifestModuleHash :: String
  }
  deriving (Eq, Show)

instance Aeson.ToJSON ManifestEntry where
  toJSON entry =
    Aeson.object
      [ "module" .= manifestModuleName entry
      , "source" .= manifestSourcePath entry
      , "file" .= manifestModuleFile entry
      , "hash" .= manifestModuleHash entry
      ]

instance Aeson.FromJSON ManifestEntry where
  parseJSON = Aeson.withObject "docs cache manifest entry" $ \object ->
    ManifestEntry
      <$> object .: "module"
      <*> object .: "source"
      <*> object .: "file"
      <*> object .: "hash"

cacheDirectory :: FilePath -> FilePath
cacheDirectory root =
  root FilePath.</> "elm-stuff" FilePath.</> ElmVersion.toChars ElmVersion.compiler FilePath.</> "elm-dev-docs"

modulesDirectory :: FilePath -> FilePath
modulesDirectory root =
  cacheDirectory root FilePath.</> "modules"

modulePath :: FilePath -> ModuleName.Raw -> FilePath
modulePath root moduleName =
  modulesDirectory root FilePath.</> ModuleName.toHyphenPath moduleName FilePath.<.> "json"

manifestPath :: FilePath -> FilePath
manifestPath root =
  cacheDirectory root FilePath.</> "manifest.json"

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

updateManifest :: FilePath -> String -> [(String, String)] -> [(FilePath, Docs.Module)] -> IO Bool
updateManifest root expectedConfigHash resolvedPackages modules =
  tryWrite $
    MVar.withMVar manifestWriteLock $ \_ -> do
      Directory.createDirectoryIfMissing True (cacheDirectory root)
      FileLock.withFileLock (cacheDirectory root FilePath.</> "manifest.lock") FileLock.Exclusive $ \_ -> do
        configBytes <- BS.readFile (root FilePath.</> "elm.json")
        if hashStrict configBytes /= expectedConfigHash
          then ioError (userError "elm.json changed while compiling docs")
          else pure ()
        existing <- readManifest root
        let allPreviousEntries = maybe [] manifestEntries existing
            previousEntries =
              case existing of
                Just manifest
                  | manifestProjectConfigHash manifest == expectedConfigHash
                      && manifestResolvedPackages manifest == resolvedPackages -> manifestEntries manifest
                _ -> []
        existingEntries <- filterM entryStillExists previousEntries
        additions <- mapMaybeM toPersistedEntry modules
        let previous = Map.fromList (map (\entry -> (manifestModuleName entry, entry)) existingEntries)
            latest = Map.fromList (map (\entry -> (manifestModuleName entry, entry)) additions)
            entries = Map.elems (Map.union latest previous)
            value =
              Aeson.object
                [ "schemaVersion" .= cacheSchemaVersion
                , "projectConfigHash" .= expectedConfigHash
                , "resolvedPackages" .= resolvedPackages
                , "modules" .= entries
                ]
        writeAtomically (manifestPath root) (LBS.toStrict (Aeson.encode value))
        removeDroppedModuleFiles allPreviousEntries entries
  where
    toEntry sourcePath moduleName =
      let name = ModuleName.toChars moduleName
      in ManifestEntry name (relativeSourcePath root sourcePath) (moduleRelativePath moduleName) ""

    toPersistedEntry (sourcePath, Docs.Module moduleName _ _ _ _ _) = do
      maybeCached <- readModule root moduleName
      maybeSource <- readBytes (absoluteSourcePath root sourcePath)
      maybeModuleBytes <- readBytes (modulePath root moduleName)
      pure $ do
        cached <- maybeCached
        source <- maybeSource
        moduleBytes <- maybeModuleBytes
        let entry = toEntry sourcePath moduleName
        if cachedSourcePath cached == manifestSourcePath entry
            && cachedSourceHash cached == hashStrict source
          then Just entry { manifestModuleHash = hashStrict moduleBytes }
          else Nothing

    entryStillExists entry = do
      case validatedModuleName entry of
        Nothing -> pure False
        Just moduleName -> do
          maybeCached <- readModule root moduleName
          maybeSource <- readBytes (absoluteSourcePath root (manifestSourcePath entry))
          maybeModuleBytes <- readBytes (modulePath root moduleName)
          pure $ case (maybeCached, maybeSource, maybeModuleBytes) of
            (Just cached, Just source, Just moduleBytes) ->
              cachedSourcePath cached == manifestSourcePath entry
                && cachedSourceHash cached == hashStrict source
                && manifestModuleHash entry == hashStrict moduleBytes
            _ -> False

    removeDroppedModuleFiles previous current = do
      let currentNames = Map.fromList (map (\entry -> (manifestModuleName entry, ())) current)
          dropped = filter (\entry -> Map.notMember (manifestModuleName entry) currentNames) previous
      mapM_
        (\entry -> maybe (pure ()) (removeIfExists . modulePath root) (validatedModuleName entry))
        dropped

readModule :: FilePath -> ModuleName.Raw -> IO (Maybe CachedModule)
readModule root expectedName = do
  maybeBytes <- readBytes (modulePath root expectedName)
  pure $ do
    bytes <- maybeBytes
    value <- either (const Nothing) Just (Aeson.eitherDecodeStrict' bytes)
    either (const Nothing) Just (Aeson.parseEither (parseCachedModule expectedName) value)

readManifest :: FilePath -> IO (Maybe Manifest)
readManifest root = do
  maybeBytes <- readBytes (manifestPath root)
  pure $ case maybeBytes >>= either (const Nothing) Just . Aeson.eitherDecodeStrict' of
    Just value ->
      either (const Nothing) Just (Aeson.parseEither parseManifest value)
    Nothing -> Nothing

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

parseManifest :: Aeson.Value -> Aeson.Parser Manifest
parseManifest =
  Aeson.withObject "docs cache manifest" $ \object -> do
    schema <- object .: "schemaVersion"
    if schema == cacheSchemaVersion
      then Manifest <$> object .: "projectConfigHash" <*> object .: "resolvedPackages" <*> object .: "modules"
      else fail "stale docs cache manifest"

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

absoluteSourcePath :: FilePath -> FilePath -> FilePath
absoluteSourcePath root sourcePath =
  if FilePath.isAbsolute sourcePath
    then sourcePath
    else root FilePath.</> sourcePath

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

filterM :: (a -> IO Bool) -> [a] -> IO [a]
filterM predicate values = do
  decisions <- mapM predicate values
  pure [value | (value, True) <- zip values decisions]

mapMaybeM :: (a -> IO (Maybe b)) -> [a] -> IO [b]
mapMaybeM callback values = do
  results <- mapM callback values
  pure [value | Just value <- results]

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- Directory.doesFileExist path
  if exists then Directory.removeFile path else pure ()

moduleRelativePath :: ModuleName.Raw -> FilePath
moduleRelativePath moduleName =
  "modules" FilePath.</> ModuleName.toHyphenPath moduleName FilePath.<.> "json"

validatedModuleName :: ManifestEntry -> Maybe ModuleName.Raw
validatedModuleName entry = do
  let raw = manifestModuleName entry
  if validModuleName raw
    then do
      let moduleName = Name.fromChars raw
      if manifestModuleFile entry == moduleRelativePath moduleName
        then Just moduleName
        else Nothing
    else Nothing

validModuleName :: String -> Bool
validModuleName raw =
  case splitOnDot raw of
    [] -> False
    segments -> all validSegment segments
  where
    validSegment segment =
      case segment of
        first : rest -> Char.isAsciiUpper first && all (\char -> Char.isAscii char && (Char.isAlphaNum char || char == '_')) rest
        [] -> False

splitOnDot :: String -> [String]
splitOnDot raw =
  case break (== '.') raw of
    (segment, []) -> [segment]
    (segment, _ : rest) -> segment : splitOnDot rest

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

{-# NOINLINE manifestWriteLock #-}
manifestWriteLock :: MVar.MVar ()
manifestWriteLock = Unsafe.unsafePerformIO (MVar.newMVar ())
