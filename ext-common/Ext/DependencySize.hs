module Ext.DependencySize
  ( Report(..)
  , Bundle(..)
  , ModuleSize(..)
  , Error(..)
  , analyze
  , render
  , matchesFilter
  ) where

import qualified Codec.Compression.GZip as GZip
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Ext.CompileHelpers.Generic as Compile
import qualified Ext.CompileProxy as CompileProxy
import qualified Ext.Optimization.Level
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir
import qualified System.Exit as SystemExit
import qualified System.FilePath as FP
import qualified System.IO.Temp as Temp
import qualified System.Process as Process
import qualified Watchtower.Live.Client as Client


data Bundle = Bundle
  { developmentBytes :: Int
  , productionOptimizedBytes :: Maybe Int
  , productionMinifiedBytes :: Maybe Int
  , productionGzipBytes :: Maybe Int
  , productionUnavailable :: Maybe String
  , minificationUnavailable :: Maybe String
  }
  deriving (Show)


data ModuleSize = ModuleSize
  { moduleName :: String
  , packageName :: String
  , moduleBytes :: Int
  }
  deriving (Show)


data Report = Report
  { entrypoint :: FilePath
  , bundle :: Bundle
  , modules :: [ModuleSize]
  , elmLanguageBytes :: Int
  , filters :: [String]
  }
  deriving (Show)


data Error = DevelopmentCompileFailed Exit.Reactor
  deriving (Show)


analyze :: FilePath -> FilePath -> [String] -> Maybe (STM.TVar (Map.Map Pkg.Name Client.PackageInfo)) -> IO (Either Error Report)
analyze root requestedEntrypoint requestedFilters packagesVar = do
  let cleanFilters = List.nub (filter (not . null) (map trim requestedFilters))
      compilePaths = NE.List requestedEntrypoint []
  developmentResult <- CompileProxy.compileDevelopmentSizes root compilePaths packagesVar
  case developmentResult of
    Left problem -> pure (Left (DevelopmentCompileFailed problem))
    Right development -> do
      productionResult <- CompileProxy.compile root compilePaths productionFlags packagesVar
      bundleResult <- buildBundle development productionResult
      let allModules = sortModules (map toModuleSize (Map.toList (Compile.developmentModuleBytes development)))
          visibleModules =
            if null cleanFilters
              then allModules
              else filter (\item -> any (`matchesFilter` moduleName item) cleanFilters) allModules
      pure $ Right Report
        { entrypoint = requestedEntrypoint
        , bundle = bundleResult
        , modules = visibleModules
        , elmLanguageBytes = Compile.developmentElmLanguageBytes development
        , filters = cleanFilters
        }


productionFlags :: Compile.Flags
productionFlags =
  Compile.Flags (Compile.Prod Ext.Optimization.Level.O0) (Compile.OutputTo Compile.Js) Compile.DebuggerNone


buildBundle :: Compile.DevelopmentSizes -> (Either Exit.Reactor Compile.CompilationResult, Map.Map FilePath Client.FileInfo) -> IO Bundle
buildBundle development (productionResult, _) =
  case productionResult of
    Left problem ->
      pure Bundle
        { developmentBytes = builderBytes (Compile.developmentBuilder development)
        , productionOptimizedBytes = Nothing
        , productionMinifiedBytes = Nothing
        , productionGzipBytes = Nothing
        , productionUnavailable = Just (productionFailure problem)
        , minificationUnavailable = Nothing
        }
    Right result ->
      case result of
        Compile.CompiledJs productionBuilder -> do
          let productionJs = Builder.toLazyByteString productionBuilder
          minified <- minify productionJs
          pure Bundle
            { developmentBytes = builderBytes (Compile.developmentBuilder development)
            , productionOptimizedBytes = Just (fromIntegral (Lazy.length productionJs))
            , productionMinifiedBytes = either (const Nothing) (Just . fromIntegral . Lazy.length) minified
            , productionGzipBytes = either (const Nothing) (Just . fromIntegral . Lazy.length . GZip.compress) minified
            , productionUnavailable = Nothing
            , minificationUnavailable = either Just (const Nothing) minified
            }
        _ ->
          pure Bundle
            { developmentBytes = builderBytes (Compile.developmentBuilder development)
            , productionOptimizedBytes = Nothing
            , productionMinifiedBytes = Nothing
            , productionGzipBytes = Nothing
            , productionUnavailable = Just "production compilation did not produce JavaScript"
            , minificationUnavailable = Nothing
            }


productionFailure :: Exit.Reactor -> String
productionFailure problem =
  case problem of
    Exit.ReactorBadGenerate (Exit.GenerateCannotOptimizeDebugValues first rest) ->
      "reachable Debug calls exist in " ++ List.intercalate ", " (map ModuleName.toChars (first : rest))
    _ -> Exit.toString (Exit.reactorToReport problem)


minify :: Lazy.ByteString -> IO (Either String Lazy.ByteString)
minify source = do
  executable <- Dir.findExecutable "uglifyjs"
  case executable of
    Nothing -> pure (Left "missing uglifyjs")
    Just uglify ->
      Exception.handle
        (\problem -> pure (Left (show (problem :: Exception.IOException)))) $
        Temp.withSystemTempDirectory "elm-dev-size" $ \dir -> do
          let input = dir FP.</> "input.js"
              compressed = dir FP.</> "compressed.js"
              output = dir FP.</> "output.js"
              compression = "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe"
          Lazy.writeFile input source
          first <- Process.readCreateProcessWithExitCode (Process.proc uglify [input, "--compress", compression, "--output", compressed]) ""
          case first of
            (SystemExit.ExitFailure _, _, stderrText) -> pure (Left (singleLine stderrText))
            (SystemExit.ExitSuccess, _, _) -> do
              second <- Process.readCreateProcessWithExitCode (Process.proc uglify [compressed, "--mangle", "--output", output]) ""
              case second of
                (SystemExit.ExitFailure _, _, stderrText) -> pure (Left (singleLine stderrText))
                (SystemExit.ExitSuccess, _, _) -> Right . Lazy.fromStrict <$> Strict.readFile output


singleLine :: String -> String
singleLine = unwords . words


toModuleSize :: (ModuleName.Canonical, Int) -> ModuleSize
toModuleSize (ModuleName.Canonical packageNameValue moduleNameValue, bytes) =
  ModuleSize
    { moduleName = Name.toChars moduleNameValue
    , packageName = if packageNameValue == Pkg.dummyName then "application" else Pkg.toChars packageNameValue
    , moduleBytes = bytes
    }


sortModules :: [ModuleSize] -> [ModuleSize]
sortModules = List.sortBy $ \one two ->
  case compare (moduleBytes two) (moduleBytes one) of
    EQ -> compare (moduleName one) (moduleName two)
    ordering -> ordering


matchesFilter :: String -> String -> Bool
matchesFilter namespace name =
  name == namespace || (namespace ++ ".") `List.isPrefixOf` name


render :: Report -> [String]
render report =
  bundleLines (bundle report)
    ++ packageLines report
    ++ filterTotalLines report
    ++ moduleLines report


bundleLines :: Bundle -> [String]
bundleLines metrics =
  [ "Bundle"
  , metricLine "Development JS" (Just (developmentBytes metrics)) Nothing
  , metricLine "Production optimized JS" (productionOptimizedBytes metrics) (productionUnavailable metrics)
  , metricLine "Production minified JS" (productionMinifiedBytes metrics) (productionUnavailable metrics `orElse` minificationUnavailable metrics)
  , metricLine "Production minified + gzip" (productionGzipBytes metrics) (productionUnavailable metrics `orElse` minificationUnavailable metrics)
  ]


packageLines :: Report -> [String]
packageLines report =
  if null (filters report)
    then
      let packageBytes = Map.fromListWith (+) [(packageName item, moduleBytes item) | item <- modules report]
          packages = List.sortBy (\(_, one) (_, two) -> compare two one) (Map.toList packageBytes)
      in ["", "Packages (development JS)"]
          ++ map (uncurry metricLineValue) packages
          ++ [metricLineValue "Elm language" (elmLanguageBytes report)]
    else []


filterTotalLines :: Report -> [String]
filterTotalLines report =
  case filters report of
    [] -> []
    namespaces ->
      "" : map
        (\namespace -> metricLineValue (namespace ++ " total (development JS)") (sum [moduleBytes item | item <- modules report, matchesFilter namespace (moduleName item)]))
        namespaces


moduleLines :: Report -> [String]
moduleLines report =
  let visible = modules report
      moduleWidth = maximum (6 : map (length . moduleName) visible)
      packageWidth = maximum (7 : map (length . packageName) visible)
      row name package bytes = padRight moduleWidth name ++ "  " ++ padRight packageWidth package ++ "  " ++ formatBytes bytes
      header = padRight moduleWidth "Module" ++ "  " ++ padRight packageWidth "Package" ++ "  Development JS"
  in ["", "Modules (development JS)", header]
      ++ map (\item -> row (moduleName item) (packageName item) (moduleBytes item)) visible
      ++ if null (filters report) then [row "Elm language" "" (elmLanguageBytes report)] else []


metricLine :: String -> Maybe Int -> Maybe String -> String
metricLine label maybeBytes maybeReason =
  pad label ++ case maybeBytes of
    Just bytes -> formatBytes bytes
    Nothing -> "unavailable" ++ maybe "" (\reason -> " (" ++ reason ++ ")") maybeReason


metricLineValue :: String -> Int -> String
metricLineValue label bytes = pad label ++ formatBytes bytes


pad :: String -> String
pad label = label ++ replicate (max 2 (34 - length label)) ' '


padRight :: Int -> String -> String
padRight width value = value ++ replicate (width - length value) ' '


formatBytes :: Int -> String
formatBytes bytes
  | bytes < 1024 = show bytes ++ " B"
  | bytes < 1024 * 1024 = showOne (fromIntegral bytes / 1024) ++ " KB"
  | otherwise = showOne (fromIntegral bytes / (1024 * 1024)) ++ " MB"


showOne :: Double -> String
showOne value =
  let rounded = fromIntegral (round (value * 10) :: Int) / 10 :: Double
  in show rounded


builderBytes :: Builder.Builder -> Int
builderBytes = fromIntegral . Lazy.length . Builder.toLazyByteString


trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')


orElse :: Maybe a -> Maybe a -> Maybe a
orElse first second =
  case first of
    Just _ -> first
    Nothing -> second
