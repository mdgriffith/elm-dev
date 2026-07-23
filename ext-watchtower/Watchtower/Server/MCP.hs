
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Watchtower.Server.MCP
  ( serve
  , availableToolNamesForTests
  , availableToolSchemasForTests
  , xmlBlockForTests
  , dependencyArgumentsValidForTests
  , dependencyOperationArgumentsValidForTests
  , dependencyPlanJsonForTests
  , dependencyTreeJsonForTests
  , legacyInstallResponseForTests
  ) where

{-
Can test via https://modelcontextprotocol.io/docs/tools/inspector
  
Run: `npx @modelcontextprotocol/inspector`.

In the app, select STDIO and put `elm-dev` with the arg `mcp` and then hit connect.

-}

import Control.Exception (SomeException)
import qualified Control.Exception as Exception
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.STM
import Data.Aeson ((.=))
import qualified Data.Name as Name
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toUpper, toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Scientific as Scientific
import qualified Elm.Package as Pkg
import qualified Elm.Docs as Docs
import qualified Ext.Common
import qualified Ext.CompileMode
import qualified Ext.CompileProxy
import qualified Ext.FileProxy
import qualified Ext.Dev.Project
import qualified Ext.Test.Install as TestInstall
import qualified Ext.Test.Result.Report as TestReport
import qualified System.CPUTime as CPUTime
import qualified Ext.Test.Runner as TestRunner
import qualified Gen.Javascript
import qualified Ext.Reporting.Error
import qualified Gen.Commands.Init as GenInit
import qualified Gen.Config as Config
import qualified Gen.Generate
import qualified Gen.Templates as Templates
import GHC.Generics
import qualified Stuff
import System.FilePath ((</>))
import qualified System.Process as Process
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import qualified Watchtower.Live as Live
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Server.JSONRPC as JSONRPC
import qualified Watchtower.Server.MCP.Protocol as MCP
import qualified Watchtower.Server.MCP.Uri as Uri
import qualified Watchtower.Server.MCP.ProjectLookup as ProjectLookup
import qualified Watchtower.Server.MCP.Docs as DocsRender
import qualified Data.Text as T
import qualified Watchtower.Server.MCP.Guides as Guides
import qualified Watchtower.State.Compile
import qualified Watchtower.State.Discover
import qualified Watchtower.State.TestJobs as TestJobs
import qualified Watchtower.State.Versions as Versions
import qualified Watchtower.Server.LSP as LSP
import qualified Watchtower.Server.LSP.Helpers as Helpers
import qualified Watchtower.Server.DevWS
import qualified Ext.Encode
import qualified Ext.Log
import qualified Elm.Details
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Exit as Exit
import qualified Reporting.Render.Code as RenderCode
import qualified Reporting.Error as Err
import qualified Reporting.Warning as Warning
import qualified Data.NonEmptyList as NE
import qualified Reporting.Exit.Help as Help
import qualified Elm.Outline as Elm.Outline
import qualified Elm.Licenses as Licenses
import qualified Elm.Version as V
import qualified Elm.Constraint as Con
import qualified Reporting.Annotation as Ann
import qualified Json.String as Json
import qualified Json.Encode as JE
import qualified Deps.Solver as Deps.Solver
import qualified Ext.Dev.Package
import qualified System.Directory as Dir
import qualified Data.Vector
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as BS
import qualified Ext.DependencyManager as DependencyManager
import qualified Watchtower.Server.LSP.Protocol as LSPProtocol
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as Time
import qualified System.Timeout as Timeout


availableTools :: [MCP.Tool]
availableTools =
  [ toolScaffoldElmApp
  , toolScaffoldElmPackage
  , toolCheck
  , toolDocs
  , toolInstall
  , toolDependencies
  , toolAdd
  , toolTestInstall
  ]

availableToolNamesForTests :: [Text]
availableToolNamesForTests =
  map MCP.toolName availableTools

availableToolSchemasForTests :: [(Text, JSON.Value)]
availableToolSchemasForTests =
  map (\tool -> (MCP.toolName tool, MCP.toolInputSchema tool)) availableTools

-- helpers
getStringArg :: JSON.Object -> Text -> Maybe String
getStringArg obj key =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (JSON.String t) -> Just (Text.unpack t)
    _ -> Nothing

getIntArg :: JSON.Object -> Text -> Maybe Int
getIntArg obj key =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (JSON.Number n) ->
      if Scientific.isInteger n then Just (floor n) else Nothing
    Just (JSON.String t) ->
      case reads (Text.unpack t) of
        [(i, "")] -> Just i
        _ -> Nothing
    _ -> Nothing

getBoolArg :: JSON.Object -> Text -> Maybe Bool
getBoolArg obj key =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (JSON.Bool b) -> Just b
    Just (JSON.String t) ->
      case Text.toLower t of
        "true" -> Just True
        "false" -> Just False
        _ -> Nothing
    _ -> Nothing

requireStringArg :: Text -> JSON.Object -> Either String String
requireStringArg key obj =
  case getStringArg obj key of
    Just v -> Right v
    Nothing -> Left ("Missing required string argument: " ++ Text.unpack key)

withDir :: FilePath -> IO a -> IO a
withDir dir action = Dir.withCurrentDirectory dir action

ok :: Text -> MCP.ToolCallResponse
ok msg = MCP.ToolCallResponse [MCP.ToolResponseText Nothing msg]

errTxt :: Text -> MCP.ToolCallResponse
errTxt msg = MCP.ToolCallResponse [MCP.ToolResponseError Nothing msg]

xmlBlock :: Text -> [(Text, Text)] -> Text -> Text
xmlBlock tag attrs body =
  let attrsText = Text.concat (map (\(k, v) -> " " <> k <> "=\"" <> xmlAttrEscape v <> "\"") attrs)
  in "<" <> tag <> attrsText <> ">\n" <> body <> "\n</" <> tag <> ">"

xmlBlockForTests :: Text -> [(Text, Text)] -> Text -> Text
xmlBlockForTests = xmlBlock

xmlAttrEscape :: Text -> Text
xmlAttrEscape =
  Text.replace "\"" "&quot;"
    . Text.replace "<" "&lt;"
    . Text.replace ">" "&gt;"
    . Text.replace "&" "&amp;"

getArrayArg :: JSON.Object -> Text -> Maybe [JSON.Value]
getArrayArg obj key =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (JSON.Array values) -> Just (Data.Vector.toList values)
    _ -> Nothing

objectField :: JSON.Object -> Text -> Maybe JSON.Object
objectField obj key =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (JSON.Object child) -> Just child
    _ -> Nothing

stringField :: JSON.Object -> Text -> Maybe Text
stringField obj key =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (JSON.String value) -> Just value
    _ -> Nothing

intField :: JSON.Object -> Text -> Maybe Int
intField obj key =
  case KeyMap.lookup (Key.fromText key) obj of
    Just (JSON.Number n) -> Scientific.toBoundedInteger n
    _ -> Nothing

projectRootText :: Client.ProjectCache -> Text
projectRootText (Client.ProjectCache proj _ _ _ _) =
  Text.pack (Ext.Dev.Project.getRoot proj)

rootAttrs :: Client.ProjectCache -> [(Text, Text)]
rootAttrs pc =
  [("root", projectRootText pc)]

maybeRootAttrs :: Maybe Client.ProjectCache -> [(Text, Text)]
maybeRootAttrs maybeProject =
  maybe [] rootAttrs maybeProject

resourceAsTool :: MCP.Resource -> Text -> Live.State -> JSONRPC.EventEmitter -> JSONRPC.ConnectionId -> IO MCP.ToolCallResponse
resourceAsTool resource uri state emit connId = do
  MCP.ReadResourceResponse contents <- MCP.read resource (MCP.ReadResourceRequest uri) state emit connId
  pure (MCP.ToolCallResponse (map (MCP.ToolResponseText Nothing . MCP.resourceContentText) contents))



rootDirSchema :: (Key.Key, JSON.Value)
rootDirSchema =
    "dir" .= JSON.object
            [ "type" .= ("string" :: Text)
            , "description" .= ("Root directory of the Elm project" :: Text)
            ]

projectIdSchema :: (Key.Key, JSON.Value)
projectIdSchema =
    "projectId" .= JSON.object
            [ "type" .= ("integer" :: Text)
            , "description" .= ("Optional short id of the project" :: Text)
            ]

schemaProject :: JSON.Value
schemaProject =
  JSON.object
    [ "type" .= ("object" :: Text)
    , "properties" .= JSON.object []
    , "required" .= ([] :: [Text])
    ]

schemaProjectPlus :: [(Text, Text)] -> JSON.Value
schemaProjectPlus extras =
  let 
    extraPairs :: [(Key.Key, JSON.Value)]
    extraPairs = fmap (\(name, desc) -> Key.fromText name .= JSON.object
                        [ "type" .= ("string" :: Text)
                        , "description" .= (desc :: Text)
                        ]) extras
    fields :: [(Key.Key, JSON.Value)]
    fields = extraPairs
    requiredFields :: [Text]
    requiredFields = fmap fst extras
  in
  JSON.object
    [ "type" .= ("object" :: Text)
    , "properties" .= JSON.object fields
    , "required" .= requiredFields
    ]

schemaDocsTool :: [(Text, Text)] -> [(Text, Text)] -> JSON.Value
schemaDocsTool required optional =
  schemaDocsToolHelp required (optional ++ [("dir", "Optional local Elm project directory to use instead of the focused project")])

schemaDocsToolNoDir :: [(Text, Text)] -> [(Text, Text)] -> JSON.Value
schemaDocsToolNoDir = schemaDocsToolHelp

schemaDocsToolHelp :: [(Text, Text)] -> [(Text, Text)] -> JSON.Value
schemaDocsToolHelp required optional =
  let
    field (name, desc) = Key.fromText name .= JSON.object
      [ "type" .= ("string" :: Text)
      , "description" .= (desc :: Text)
      ]
  in
  JSON.object
    [ "type" .= ("object" :: Text)
    , "properties" .= JSON.object (fmap field (required ++ optional))
    , "required" .= fmap fst required
    ]

-- scaffold app
toolScaffoldElmApp :: MCP.Tool
toolScaffoldElmApp = MCP.Tool
  { MCP.toolName = "generate_scaffold_app"
  , MCP.toolDescription = "Generate a new Elm application scaffold in the given directory."
  , MCP.toolInputSchema =
      JSON.object
        [ "type" .= ("object" :: Text)
        , "properties" .= JSON.object
            [ rootDirSchema
            ]
        , "required" .= (["dir"] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "dir" args of
        Left e -> pure (errTxt (Text.pack e))
        Right dir -> do
          r <- Exception.try (GenInit.run dir) :: IO (Either SomeException (Either Text ()))
          case r of
            Left _ -> pure (errTxt "Failed to initialize project")
            Right (Left msg) -> pure (errTxt msg)
            Right (Right ()) -> do
              -- Ensure elm-explorations/test is available in test-dependencies
              _ <- withDir dir $ do
                let testPkg = Pkg.toName (Utf8.fromChars "elm-explorations") "test"
                _ <- TestInstall.installTestDependency testPkg
                pure ()
              canonicalRoot <- Dir.canonicalizePath dir
              Watchtower.State.Discover.discover state canonicalRoot
              -- Select the newly created project for this connection, if discover found it
              let (Client.State _ mProjects _ _ _ _ _ _ _) = state
              projects1 <- Control.Concurrent.STM.readTVarIO mProjects
              case Client.findByRoot canonicalRoot projects1 of
                Just (Client.ProjectCache proj _ _ _ _) -> do
                  let pid = Ext.Dev.Project._shortId proj
                  _ <- Client.setFocusedProjectId state connId pid
                  Watchtower.Server.DevWS.broadcastServiceStatus state
                Nothing -> pure ()
              let body =
                    Text.unlines
                      [ "Created a new Elm application using the elm-dev architecture."
                      , ""
                      , "Key files:"
                      , "  - elm.dev.json"
                      , "  - elm.json"
                      , "  - README.md"
                      , "  - src/app/Page/Home.elm"
                      , ""
                      , "The project is now selected for this session."
                      , Text.concat ["Root: ", Text.pack canonicalRoot]
                      , "No need to call project_select; you can use other tools now."
                      , ""
                      , "Read `file://architecture` for an overview and guidance on how to work with this setup."
                      ]
              pure (ok body)
  }

-- (no-op placeholder; colors are controlled at render-time)

-- diagnostics
toolDiagnostics :: MCP.Tool
toolDiagnostics = MCP.Tool
  { MCP.toolName = "diagnostics"
  , MCP.toolDescription = "Show Elm compiler diagnostics for a project or file. Warnings are suppressed by default."
  , MCP.toolInputSchema =
      JSON.object
        [ "type" .= ("object" :: Text)
        , "properties" .= JSON.object
            [ Key.fromText "dir" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Optional local Elm project directory to use instead of the focused project" :: Text)
                ]
            , Key.fromText "file" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Optional absolute path to a file to filter diagnostics" :: Text)
                ]
            , Key.fromText "includeWarnings" .= JSON.object
                [ "type" .= ("boolean" :: Text)
                , "description" .= ("Include warnings in the report. Defaults to false." :: Text)
                ]
            , Key.fromText "limit" .= JSON.object
                [ "type" .= ("integer" :: Text)
                , "description" .= ("Maximum number of diagnostics to render. Defaults to 100." :: Text)
                ]
            ]
        , "required" .= ([] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      selection <- resolveDiagnosticsProjectFromArgs args state connId
      case selection of
        Left msg -> pure (errTxt msg)
        Right pc -> do
          let includeWarnings = Maybe.fromMaybe False (getBoolArg args "includeWarnings")
              limit = Maybe.fromMaybe 100 (getIntArg args "limit")
              maybeFile = getStringArg args "file"
          body <- diagnosticsReport state pc maybeFile includeWarnings limit
          pure (ok body)
  }

-- scaffold package
toolScaffoldElmPackage :: MCP.Tool
toolScaffoldElmPackage = MCP.Tool
  { MCP.toolName = "generate_scaffold_package"
  , MCP.toolDescription = "Generate a new Elm package scaffold in the given directory."
  , MCP.toolInputSchema =
      JSON.object
        [ "type" .= ("object" :: Text)
        , "properties" .= JSON.object
            [ rootDirSchema
            , Key.fromText "name" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Capitalized namespace this package is built around (e.g. Markdown)" :: Text)
                ]
            , Key.fromText "description" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Optional description to populate the elm.json summary field" :: Text)
                ]
            ]
        , "required" .= (["dir","name"] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      let mDesc = getStringArg args "description"
      case (requireStringArg "dir" args, requireStringArg "name" args) of
        (Left e, _) -> pure (errTxt (Text.pack e))
        (_, Left e) -> pure (errTxt (Text.pack e))
        (Right dir, Right nameStr) -> do
          r <- Exception.try (withDir dir $ do
                  exists <- Dir.doesFileExist ("elm.json" :: FilePath)
                  if exists
                    then pure (Left ("elm.json already exists" :: Text))
                    else do
                      Dir.createDirectoryIfMissing True ("src" :: FilePath)
                      -- Write placeholder module
                      let moduleName = nameStr
                      let placeholderSummary = maybe ("A new Elm package for " ++ moduleName) id mDesc
                      let moduleContent =
                            Text.unlines
                              [ "module " <> Text.pack moduleName <> " exposing (..)"
                              , ""
                              , "{-| " <> Text.pack placeholderSummary <> " -}"
                              , ""
                              , "-- Replace with real API"
                              , ""
                              , "example : String"
                              , "example ="
                              , "    \"Hello from " <> Text.pack moduleName <> "\""
                              ]
                      writeFile ("src" </> moduleName <> ".elm") (Text.unpack moduleContent)
                      -- Create elm.json for package
                      let Pkg.Name author _ = Pkg.dummyName
                      let projectPart = "elm-" ++ map toLower moduleName
                      let pkgName = Pkg.toName author projectPart
                      let summary = maybe Elm.Outline.defaultSummary Json.fromChars mDesc
                      let license = Licenses.bsd3
                      let version = V.one
                      let exposed = Elm.Outline.ExposedList [ Name.fromChars moduleName ]
                      let deps = Map.fromList [ (Pkg.core, Con.untilNextMajor (V.Version 1 0 5)) ]
                      let testDeps = Map.empty
                      let elmConstraint = Con.defaultElm
                      let outline = Elm.Outline.Pkg (Elm.Outline.PkgOutline pkgName summary license version exposed deps testDeps elmConstraint)
                      -- Write elm.json; ensure it hits disk even in memory mode
                      let builder = JE.encode (Elm.Outline.encode outline) <> BB.char7 '\n'
                      Ext.FileProxy.writeUtf8AllTheWayToDisk (dir </> "elm.json") (LBS.toStrict (BB.toLazyByteString builder))
                      -- Ensure elm-explorations/test is available in test-dependencies
                      let testPkg = Pkg.toName (Utf8.fromChars "elm-explorations") "test"
                      _ <- TestInstall.installTestDependency testPkg
                      pure (Right ())
                ) :: IO (Either SomeException (Either Text ()))
          case r of
            Left _ -> pure (errTxt "Failed to scaffold package")
            Right (Left msg) -> pure (errTxt (Text.concat ["Cannot scaffold package: ", msg]))
            Right (Right ()) -> do
              canonicalRoot <- Dir.canonicalizePath dir
              Watchtower.State.Discover.discover state canonicalRoot
              -- Select the newly created project for this connection, if discover found it
              let (Client.State _ mProjects _ _ _ _ _ _ _) = state
              projects1 <- Control.Concurrent.STM.readTVarIO mProjects
              case Client.findByRoot canonicalRoot projects1 of
                Just (Client.ProjectCache proj _ _ _ _) -> do
                  let pid = Ext.Dev.Project._shortId proj
                  _ <- Client.setFocusedProjectId state connId pid
                  Watchtower.Server.DevWS.broadcastServiceStatus state
                Nothing -> pure ()
              let body =
                    Text.unlines
                      [ "Created a new Elm package scaffold."
                      , ""
                      , "Key files:"
                      , "  - elm.json"
                      , "  - src/" <> Text.pack nameStr <> ".elm"
                      , ""
                      , "The project is now selected for this session."
                      , Text.concat ["Root: ", Text.pack canonicalRoot]
                      , "No need to call project_select; you can use other tools now."
                      , "Read `file://architecture` for an overview and guidance on how to work with this setup."
                      ]
              pure (ok body)
  }

-- compile
toolCompile :: MCP.Tool
toolCompile = MCP.Tool
  { MCP.toolName = "compile"
  , MCP.toolDescription = "Compile the Elm project (typecheck/build)."
  , MCP.toolInputSchema = schemaProject
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      selection <- ProjectLookup.resolveProjectFromSession Nothing connId state
      case selection of
        Left msg -> pure (errTxt msg)
        Right projCache -> do
          -- Ensure VFS is updated from disk and bump fs version if needed
          let (Client.ProjectCache proj _ _ _ _) = projCache
          _ <- Watchtower.State.Compile.ensureProjectFresh state "mcp.compile" projCache
          currentResult <- Control.Concurrent.STM.readTVarIO (Client.compileResult projCache)
          -- Also compile tests regardless of main compile result
          _ <- Watchtower.State.Compile.compileTests state projCache
          -- Mark compiled version = current fs version snapshot, regardless of success or error
          let projectRoot = Ext.Dev.Project.getRoot proj
          cur <- Versions.readVersions projectRoot
          Versions.setCompileVersionTo projectRoot (Versions.fsVersion cur)
          case currentResult of
            Client.Error clientErr -> do
              -- Render errors as terminal-style text, like `elm make`
              let doc =
                    case clientErr of
                      Client.ReactorError reactor ->
                        Help.reportToDoc (Exit.reactorToReport reactor)
                      Client.GenerationError msg ->
                        Help.reportToDoc (Help.report "GENERATION ERROR" Nothing msg [])
              let txt = Text.pack (Help.toString doc)
              pure (errTxt txt)
            _ -> pure (ok "Compiled successfully")
  }

-- check
toolCheck :: MCP.Tool
toolCheck = MCP.Tool
  { MCP.toolName = "check"
  , MCP.toolDescription = "Run Elm diagnostics and/or start background tests for a project. Poll or cancel test jobs with testJobs. Omit targets or tests to skip that check."
  , MCP.toolInputSchema =
      JSON.object
        [ "type" .= ("object" :: Text)
        , "properties" .= JSON.object
            [ Key.fromText "dir" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Optional local Elm project directory to use instead of the focused project" :: Text)
                ]
            , Key.fromText "includeWarnings" .= JSON.object
                [ "type" .= ("boolean" :: Text)
                , "description" .= ("Include warnings in diagnostics. Defaults to false." :: Text)
                ]
            , Key.fromText "limit" .= JSON.object
                [ "type" .= ("integer" :: Text)
                , "description" .= ("Maximum diagnostics per diagnostic target. Defaults to 100." :: Text)
                ]
            , Key.fromText "targets" .= JSON.object
                [ "type" .= ("object" :: Text)
                , "description" .= ("Tagged union: { kind: \"all\" } or { kind: \"only\", targets: [{ kind: \"project\" } | { kind: \"file\", file } | { kind: \"module\", module }] }" :: Text)
                ]
            , Key.fromText "tests" .= JSON.object
                [ "type" .= ("object" :: Text)
                , "description" .= ("Start one background test job: { kind: \"all\", waitSeconds?, executionTimeoutSeconds? } or { kind: \"only\", runs: [{ glob?, fuzz?, seed?, timeoutSeconds? }], waitSeconds?, executionTimeoutSeconds? }. glob matches exposed top-level Test values by qualified Elm name, for example \"AppSpatialInteractionTest.tests\", \"AppSpatialInteractionTest.*\", or \"*.tests\"; it does not match test labels or file paths. Defaults: wait 5 seconds (maximum 20), execute the job for up to 900 seconds." :: Text)
                ]
            , Key.fromText "testJobs" .= JSON.object
                [ "type" .= ("array" :: Text)
                , "description" .= ("Inspect or cancel background test jobs: [{ kind: \"status\", runId? } | { kind: \"cancel\", runId }]. Omit runId from status to list recent jobs." :: Text)
                ]
            ]
        , "required" .= ([] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      let maybeTargets = objectField args "targets"
          maybeTests = objectField args "tests"
      case (maybeTargets, maybeTests) of
        (Nothing, Nothing) -> do
          jobActionBlocks <- runCheckTestJobActions args
          if null jobActionBlocks
            then pure (errTxt "Provide targets, tests, and/or testJobs. Use { kind: \"all\" } to check the whole project.")
            else pure (ok (Text.intercalate "\n\n" jobActionBlocks))
        _ -> do
          selection <- resolveDiagnosticsProjectFromArgs args state connId
          case selection of
            Left msg -> pure (errTxt msg)
            Right pc -> do
              jobActionBlocks <- runCheckTestJobActions args
              let includeWarnings = Maybe.fromMaybe False (getBoolArg args "includeWarnings")
                  limit = Maybe.fromMaybe 100 (getIntArg args "limit")
              diagBlocks <- runCheckDiagnostics state pc includeWarnings limit maybeTargets
              testBlocks <- maybe (pure []) (startCheckTests pc) maybeTests
              jobsSummary <- renderTestJobsSummary (Text.unpack (projectRootText pc))
              let blocks = jobActionBlocks ++ diagBlocks ++ testBlocks ++ [jobsSummary]
              pure (ok (Text.intercalate "\n\n" blocks))
  }

runCheckDiagnostics :: Client.State -> Client.ProjectCache -> Bool -> Int -> Maybe JSON.Object -> IO [Text]
runCheckDiagnostics state pc includeWarnings limit maybeTargets =
  case maybeTargets of
    Nothing -> pure []
    Just targetsObj ->
      case stringField targetsObj "kind" of
        Just "all" -> do
          body <- diagnosticsReport state pc Nothing includeWarnings limit
          pure [xmlBlock "diagnostics" ([("index", "0"), ("kind", "all"), ("status", "ok")] ++ rootAttrs pc) body]
        Just "only" ->
          case getArrayArg targetsObj "targets" of
            Nothing -> pure [xmlBlock "diagnostics" [("status", "error")] "Expected targets: { kind: \"only\", targets: [...] }." ]
            Just targets -> fmap concat (mapM (runOneDiagnosticTarget state pc includeWarnings limit) (zip [(0 :: Int)..] targets))
        Just other -> pure [xmlBlock "diagnostics" [("status", "error"), ("kind", other)] "Unknown diagnostics target selection kind."]
        Nothing -> pure [xmlBlock "diagnostics" [("status", "error")] "Missing targets.kind."]

runOneDiagnosticTarget :: Client.State -> Client.ProjectCache -> Bool -> Int -> (Int, JSON.Value) -> IO [Text]
runOneDiagnosticTarget state pc includeWarnings limit (index, value) =
  case value of
    JSON.Object targetObj ->
      case stringField targetObj "kind" of
        Just "project" -> do
          body <- diagnosticsReport state pc Nothing includeWarnings limit
          pure [xmlBlock "diagnostics" ([("index", Text.pack (show index)), ("kind", "project"), ("status", "ok")] ++ rootAttrs pc) body]
        Just "file" ->
          case stringField targetObj "file" of
            Nothing -> pure [xmlBlock "diagnostics" [("index", Text.pack (show index)), ("kind", "file"), ("status", "error")] "Missing file."]
            Just filePath -> do
              body <- diagnosticsReport state pc (Just (Text.unpack filePath)) includeWarnings limit
              pure [xmlBlock "diagnostics" ([("index", Text.pack (show index)), ("kind", "file"), ("file", filePath), ("status", "ok")] ++ rootAttrs pc) body]
        Just "module" ->
          case stringField targetObj "module" of
            Nothing -> pure [xmlBlock "diagnostics" [("index", Text.pack (show index)), ("kind", "module"), ("status", "error")] "Missing module."]
            Just moduleName -> do
              resolved <- resolveModuleSpecScoped state (Just pc) (ModuleByName (Name.fromChars (Text.unpack moduleName)))
              case resolved of
                Right (ModuleResolved _ (Just filePath) _ _) -> do
                  body <- diagnosticsReport state pc (Just filePath) includeWarnings limit
                  pure [xmlBlock "diagnostics" ([("index", Text.pack (show index)), ("kind", "module"), ("module", moduleName), ("file", Text.pack filePath), ("status", "ok")] ++ rootAttrs pc) body]
                Right _ -> pure [xmlBlock "diagnostics" [("index", Text.pack (show index)), ("kind", "module"), ("module", moduleName), ("status", "error")] "Module resolved to package docs, not a local file."]
                Left msg -> pure [xmlBlock "diagnostics" [("index", Text.pack (show index)), ("kind", "module"), ("module", moduleName), ("status", "error")] msg]
        Just other -> pure [xmlBlock "diagnostics" [("index", Text.pack (show index)), ("kind", other), ("status", "error")] "Unknown diagnostic target kind."]
        Nothing -> pure [xmlBlock "diagnostics" [("index", Text.pack (show index)), ("status", "error")] "Missing target.kind."]
    _ -> pure [xmlBlock "diagnostics" [("index", Text.pack (show index)), ("status", "error")] "Expected target object."]

data CheckTestRun = CheckTestRun
  { checkTestBlock :: Text
  , checkTestPassed :: Int
  , checkTestFailed :: Int
  , checkTestTotal :: Int
  , checkTestInfrastructureFailed :: Bool
  }


startCheckTests :: Client.ProjectCache -> JSON.Object -> IO [Text]
startCheckTests pc testsObj = do
  let waitSeconds = max 0 (min 20 (Maybe.fromMaybe 5 (intField testsObj "waitSeconds")))
      executionTimeoutSeconds = max 1 (Maybe.fromMaybe 900 (intField testsObj "executionTimeoutSeconds"))
      root = Text.unpack (projectRootText pc)
  submitted <- TestJobs.submit root $ \reportProgress ->
    do
      result <- Timeout.timeout (executionTimeoutSeconds * 1000000)
        (runCheckTestsNow pc reportProgress testsObj)
      pure $ case result of
        Nothing -> TestJobs.ActionFailed ("Test job timed out after " <> Text.pack (show executionTimeoutSeconds) <> " seconds.")
        Just completed -> completed
  waited <- TestJobs.waitFor (TestJobs.jobId submitted) waitSeconds
  pure [renderTestJob True (Maybe.fromMaybe submitted waited)]


runCheckTestsNow :: Client.ProjectCache -> (Text -> IO ()) -> JSON.Object -> IO TestJobs.ActionResult
runCheckTestsNow pc reportProgress testsObj =
  case stringField testsObj "kind" of
    Just "all" -> do
      reportProgress "Running test configuration 1/1"
      result <- runOneCheckTest pc 0 JSON.Null
      pure (combineCheckTestRuns [result])
    Just "only" ->
      case getArrayArg testsObj "runs" of
        Nothing -> pure (TestJobs.ActionFailed "Expected tests: { kind: \"only\", runs: [...] }.")
        Just [] -> pure (TestJobs.ActionFailed "Expected at least one test run.")
        Just runs -> do
          let totalRuns = length runs
          results <- mapM
            (\(index, value) -> do
                reportProgress ("Running test configuration " <> Text.pack (show (index + 1)) <> "/" <> Text.pack (show totalRuns))
                runOneCheckTest pc index value
            )
            (zip [(0 :: Int)..] runs)
          pure (combineCheckTestRuns results)
    Just other -> pure (TestJobs.ActionFailed ("Unknown test selection kind: " <> other))
    Nothing -> pure (TestJobs.ActionFailed "Missing tests.kind.")


combineCheckTestRuns :: [CheckTestRun] -> TestJobs.ActionResult
combineCheckTestRuns results =
  let body = Text.intercalate "\n\n" (map checkTestBlock results)
      passed = sum (map checkTestPassed results)
      failed = sum (map checkTestFailed results)
      total = sum (map checkTestTotal results)
  in if any checkTestInfrastructureFailed results
       then TestJobs.ActionFailed body
       else TestJobs.ActionCompleted TestJobs.Completion
         { TestJobs.completionOutcome = if failed > 0 then TestJobs.TestsFailed else TestJobs.TestsPassed
         , TestJobs.completionBody = body
         , TestJobs.completionPassed = passed
         , TestJobs.completionFailed = failed
         , TestJobs.completionTotal = total
         }


runOneCheckTest :: Client.ProjectCache -> Int -> JSON.Value -> IO CheckTestRun
runOneCheckTest (Client.ProjectCache proj _ _ _ mTestVar) index value = do
  let dir = Ext.Dev.Project.getRoot proj
      runObj = case value of
        JSON.Object obj -> obj
        _ -> KeyMap.empty
      mTimeoutSeconds = fmap (max 1) (intField runObj "timeoutSeconds")
      mGlobText = stringField runObj "glob"
      mGlobString = fmap Text.unpack mGlobText
      mGlobs = fmap (\g -> [g]) mGlobString
      mSeed = intField runObj "seed"
      mFuzz = intField runObj "fuzz"
      baseAttrs = [("index", Text.pack (show index)), ("root", Text.pack dir)] ++ maybe [] (\g -> [("glob", g)]) mGlobText
      isUnfilteredRun = Maybe.isNothing mGlobText
  Control.Concurrent.STM.atomically $ Watchtower.State.Compile.clearTestResults mTestVar
  versionsVar <- Versions.getOrInit dir
  versionsAtStart <- Control.Concurrent.STM.readTVarIO versionsVar
  startedAt <- Time.getCurrentTime
  result <- TestRunner.run mTimeoutSeconds mGlobs mSeed mFuzz dir
  case result of
    Left e ->
      let msg = case e of
            TestRunner.TimedOut timeoutSeconds -> Text.pack ("Tests timed out after " ++ show (timeoutSeconds * 1000) ++ "ms")
            TestRunner.RunFailed runMsg -> Text.pack runMsg
          block = xmlBlock "tests" ([("index", Text.pack (show index)), ("status", "error"), ("root", Text.pack dir)] ++ maybe [] (\g -> [("glob", g)]) mGlobText) msg
      in pure (CheckTestRun block 0 0 0 True)
    Right (TestRunner.RunSuccess info reports seed fuzz) -> do
      finishedAt <- Time.getCurrentTime
      let durationMs = round (realToFrac (Time.diffUTCTime finishedAt startedAt) * 1000 :: Double)
          rendered = TestReport.renderReportsWithDuration False (Just durationMs) (Just seed) (Just fuzz) reports
          summary = TestRunner.tiSummary info
          total = TestRunner.summaryTotal summary
          passed = TestRunner.summaryPassed summary
          failed = TestRunner.summaryFailed summary
          failures = TestRunner.summaryFailures summary
          tr = Client.TestResults total passed failed failures
          clientInfo = Client.TestInfo
                         { Client.testFiles = TestRunner.tiFiles info
                         , Client.testResults = Just tr
                         , Client.testCompilation = Just Client.TestSuccess
                         }
          attrs = baseAttrs ++ [("status", if failed > 0 then "failed" else "ok"), ("passed", Text.pack (show passed)), ("failed", Text.pack (show failed)), ("total", Text.pack (show total))]
      when isUnfilteredRun $ Control.Concurrent.STM.atomically $ do
        versionsAtEnd <- Control.Concurrent.STM.readTVar versionsVar
        when (Versions.fsVersion versionsAtStart == Versions.fsVersion versionsAtEnd) $
          Control.Concurrent.STM.writeTVar mTestVar (Just clientInfo)
      Ext.Log.log Ext.Log.Test rendered
      pure (CheckTestRun (xmlBlock "tests" attrs (Text.pack rendered)) passed failed total False)


runCheckTestJobActions :: JSON.Object -> IO [Text]
runCheckTestJobActions args =
  case KeyMap.lookup "testJobs" args of
    Nothing -> pure []
    Just (JSON.Array actions) -> mapM runCheckTestJobAction (zip [(0 :: Int)..] (Data.Vector.toList actions))
    Just _ -> pure [xmlBlock "test-job" [("status", "error")] "Expected testJobs to be an array."]


runCheckTestJobAction :: (Int, JSON.Value) -> IO Text
runCheckTestJobAction (index, value) =
  case value of
    JSON.Object actionObj ->
      case stringField actionObj "kind" of
        Just "status" ->
          case stringField actionObj "runId" of
            Just runId -> do
              found <- TestJobs.lookupJob runId
              pure $ case found of
                Nothing -> xmlBlock "test-job" [("index", Text.pack (show index)), ("runId", runId), ("status", "error")] "Unknown or expired test job."
                Just job -> renderTestJob True job
            Nothing -> renderTestJobsCollection Nothing <$> TestJobs.listJobs
        Just "cancel" ->
          case stringField actionObj "runId" of
            Nothing -> pure (xmlBlock "test-job" [("index", Text.pack (show index)), ("status", "error")] "Missing runId.")
            Just runId -> do
              cancelled <- TestJobs.cancel runId
              pure $ case cancelled of
                Nothing -> xmlBlock "test-job" [("index", Text.pack (show index)), ("runId", runId), ("status", "error")] "Unknown or expired test job."
                Just job -> renderTestJob False job
        Just other -> pure (xmlBlock "test-job" [("index", Text.pack (show index)), ("kind", other), ("status", "error")] "Unknown testJobs action kind.")
        Nothing -> pure (xmlBlock "test-job" [("index", Text.pack (show index)), ("status", "error")] "Missing testJobs action kind.")
    _ -> pure (xmlBlock "test-job" [("index", Text.pack (show index)), ("status", "error")] "Expected testJobs action object.")


renderTestJobsSummary :: FilePath -> IO Text
renderTestJobsSummary root =
  renderTestJobsCollection (Just root) <$> TestJobs.listJobsForRoot root


renderTestJobsCollection :: Maybe FilePath -> [TestJobs.JobView] -> Text
renderTestJobsCollection maybeRoot jobs =
  let running = length (filter (\job -> TestJobs.jobStatus job == TestJobs.JobRunning || TestJobs.jobStatus job == TestJobs.JobQueued) jobs)
      completed = length (filter ((== TestJobs.JobCompleted) . TestJobs.jobStatus) jobs)
      failed = length (filter ((== TestJobs.JobFailed) . TestJobs.jobStatus) jobs)
      testsFailed = length
        [ ()
        | job <- jobs
        , Just completion <- [TestJobs.jobCompletion job]
        , TestJobs.completionOutcome completion == TestJobs.TestsFailed
        ]
      cancelled = length (filter ((== TestJobs.JobCancelled) . TestJobs.jobStatus) jobs)
      attrs =
        maybe [] (\root -> [("root", Text.pack root)]) maybeRoot
          ++ [("running", Text.pack (show running)), ("completed", Text.pack (show completed)), ("testsFailed", Text.pack (show testsFailed)), ("failed", Text.pack (show failed)), ("cancelled", Text.pack (show cancelled))]
      body = if null jobs then "No test jobs." else Text.intercalate "\n" (map renderTestJobSummaryLine (take 20 jobs))
  in xmlBlock "test-jobs" attrs body


renderTestJobSummaryLine :: TestJobs.JobView -> Text
renderTestJobSummaryLine job =
  let counts = case TestJobs.jobCompletion job of
        Nothing -> ""
        Just completion ->
          " outcome=" <> jobOutcomeText (TestJobs.completionOutcome completion)
            <> " passed=" <> Text.pack (show (TestJobs.completionPassed completion))
            <> " failed=" <> Text.pack (show (TestJobs.completionFailed completion))
            <> " total=" <> Text.pack (show (TestJobs.completionTotal completion))
  in TestJobs.jobId job <> " " <> jobStatusText (TestJobs.jobStatus job) <> counts <> " " <> TestJobs.jobMessage job


renderTestJob :: Bool -> TestJobs.JobView -> Text
renderTestJob detailed job =
  let completionAttrs = case TestJobs.jobCompletion job of
        Nothing -> []
        Just completion ->
          [("outcome", jobOutcomeText (TestJobs.completionOutcome completion)), ("passed", Text.pack (show (TestJobs.completionPassed completion))), ("failed", Text.pack (show (TestJobs.completionFailed completion))), ("total", Text.pack (show (TestJobs.completionTotal completion)))]
      attrs =
        [("runId", TestJobs.jobId job), ("status", jobStatusText (TestJobs.jobStatus job)), ("root", Text.pack (TestJobs.jobRoot job)), ("createdAt", Text.pack (show (TestJobs.jobCreatedAt job)))]
          ++ maybe [] (\startedAt -> [("startedAt", Text.pack (show startedAt))]) (TestJobs.jobStartedAt job)
          ++ maybe [] (\completedAt -> [("completedAt", Text.pack (show completedAt))]) (TestJobs.jobCompletedAt job)
          ++ maybe [] (\expiresAt -> [("expiresNoLaterThan", Text.pack (show expiresAt))]) (TestJobs.jobExpiresNoLaterThan job)
          ++ completionAttrs
      body
        | detailed = case (TestJobs.jobCompletion job, TestJobs.jobFailure job) of
            (Just completion, _) -> TestJobs.completionBody completion
            (_, Just failure) -> failure
            _ -> TestJobs.jobMessage job
        | otherwise = TestJobs.jobMessage job
  in xmlBlock "test-job" attrs body


jobStatusText :: TestJobs.JobStatus -> Text
jobStatusText status =
  case status of
    TestJobs.JobQueued -> "queued"
    TestJobs.JobRunning -> "running"
    TestJobs.JobCompleted -> "completed"
    TestJobs.JobFailed -> "failed"
    TestJobs.JobCancelled -> "cancelled"


jobOutcomeText :: TestJobs.JobOutcome -> Text
jobOutcomeText outcome =
  case outcome of
    TestJobs.TestsPassed -> "passed"
    TestJobs.TestsFailed -> "tests_failed"

-- docs_package
toolDocs :: MCP.Tool
toolDocs = MCP.Tool
  { MCP.toolName = "docs"
  , MCP.toolDescription = "Get multiple Elm docs results in one call. queries is an array of tagged unions: project, modules, module, file, value, search, package."
  , MCP.toolInputSchema =
      JSON.object
        [ "type" .= ("object" :: Text)
        , "properties" .= JSON.object
            [ Key.fromText "dir" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Optional local Elm project directory to use instead of the focused project" :: Text)
                ]
            , Key.fromText "queries" .= JSON.object
                [ "type" .= ("array" :: Text)
                , "description" .= ("Tagged docs queries: { kind: \"project\" }, { kind: \"modules\" }, { kind: \"module\", module }, { kind: \"file\", file }, { kind: \"value\", value }, { kind: \"search\", query, package?, version? }, { kind: \"package\", package, version? }" :: Text)
                , "items" .= JSON.object [ "type" .= ("object" :: Text) ]
                ]
            ]
        , "required" .= (["queries"] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      case getArrayArg args "queries" of
        Nothing -> pure (errTxt "Missing queries array.")
        Just queries -> do
          blocks <- mapM (runDocsQuery args state connId) (zip [(0 :: Int)..] queries)
          pure (ok (Text.intercalate "\n\n" blocks))
  }

runDocsQuery :: JSON.Object -> Client.State -> JSONRPC.ConnectionId -> (Int, JSON.Value) -> IO Text
runDocsQuery args state connId (index, value) =
  case value of
    JSON.Object query ->
      case stringField query "kind" of
        Just "project" -> do
          mProject <- resolveDocsProjectFromArgs args state connId
          case mProject of
            Left msg -> pure (docsError index "project" [] msg)
            Right Nothing -> pure (docsError index "project" [] "No project selected")
            Right (Just pc) -> do
              localDocs <- localModulesForProject state pc
              pkgs <- Control.Concurrent.STM.readTVarIO (Client.packages state)
              outline <- Elm.Outline.read (Ext.Dev.Project.getRoot (projectFromCache pc))
              pure (xmlBlock "docs" ([("index", Text.pack (show index)), ("kind", "project"), ("status", "ok")] ++ rootAttrs pc) (renderDocsProject pc outline localDocs pkgs))
        Just "modules" -> do
          mProject <- resolveDocsProjectFromArgs args state connId
          case mProject of
            Left msg -> pure (docsError index "modules" [] msg)
            Right maybeProject -> do
              localDocs <- maybe (pure []) (localModulesForProject state) maybeProject
              pkgs <- Control.Concurrent.STM.readTVarIO (Client.packages state)
              outline <- traverse (Elm.Outline.read . Ext.Dev.Project.getRoot . projectFromCache) maybeProject
              pure (xmlBlock "docs" ([("index", Text.pack (show index)), ("kind", "modules"), ("status", "ok")] ++ maybeRootAttrs maybeProject) (renderDocsModules maybeProject outline localDocs pkgs))
        Just "module" ->
          case stringField query "module" of
            Nothing -> pure (docsError index "module" [] "Missing module.")
            Just moduleName -> do
              mProject <- resolveDocsProjectFromArgs args state connId
              case mProject of
                Left msg -> pure (docsError index "module" [("module", moduleName)] msg)
                Right maybeProject -> do
                  let wantName = Name.fromChars (Text.unpack moduleName)
                  r <- resolveModuleSpecScoped state maybeProject (ModuleByName wantName)
                  case r of
                    Right (ModuleResolved _ maybePath maybePkg docsMod) -> do
                      let meta = DocsRender.ModuleMeta moduleName (fmap (Ext.Dev.Project.getRoot . projectFromCache) maybeProject) maybePath (fmap (Text.pack . Pkg.toChars) maybePkg)
                      pure (xmlBlock "docs" ([("index", Text.pack (show index)), ("kind", "module"), ("module", moduleName), ("status", "ok")] ++ maybeRootAttrs maybeProject) (DocsRender.renderModule meta docsMod))
                    Left _ -> pure (docsError index "module" [("module", moduleName)] ("No module found named " <> moduleName))
        Just "file" ->
          case stringField query "file" of
            Nothing -> pure (docsError index "file" [] "Missing file.")
            Just filePath -> do
              mProject <- resolveDocsProjectFromArgs args state connId
              case mProject of
                Left msg -> pure (docsError index "file" [("file", filePath)] msg)
                Right maybeProject -> do
                  r <- resolveModuleSpecScoped state maybeProject (ModuleByFilePath (Text.unpack filePath))
                  case r of
                    Right (ModuleResolved moduleName maybePath maybePkg docsMod) -> do
                      let moduleText = Text.pack (Name.toChars moduleName)
                          meta = DocsRender.ModuleMeta moduleText (fmap (Ext.Dev.Project.getRoot . projectFromCache) maybeProject) maybePath (fmap (Text.pack . Pkg.toChars) maybePkg)
                      pure (xmlBlock "docs" ([("index", Text.pack (show index)), ("kind", "file"), ("file", filePath), ("module", moduleText), ("status", "ok")] ++ maybeRootAttrs maybeProject) (DocsRender.renderModule meta docsMod))
                    Left msg -> pure (docsError index "file" [("file", filePath)] msg)
        Just "value" ->
          case stringField query "value" of
            Nothing -> pure (docsError index "value" [] "Missing value.")
            Just valueName -> do
              mProject <- resolveDocsProjectFromArgs args state connId
              case mProject of
                Left msg -> pure (docsError index "value" [("value", valueName)] msg)
                Right maybeProject ->
                  case splitModuleAndValue valueName of
                    Nothing -> pure (docsError index "value" [("value", valueName)] "Expected fully-qualified value name, e.g. List.map")
                    Just (modName, valName) -> do
                      r <- resolveModuleSpecScoped state maybeProject (ModuleByName modName)
                      case r of
                        Right (ModuleResolved _ maybePath maybePkg docsMod) -> do
                          let moduleText = Text.pack (Name.toChars modName)
                              valueText = Text.pack (Name.toChars valName)
                              meta = DocsRender.ValueMeta moduleText valueText (fmap (Ext.Dev.Project.getRoot . projectFromCache) maybeProject) maybePath (fmap (Text.pack . Pkg.toChars) maybePkg)
                          pure (xmlBlock "docs" ([("index", Text.pack (show index)), ("kind", "value"), ("value", valueName), ("status", "ok")] ++ maybeRootAttrs maybeProject) (DocsRender.renderValue meta docsMod valName))
                        Left _ -> pure (docsError index "value" [("value", valueName)] ("No value found that matches " <> valueName))
        Just "search" ->
          case stringField query "query" of
            Nothing -> pure (docsError index "search" [] "Missing query.")
            Just searchQuery -> do
              packageDocs <- case stringField query "package" of
                Nothing -> pure (Right Nothing)
                Just pkg -> case parsePkgName pkg of
                  Nothing -> pure (Left "Invalid package name. Expected author/project")
                  Just pkgName -> do
                    let mVersionText = stringField query "version"
                    case traverse parseVersionText mVersionText of
                      Nothing -> pure (Left "Invalid version. Expected MAJOR.MINOR.PATCH, e.g. 1.1.3")
                      Just mVersion -> do
                        mRoot <- docsRootFromArgs args state connId
                        docsResult <- loadPackageDocsForTool state pkgName mVersion mRoot
                        case docsResult of
                          Left msg -> pure (Left msg)
                          Right docsMap -> pure (Right (Just (pkgName, Map.elems docsMap)))
              case packageDocs of
                Left msg -> pure (docsError index "search" [("query", searchQuery)] msg)
                Right scopedPackage -> do
                  mProject <- case (scopedPackage, getStringArg args "dir") of
                    (Just _, Nothing) -> pure (Right Nothing)
                    _ -> resolveDocsProjectFromArgs args state connId
                  case mProject of
                    Left msg -> pure (docsError index "search" [("query", searchQuery)] msg)
                    Right maybeProject -> do
                      localDocs <- maybe (pure []) (localModulesForProject state) maybeProject
                      pkgs <- Control.Concurrent.STM.readTVarIO (Client.packages state)
                      let packageDocsList = case scopedPackage of
                            Just (pkgName, docsMods) -> [(pkgName, docsMods)]
                            Nothing -> fmap (\(pkgName, info) -> (pkgName, fmap Client.packageModuleDocs (Map.elems (Client.packageModules info)))) (Map.toList pkgs)
                      pure (xmlBlock "docs" ([("index", Text.pack (show index)), ("kind", "search"), ("query", searchQuery), ("status", "ok")] ++ maybeRootAttrs maybeProject) (renderDocsSearch searchQuery localDocs packageDocsList))
        Just "package" ->
          case stringField query "package" of
            Nothing -> pure (docsError index "package" [] "Missing package.")
            Just pkg ->
              case parsePkgName pkg of
                Nothing -> pure (docsError index "package" [("package", pkg)] "Invalid package name. Expected author/project")
                Just pkgName -> do
                  mRoot <- docsRootFromArgs args state connId
                  let mVersionText = stringField query "version"
                  case traverse parseVersionText mVersionText of
                    Nothing -> pure (docsError index "package" [("package", pkg)] "Invalid version. Expected MAJOR.MINOR.PATCH, e.g. 1.1.3")
                    Just mVersion -> do
                      result <- renderPackageDocsText state pkgName mVersion mRoot
                      case result of
                        Left msg -> pure (docsError index "package" [("package", pkg)] msg)
                        Right body -> pure (xmlBlock "docs" ([("index", Text.pack (show index)), ("kind", "package"), ("package", pkg), ("status", "ok")] ++ maybe [] (\root -> [("root", Text.pack root)]) mRoot) body)
        Just other -> pure (docsError index other [] "Unknown docs query kind.")
        Nothing -> pure (docsError index "unknown" [] "Missing query.kind.")
    _ -> pure (docsError index "unknown" [] "Expected query object.")

docsError :: Int -> Text -> [(Text, Text)] -> Text -> Text
docsError index kind extraAttrs msg =
  xmlBlock "docs" ([("index", Text.pack (show index)), ("kind", kind), ("status", "error")] ++ extraAttrs) msg

toolDocsPackage :: MCP.Tool
toolDocsPackage = MCP.Tool
  { MCP.toolName = "docs_package"
  , MCP.toolDescription = "Get rendered Elm package documentation (author/project), e.g. elm/json. Uses the focused project's elm.json version when available, otherwise the latest registry version."
  , MCP.toolInputSchema = schemaDocsTool [("package", "Elm package name, e.g. elm/json")] [("version", "Optional exact package version, e.g. 1.1.3")]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state emit connId -> do
      case requireStringArg "package" args of
        Left e -> pure (errTxt (Text.pack e))
        Right pkg ->
          case parsePkgName (Text.pack pkg) of
            Nothing -> pure (errTxt "Invalid package name. Expected author/project")
            Just pkgName -> do
              mRoot <- docsRootFromArgs args state connId
              let mVersionText = fmap Text.pack (getStringArg args "version")
              case traverse parseVersionText mVersionText of
                Nothing -> pure (errTxt "Invalid version. Expected MAJOR.MINOR.PATCH, e.g. 1.1.3")
                Just mVersion -> renderPackageDocsTool state pkgName mVersion mRoot
  }

-- docs_module
toolDocsModule :: MCP.Tool
toolDocsModule = MCP.Tool
  { MCP.toolName = "docs_module"
  , MCP.toolDescription = "Get rendered documentation for a local project or dependency module, e.g. List or App.Main."
  , MCP.toolInputSchema = schemaDocsTool [("module", "Elm module name, e.g. List or App.Main")] []
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state emit connId -> do
      case requireStringArg "module" args of
        Left e -> pure (errTxt (Text.pack e))
        Right moduleName -> do
          mProject <- resolveDocsProjectFromArgs args state connId
          case mProject of
            Left msg -> pure (errTxt msg)
            Right maybeProject -> do
              let wantName = Name.fromChars moduleName
              r <- resolveModuleSpecScoped state maybeProject (ModuleByName wantName)
              case r of
                Right (ModuleResolved _ maybePath maybePkg docsMod) -> do
                  let meta = DocsRender.ModuleMeta (Text.pack moduleName) (fmap (Ext.Dev.Project.getRoot . projectFromCache) maybeProject) maybePath (fmap (Text.pack . Pkg.toChars) maybePkg)
                      body = DocsRender.renderModule meta docsMod
                  pure (ok body)
                Left _ -> pure (errTxt ("No module found named " <> Text.pack moduleName))
  }

-- docs_file
toolDocsFile :: MCP.Tool
toolDocsFile = MCP.Tool
  { MCP.toolName = "docs_file"
  , MCP.toolDescription = "Get rendered documentation for a local Elm file by absolute path."
  , MCP.toolInputSchema = schemaDocsToolNoDir [("file", "Absolute path to an Elm file")] []
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state emit connId -> do
      case requireStringArg "file" args of
        Left e -> pure (errTxt (Text.pack e))
        Right filePath -> do
          let uri = Text.concat ["elm://docs/file?file=", Text.pack filePath]
          resourceAsTool resourceFileDocs uri state emit connId
  }

-- docs_value
toolDocsValue :: MCP.Tool
toolDocsValue = MCP.Tool
  { MCP.toolName = "docs_value"
  , MCP.toolDescription = "Get rendered documentation for a value, type alias, custom type, or operator, e.g. List.map."
  , MCP.toolInputSchema = schemaDocsTool [("value", "Fully-qualified Elm value name, e.g. List.map")] []
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state emit connId -> do
      case requireStringArg "value" args of
        Left e -> pure (errTxt (Text.pack e))
        Right valueName -> do
          mProject <- resolveDocsProjectFromArgs args state connId
          case mProject of
            Left msg -> pure (errTxt msg)
            Right maybeProject ->
              case splitModuleAndValue (Text.pack valueName) of
                Nothing -> pure (errTxt "Expected fully-qualified value name, e.g. List.map")
                Just (modName, valName) -> do
                  r <- resolveModuleSpecScoped state maybeProject (ModuleByName modName)
                  case r of
                    Right (ModuleResolved _ maybePath maybePkg docsMod) -> do
                      let meta = DocsRender.ValueMeta
                                   (Text.pack (Name.toChars modName))
                                   (Text.pack (Name.toChars valName))
                                   (fmap (Ext.Dev.Project.getRoot . projectFromCache) maybeProject)
                                   maybePath
                                   (fmap (Text.pack . Pkg.toChars) maybePkg)
                          body = DocsRender.renderValue meta docsMod valName
                      pure (ok body)
                    Left _ -> pure (errTxt ("No value found that matches " <> Text.pack valueName))
  }

-- docs_search
toolDocsSearch :: MCP.Tool
toolDocsSearch = MCP.Tool
  { MCP.toolName = "docs_search"
  , MCP.toolDescription = "Search local project and dependency documentation by module, value/type/operator name, or doc comment."
  , MCP.toolInputSchema = schemaDocsTool [("query", "Search query")]
      [("package", "Optional package name to search, e.g. elm/json"), ("version", "Optional exact package version when package is provided")]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "query" args of
        Left e -> pure (errTxt (Text.pack e))
        Right query -> do
          packageDocs <- case getStringArg args "package" of
            Nothing -> pure (Right Nothing)
            Just pkg -> case parsePkgName (Text.pack pkg) of
              Nothing -> pure (Left "Invalid package name. Expected author/project")
              Just pkgName -> do
                let mVersionText = fmap Text.pack (getStringArg args "version")
                case traverse parseVersionText mVersionText of
                  Nothing -> pure (Left "Invalid version. Expected MAJOR.MINOR.PATCH, e.g. 1.1.3")
                  Just mVersion -> do
                    mRoot <- docsRootFromArgs args state connId
                    docsResult <- loadPackageDocsForTool state pkgName mVersion mRoot
                    case docsResult of
                      Left msg -> pure (Left msg)
                      Right docsMap -> pure (Right (Just (pkgName, Map.elems docsMap)))
          case packageDocs of
            Left msg -> pure (errTxt msg)
            Right scopedPackage -> do
              mProject <- case (scopedPackage, getStringArg args "dir") of
                (Just _, Nothing) -> pure (Right Nothing)
                _ -> resolveDocsProjectFromArgs args state connId
              case mProject of
                Left msg -> pure (errTxt msg)
                Right maybeProject -> do
                  localDocs <- maybe (pure []) (localModulesForProject state) maybeProject
                  pkgs <- Control.Concurrent.STM.readTVarIO (Client.packages state)
                  let packageDocsList = case scopedPackage of
                        Just (pkgName, docsMods) -> [(pkgName, docsMods)]
                        Nothing -> fmap (\(pkgName, info) -> (pkgName, fmap Client.packageModuleDocs (Map.elems (Client.packageModules info)))) (Map.toList pkgs)
                      body = renderDocsSearch (Text.pack query) localDocs packageDocsList
                  pure (ok body)
  }

-- docs_modules
toolDocsModules :: MCP.Tool
toolDocsModules = MCP.Tool
  { MCP.toolName = "docs_modules"
  , MCP.toolDescription = "List documented modules for a project and loaded dependencies, grouped by local/public/private and package."
  , MCP.toolInputSchema = schemaDocsTool [] []
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      mProject <- resolveDocsProjectFromArgs args state connId
      case mProject of
        Left msg -> pure (errTxt msg)
        Right maybeProject -> do
          localDocs <- maybe (pure []) (localModulesForProject state) maybeProject
          pkgs <- Control.Concurrent.STM.readTVarIO (Client.packages state)
          outline <- traverse (Elm.Outline.read . Ext.Dev.Project.getRoot . projectFromCache) maybeProject
          let body = renderDocsModules maybeProject outline localDocs pkgs
          pure (ok body)
  }

-- docs_project
toolDocsProject :: MCP.Tool
toolDocsProject = MCP.Tool
  { MCP.toolName = "docs_project"
  , MCP.toolDescription = "Show a documentation-oriented overview for a local Elm project without dumping all module docs."
  , MCP.toolInputSchema = schemaDocsTool [] []
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      mProject <- resolveDocsProjectFromArgs args state connId
      case mProject of
        Left msg -> pure (errTxt msg)
        Right Nothing -> pure (errTxt "No project selected")
        Right (Just pc) -> do
          localDocs <- localModulesForProject state pc
          pkgs <- Control.Concurrent.STM.readTVarIO (Client.packages state)
          outline <- Elm.Outline.read (Ext.Dev.Project.getRoot (projectFromCache pc))
          pure (ok (renderDocsProject pc outline localDocs pkgs))
  }

-- install elm package
toolInstall :: MCP.Tool
toolInstall = MCP.Tool
  { MCP.toolName = "install"
  , MCP.toolDescription = "Install an Elm package (author/project)."
  , MCP.toolInputSchema = schemaProjectPlus [("package", "Elm package name, e.g. elm/json")]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> runSingleDependencyInstall "Installed successfully" "Already installed" DependencyManager.Production args state connId
  }


toolDependencies :: MCP.Tool
toolDependencies = MCP.Tool
  { MCP.toolName = "dependencies"
  , MCP.toolDescription = "Plan or apply Elm dependency operations: install, uninstall, uninstall-unused, tree, or upgrade. Tree results group direct and indirect packages, identify unused dependencies, and explain which packages use each indirect dependency."
  , MCP.toolInputSchema =
      JSON.object
        [ "type" .= ("object" :: Text)
        , "additionalProperties" .= False
        , "properties" .= JSON.object
            [ rootDirSchema
            , "operation" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "enum" .= (["install", "uninstall", "uninstall-unused", "tree", "upgrade"] :: [Text])
                ]
            , "scope" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "enum" .= (["production", "test"] :: [Text])
                , "default" .= ("production" :: Text)
                ]
            , "packages" .= JSON.object
                [ "type" .= ("array" :: Text)
                , "description" .= ("Packages. Install accepts author/project, author/project@MAJOR, or author/project@MAJOR.MINOR.PATCH." :: Text)
                , "items" .= JSON.object ["type" .= ("string" :: Text)]
                ]
            , "unsafe" .= JSON.object ["type" .= ("boolean" :: Text), "default" .= False]
            , "allScopes" .= JSON.object ["type" .= ("boolean" :: Text), "default" .= False]
            , "dryRun" .= JSON.object ["type" .= ("boolean" :: Text), "default" .= False]
            ]
        , "required" .= (["operation"] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> runDependencyTool args state connId
  }


runDependencyTool :: JSON.Object -> Client.State -> JSONRPC.ConnectionId -> IO MCP.ToolCallResponse
runDependencyTool args state connId =
  case getStringArg args "operation" of
    Nothing -> pure (errTxt "Missing required string argument: operation")
    Just operation ->
      if not (dependencyArgumentsValid args)
      then pure (errTxt "Invalid dependency arguments. scope must be a string, packages an array of strings, and boolean options must be booleans.")
      else if not (dependencyOperationArgumentsValid operation args)
      then pure (errTxt "One or more arguments are not supported for this dependency operation.")
      else case parseDependencyScope (getStringArg args "scope") of
        Left message -> pure (errTxt message)
        Right scope ->
          do
            selection <- resolveProjectFromDirArg args state connId
            case selection of
                Left message -> pure (errTxt message)
                Right pc@(Client.ProjectCache project _ _ _ _) ->
                  let
                    root = Ext.Dev.Project.getRoot project
                    packageStrings = dependencyPackageStrings args
                    dryRun = Maybe.fromMaybe False (getBoolArg args "dryRun")
                  in
                  case operation of
                    "install" ->
                      case traverse DependencyManager.parsePackageRequirement packageStrings of
                        Nothing -> pure (errTxt "Invalid package requirement. Expected author/project, author/project@MAJOR, or author/project@MAJOR.MINOR.PATCH.")
                        Just [] -> pure (errTxt "Install requires at least one package.")
                        Just requirements -> runDependencyPlan (Text.pack operation) scope state pc root dryRun (DependencyManager.planInstallRequirements root scope requirements)

                    "uninstall" ->
                      case traverse parseBareDependency packageStrings of
                        Nothing -> pure (errTxt "Invalid package name. Expected author/project without a version.")
                        Just [] -> pure (errTxt "Uninstall requires at least one package.")
                        Just packages -> runDependencyPlan (Text.pack operation) scope state pc root dryRun (DependencyManager.planUninstall root scope packages)

                    "uninstall-unused" ->
                      do  unused <- DependencyManager.findUnused root scope
                          case unused of
                            Left problem -> pure (dependencyErrorResponse problem)
                            Right [] -> pure (ok (dependencyUnchangedJson "uninstall-unused" scope))
                            Right packages -> runDependencyPlan (Text.pack operation) scope state pc root dryRun (DependencyManager.planUninstall root scope packages)

                    "upgrade" ->
                      case traverse parseBareDependency packageStrings of
                        Nothing -> pure (errTxt "Invalid package name. Expected author/project without a version.")
                        Just packages ->
                          let
                            targets = if null packages then Nothing else Just packages
                            policy = if Maybe.fromMaybe False (getBoolArg args "unsafe") then DependencyManager.AllowMajor else DependencyManager.Compatible
                            allScopes = Maybe.fromMaybe False (getBoolArg args "allScopes")
                          in
                          runDependencyPlan (Text.pack operation) scope state pc root dryRun (DependencyManager.planUpgrade root scope policy targets allScopes)

                    "tree" ->
                      do  result <- DependencyManager.dependencyTree root scope
                          case result of
                            Left problem -> pure (dependencyErrorResponse problem)
                            Right tree ->
                              case packageStrings of
                                [] -> dependencyTreeResponse root scope tree
                                [packageString] ->
                                  case parseBareDependency packageString of
                                    Nothing -> pure (errTxt "Invalid package tree filter. Expected author/project.")
                                    Just package -> dependencyTreeResponse root scope (DependencyManager.filterDependencyTree package tree)
                                _ -> pure (errTxt "Tree accepts at most one package filter.")

                    _ -> pure (errTxt "Unknown dependency operation.")


parseDependencyScope :: Maybe String -> Either Text DependencyManager.Scope
parseDependencyScope maybeScope =
  case maybeScope of
    Nothing -> Right DependencyManager.Production
    Just "production" -> Right DependencyManager.Production
    Just "test" -> Right DependencyManager.Test
    Just _ -> Left "Invalid scope. Expected production or test."


dependencyPackageStrings :: JSON.Object -> [String]
dependencyPackageStrings args =
  case getArrayArg args "packages" of
    Nothing -> []
    Just values -> [Text.unpack value | JSON.String value <- values]


dependencyArgumentsValid :: JSON.Object -> Bool
dependencyArgumentsValid args =
  onlyKnownFields
    && optionalString "dir"
    && optionalString "scope"
    && optionalStringArray "packages"
    && all optionalBool ["unsafe", "allScopes", "dryRun"]
  where
    onlyKnownFields = all (\key -> Key.toText key `elem` ["dir", "operation", "scope", "packages", "unsafe", "allScopes", "dryRun"]) (KeyMap.keys args)
    optionalString key = fieldMatches key (\value -> case value of JSON.String _ -> True; _ -> False)
    optionalBool key = fieldMatches key (\value -> case value of JSON.Bool _ -> True; _ -> False)
    optionalStringArray key = fieldMatches key (\value -> case value of JSON.Array values -> all isString values; _ -> False)
    isString value = case value of JSON.String _ -> True; _ -> False
    fieldMatches key matches =
      case KeyMap.lookup (Key.fromText key) args of
        Nothing -> True
        Just value -> matches value


dependencyArgumentsValidForTests :: JSON.Object -> Bool
dependencyArgumentsValidForTests = dependencyArgumentsValid


dependencyOperationArgumentsValid :: String -> JSON.Object -> Bool
dependencyOperationArgumentsValid operation args =
  case operation of
    "install" -> absent "unsafe" && absent "allScopes"
    "uninstall" -> absent "unsafe" && absent "allScopes"
    "uninstall-unused" -> absent "packages" && absent "unsafe" && absent "allScopes"
    "upgrade" -> True
    "tree" -> absent "dryRun" && absent "unsafe" && absent "allScopes"
    _ -> True
  where
    absent key = not (KeyMap.member (Key.fromText key) args)


dependencyOperationArgumentsValidForTests :: String -> JSON.Object -> Bool
dependencyOperationArgumentsValidForTests = dependencyOperationArgumentsValid


parseBareDependency :: String -> Maybe Pkg.Name
parseBareDependency raw =
  if '@' `elem` raw
  then Nothing
  else case DependencyManager.parsePackageRequirement raw of
    Just (DependencyManager.PackageRequirement package DependencyManager.Latest) -> Just package
    _ -> Nothing


runDependencyPlan :: Text -> DependencyManager.Scope -> Client.State -> Client.ProjectCache -> FilePath -> Bool -> IO (Either DependencyManager.Error DependencyManager.Plan) -> IO MCP.ToolCallResponse
runDependencyPlan operation scope state pc@(Client.ProjectCache _ _ _ _ testVar) root dryRun planning =
  do  planned <- planning
      case planned of
        Left problem -> pure (dependencyErrorResponse problem)
        Right plan ->
          if dryRun || not (DependencyManager.planChanged plan)
          then pure (ok (dependencyPlanJson operation scope (if not (DependencyManager.planChanged plan) then "unchanged" else "planned") False plan))
          else do
            applied <- DependencyManager.applyPlan root plan
            case applied of
              Left problem -> pure (dependencyErrorResponse problem)
              Right () ->
                do  refreshDependencyProject state pc root testVar
                    pure (ok (dependencyPlanJson operation scope "applied" True plan))


refreshDependencyProject :: Client.State -> Client.ProjectCache -> FilePath -> Control.Concurrent.STM.TVar (Maybe Client.TestInfo) -> IO ()
refreshDependencyProject state pc root testVar =
  do  _ <- Versions.bumpFsVersion root
      Control.Concurrent.STM.atomically (Watchtower.State.Compile.clearTestResults testVar)
      _ <- Watchtower.State.Compile.ensureProjectFresh state "mcp.dependencies" pc
      pure ()


dependencyPlanJson :: Text -> DependencyManager.Scope -> Text -> Bool -> DependencyManager.Plan -> Text
dependencyPlanJson operation scope status written plan =
  jsonText $
    JSON.object
      [ "operation" .= operation
      , "scope" .= dependencyScopeText scope
      , "status" .= status
      , "written" .= written
      , "changes" .= map dependencyChangeValue (DependencyManager.changes plan)
      ]


dependencyPlanJsonForTests :: Text -> DependencyManager.Scope -> Text -> Bool -> DependencyManager.Plan -> Text
dependencyPlanJsonForTests = dependencyPlanJson


dependencyChangeValue :: DependencyManager.DependencyChange -> JSON.Value
dependencyChangeValue change =
  case change of
    DependencyManager.DependencyAdded scope package value -> dependencyChangeObject "added" scope package Nothing (Just value)
    DependencyManager.DependencyRemoved scope package value -> dependencyChangeObject "removed" scope package (Just value) Nothing
    DependencyManager.DependencyChanged scope package old new -> dependencyChangeObject "changed" scope package (Just old) (Just new)
    DependencyManager.DependencyMoved oldScope newScope package old new ->
      JSON.object
        [ "kind" .= ("moved" :: Text)
        , "package" .= Pkg.toChars package
        , "fromScope" .= dependencyScopeText oldScope
        , "scope" .= dependencyScopeText newScope
        , "from" .= dependencyValueText old
        , "to" .= dependencyValueText new
        ]
    DependencyManager.DependencyReclassified scope package oldKind newKind value ->
      JSON.object
        [ "kind" .= ("reclassified" :: Text)
        , "package" .= Pkg.toChars package
        , "scope" .= dependencyScopeText scope
        , "from" .= dependencyKindText oldKind
        , "to" .= dependencyKindText newKind
        , "version" .= dependencyValueText value
        ]


dependencyChangeObject :: Text -> DependencyManager.Scope -> Pkg.Name -> Maybe DependencyManager.DependencyValue -> Maybe DependencyManager.DependencyValue -> JSON.Value
dependencyChangeObject kind scope package old new =
  JSON.object $
    [ "kind" .= kind
    , "package" .= Pkg.toChars package
    , "scope" .= dependencyScopeText scope
    ]
    ++ maybe [] (\value -> ["from" .= dependencyValueText value]) old
    ++ maybe [] (\value -> ["to" .= dependencyValueText value]) new


dependencyTreeResponse :: FilePath -> DependencyManager.Scope -> DependencyManager.DependencyTree -> IO MCP.ToolCallResponse
dependencyTreeResponse root scope tree =
  do  unusedResult <- DependencyManager.findUnused root scope
      case unusedResult of
        Left problem -> pure (dependencyErrorResponse problem)
        Right unused -> pure (ok (dependencyTreeJson (Set.fromList unused) tree))


dependencyTreeJson :: Set.Set Pkg.Name -> DependencyManager.DependencyTree -> Text
dependencyTreeJson unused tree =
  let
    nodes = Map.fromList (map (\node -> (DependencyManager.treePackage node, node)) (DependencyManager.treeNodes tree))
    direct = filter (dependencyTreeRootNode (DependencyManager.treeScope tree)) (DependencyManager.treeNodes tree)
    indirect = filter ((== DependencyManager.Indirect) . DependencyManager.treeKind) (DependencyManager.treeNodes tree)
  in
  jsonText $
    JSON.object
      [ "root" .= DependencyManager.treeRoot tree
      , "scope" .= dependencyScopeText (DependencyManager.treeScope tree)
      , "direct" .= map (dependencyTreeNodeValue nodes unused) direct
      , "indirect" .= map (dependencyTreeNodeValue nodes Set.empty) indirect
      ]


dependencyTreeJsonForTests :: Set.Set Pkg.Name -> DependencyManager.DependencyTree -> Text
dependencyTreeJsonForTests = dependencyTreeJson


dependencyTreeRootNode :: DependencyManager.Scope -> DependencyManager.TreeNode -> Bool
dependencyTreeRootNode scope node =
  DependencyManager.treeKind node == DependencyManager.ProductionRoot
    || (scope == DependencyManager.Test && DependencyManager.treeKind node == DependencyManager.TestRoot)


dependencyTreeNodeValue :: Map.Map Pkg.Name DependencyManager.TreeNode -> Set.Set Pkg.Name -> DependencyManager.TreeNode -> JSON.Value
dependencyTreeNodeValue nodes unused node =
  JSON.object
    [ "package" .= Pkg.toChars (DependencyManager.treePackage node)
    , "version" .= V.toChars (DependencyManager.treeVersion node)
    , "kind" .= dependencyTreeKindText (DependencyManager.treeKind node)
    , "unused" .= Set.member (DependencyManager.treePackage node) unused
    , "usedBy" .= dependencyTreeUsedBy nodes node
    , "dependencies" .= map (dependencyTreeReferenceValue nodes) (DependencyManager.treeDependencies node)
    ]


dependencyTreeReferenceValue :: Map.Map Pkg.Name DependencyManager.TreeNode -> Pkg.Name -> JSON.Value
dependencyTreeReferenceValue nodes package =
  case Map.lookup package nodes of
    Nothing -> JSON.object ["package" .= Pkg.toChars package]
    Just node ->
      JSON.object
        [ "package" .= Pkg.toChars package
        , "version" .= V.toChars (DependencyManager.treeVersion node)
        , "kind" .= dependencyTreeKindText (DependencyManager.treeKind node)
        ]


dependencyTreeUsedBy :: Map.Map Pkg.Name DependencyManager.TreeNode -> DependencyManager.TreeNode -> [String]
dependencyTreeUsedBy nodes node =
  if DependencyManager.treeKind node /= DependencyManager.Indirect
  then []
  else
    [ Pkg.toChars (DependencyManager.treePackage parent)
    | parent <- Map.elems nodes
    , DependencyManager.treePackage node `elem` DependencyManager.treeDependencies parent
    ]


dependencyUnchangedJson :: Text -> DependencyManager.Scope -> Text
dependencyUnchangedJson operation scope =
  jsonText (JSON.object ["operation" .= operation, "scope" .= dependencyScopeText scope, "status" .= ("unchanged" :: Text), "written" .= False, "changes" .= ([] :: [JSON.Value])])


dependencyErrorResponse :: DependencyManager.Error -> MCP.ToolCallResponse
dependencyErrorResponse problem =
  errTxt (jsonText (JSON.object ["status" .= ("error" :: Text), "error" .= dependencyErrorCode problem]))


dependencyErrorCode :: DependencyManager.Error -> Text
dependencyErrorCode problem =
  case problem of
    DependencyManager.NoOutline -> "no-outline"
    DependencyManager.RegistryProblem _ -> "registry"
    DependencyManager.OutlineProblem _ -> "elm-json"
    DependencyManager.NoSolution -> "no-solution"
    DependencyManager.NoOfflineSolution -> "no-offline-solution"
    DependencyManager.SolverProblem _ -> "solver"
    DependencyManager.VerificationProblem _ -> "verification"
    DependencyManager.TestRefreshProblem _ -> "test-refresh"
    DependencyManager.AnalysisProblem _ -> "analysis"
    DependencyManager.RequiredDependency package -> Text.pack ("required-dependency:" ++ Pkg.toChars package)
    DependencyManager.UnsupportedPackageUpgrade -> "unsupported-package-upgrade"
    DependencyManager.UnsavedChanges -> "unsaved-changes"
    DependencyManager.StalePlan -> "stale-plan"
    DependencyManager.DuplicateRequirement package -> Text.pack ("duplicate-requirement:" ++ Pkg.toChars package)
    DependencyManager.UnsupportedPackageExactRequirement package -> Text.pack ("unsupported-package-exact-requirement:" ++ Pkg.toChars package)


dependencyScopeText :: DependencyManager.Scope -> Text
dependencyScopeText scope =
  case scope of
    DependencyManager.Production -> "production"
    DependencyManager.Test -> "test"


dependencyKindText :: DependencyManager.DependencyKind -> Text
dependencyKindText kind =
  case kind of
    DependencyManager.DirectDependency -> "direct"
    DependencyManager.IndirectDependency -> "indirect"


dependencyTreeKindText :: DependencyManager.TreeKind -> Text
dependencyTreeKindText kind =
  case kind of
    DependencyManager.ProductionRoot -> "production"
    DependencyManager.TestRoot -> "test"
    DependencyManager.Indirect -> "indirect"


dependencyValueText :: DependencyManager.DependencyValue -> Text
dependencyValueText value =
  case value of
    DependencyManager.AppVersion version -> Text.pack (V.toChars version)
    DependencyManager.PackageConstraint constraint -> Text.pack (Con.toChars constraint)


jsonText :: JSON.Value -> Text
jsonText = Data.Text.Encoding.decodeUtf8 . LBS.toStrict . JSON.encode

-- add_page
toolAdd :: MCP.Tool
toolAdd = MCP.Tool
  { MCP.toolName = "add"
  , MCP.toolDescription = "Add multiple elm-dev artifacts in one call. additions is an array of tagged unions: page, store, effect, listener, theme."
  , MCP.toolInputSchema =
      JSON.object
        [ "type" .= ("object" :: Text)
        , "properties" .= JSON.object
            [ Key.fromText "dir" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Optional local Elm project directory to use instead of the focused project" :: Text)
                ]
            , Key.fromText "additions" .= JSON.object
                [ "type" .= ("array" :: Text)
                , "description" .= ("Tagged additions: { kind: \"page\", url }, { kind: \"store\", module }, { kind: \"effect\", module }, { kind: \"listener\", module }, { kind: \"theme\" }" :: Text)
                , "items" .= JSON.object [ "type" .= ("object" :: Text) ]
                ]
            ]
        , "required" .= (["additions"] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      case getArrayArg args "additions" of
        Nothing -> pure (errTxt "Missing additions array.")
        Just additions -> do
          selection <- resolveProjectFromDirArg args state connId
          case selection of
            Left msg -> pure (errTxt msg)
            Right pc -> do
              blocks <- mapM (runOneAdd pc) (zip [(0 :: Int)..] additions)
              pure (ok (Text.intercalate "\n\n" blocks))
  }

runOneAdd :: Client.ProjectCache -> (Int, JSON.Value) -> IO Text
runOneAdd (Client.ProjectCache proj _ _ _ _) (index, value) =
  case value of
    JSON.Object addition -> do
      let dir = Ext.Dev.Project.getRoot proj
      case stringField addition "kind" of
        Just "page" ->
          case stringField addition "url" of
            Nothing -> pure (addError index "page" [] "Missing url.")
            Just url -> withDir dir $ do
              cfg <- Gen.Generate.readConfigOrFail dir
              let name = urlToElmModuleName (Text.unpack url)
              Templates.write "Page" Config.elmSrc name
              let updated = case Config.configPages cfg of
                    Nothing -> cfg { Config.configPages = Just (Map.singleton (Text.pack name) (Config.PageConfig url [] False)) }
                    Just pagesMap -> cfg { Config.configPages = Just (Map.insert (Text.pack name) (Config.PageConfig url [] False) pagesMap) }
              LBS.writeFile "elm.dev.json" (Aeson.encodePretty updated)
              pure (xmlBlock "add" ([("index", Text.pack (show index)), ("kind", "page"), ("url", url), ("module", Text.pack name), ("status", "ok")] ++ [("root", Text.pack dir)]) (Text.pack ("Created page " ++ name)))
        Just "store" ->
          runModuleAdd index dir "store" "Store" Nothing addition
        Just "effect" ->
          runModuleAdd index dir "effect" "Effect" (Just ("effect", Config.effectTsSrc)) addition
        Just "listener" ->
          runModuleAdd index dir "listener" "Listen" Nothing addition
        Just "theme" -> withDir dir $ do
          cfg <- Gen.Generate.readConfigOrFail dir
          case Config.configTheme cfg of
            Just _ -> pure (addError index "theme" [] "Theme already exists")
            Nothing -> do
              let updated = cfg { Config.configTheme = Nothing }
              LBS.writeFile "elm.dev.json" (Aeson.encodePretty updated)
              pure (xmlBlock "add" [("index", Text.pack (show index)), ("kind", "theme"), ("status", "ok"), ("root", Text.pack dir)] "Added theme")
        Just other -> pure (addError index other [] "Unknown addition kind.")
        Nothing -> pure (addError index "unknown" [] "Missing addition.kind.")
    _ -> pure (addError index "unknown" [] "Expected addition object.")

runModuleAdd :: Int -> FilePath -> Text -> String -> Maybe (String, FilePath) -> JSON.Object -> IO Text
runModuleAdd index dir kind template maybeTs addition =
  case stringField addition "module" of
    Nothing -> pure (addError index kind [] "Missing module.")
    Just moduleName -> withDir dir $ do
      _ <- Gen.Generate.readConfigOrFail dir
      Templates.write template Config.elmSrc (Text.unpack moduleName)
      case maybeTs of
        Nothing -> pure ()
        Just (tsTemplate, tsSrc) -> Templates.writeTs tsTemplate tsSrc (Text.unpack moduleName)
      pure (xmlBlock "add" [("index", Text.pack (show index)), ("kind", kind), ("module", moduleName), ("status", "ok"), ("root", Text.pack dir)] ("Created " <> kind <> " " <> moduleName))

addError :: Int -> Text -> [(Text, Text)] -> Text -> Text
addError index kind extraAttrs msg =
  xmlBlock "add" ([("index", Text.pack (show index)), ("kind", kind), ("status", "error")] ++ extraAttrs) msg

toolAddPage :: MCP.Tool
toolAddPage = MCP.Tool
  { MCP.toolName = "add_page"
  , MCP.toolDescription = "Add a new page to the project."
  , MCP.toolInputSchema = schemaProjectPlus [("url", "Page url path, e.g. /about")]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "url" args of
        Left e -> pure (errTxt (Text.pack e))
        Right url -> do
          selection <- ProjectLookup.resolveProjectFromSession Nothing connId state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              withDir dir $ do
                cfg <- Gen.Generate.readConfigOrFail dir
                let name = urlToElmModuleName url
                Templates.write "Page" Config.elmSrc name
                let urlText = Text.pack url
                let updated = case Config.configPages cfg of
                      Nothing -> cfg { Config.configPages = Just (Map.singleton (Text.pack name) (Config.PageConfig urlText [] False)) }
                      Just pagesMap -> cfg { Config.configPages = Just (Map.insert (Text.pack name) (Config.PageConfig urlText [] False) pagesMap) }
                LBS.writeFile "elm.dev.json" (Aeson.encodePretty updated)
                pure (ok (Text.pack ("Created page " ++ name)))
  }

-- add_store
toolAddStore :: MCP.Tool
toolAddStore = MCP.Tool
  { MCP.toolName = "add_store"
  , MCP.toolDescription = "Add a new store module."
  , MCP.toolInputSchema = schemaProjectPlus [("module", "Module name, e.g. App.Store.Session")]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "module" args of
        Left e -> pure (errTxt (Text.pack e))
        Right modul -> do
          selection <- ProjectLookup.resolveProjectFromSession Nothing connId state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              withDir dir $ do
                _ <- Gen.Generate.readConfigOrFail dir
                Templates.write "Store" Config.elmSrc modul
                pure (ok (Text.pack ("Created store " ++ modul)))
  }

-- add_effect
toolAddEffect :: MCP.Tool
toolAddEffect = MCP.Tool
  { MCP.toolName = "add_effect"
  , MCP.toolDescription = "Add a new effect module and TS interop."
  , MCP.toolInputSchema = schemaProjectPlus [("module", "Module name, e.g. Effect.Http")]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "module" args of
        Left e -> pure (errTxt (Text.pack e))
        Right modul -> do
          selection <- ProjectLookup.resolveProjectFromSession Nothing connId state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              withDir dir $ do
                _ <- Gen.Generate.readConfigOrFail dir
                Templates.write "Effect" Config.elmSrc modul
                Templates.writeTs "effect" Config.effectTsSrc modul
                pure (ok (Text.pack ("Created effect " ++ modul)))
  }

-- add_listener
toolAddListener :: MCP.Tool
toolAddListener = MCP.Tool
  { MCP.toolName = "add_listener"
  , MCP.toolDescription = "Add a new listener module."
  , MCP.toolInputSchema = schemaProjectPlus [("module", "Module name, e.g. Listen.Resize")]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "module" args of
        Left e -> pure (errTxt (Text.pack e))
        Right modul -> do
          selection <- ProjectLookup.resolveProjectFromSession Nothing connId state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              withDir dir $ do
                _ <- Gen.Generate.readConfigOrFail dir
                Templates.write "Listen" Config.elmSrc modul
                pure (ok (Text.pack ("Created listener " ++ modul)))
  }

-- add_theme
toolAddTheme :: MCP.Tool
toolAddTheme = MCP.Tool
  { MCP.toolName = "add_theme"
  , MCP.toolDescription = "Add a theme section to elm.dev.json if not present."
  , MCP.toolInputSchema = schemaProject
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      selection <- ProjectLookup.resolveProjectFromSession Nothing connId state
      case selection of
        Left msg -> pure (errTxt msg)
        Right (Client.ProjectCache proj _ _ _ _) -> do
          let dir = Ext.Dev.Project.getRoot proj
          withDir dir $ do
            cfg <- Gen.Generate.readConfigOrFail dir
            case Config.configTheme cfg of
              Just _ -> pure (errTxt "Theme already exists")
              Nothing -> do
                let updated = cfg { Config.configTheme = Nothing }
                LBS.writeFile "elm.dev.json" (Aeson.encodePretty updated)
                pure (ok "Added theme")
  }

-- test_install
toolTestInstall :: MCP.Tool
toolTestInstall = MCP.Tool
  { MCP.toolName = "test_install"
  , MCP.toolDescription = "Install an Elm test dependency (author/project) into test-dependencies."
  , MCP.toolInputSchema = schemaProjectPlus [("package", "Elm package name, e.g. elm-explorations/test")]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> runSingleDependencyInstall "Installed successfully" "Already installed" DependencyManager.Test args state connId
  }


runSingleDependencyInstall :: Text -> Text -> DependencyManager.Scope -> JSON.Object -> Client.State -> JSONRPC.ConnectionId -> IO MCP.ToolCallResponse
runSingleDependencyInstall successMessage unchangedMessage scope args state connId =
  case requireStringArg "package" args of
    Left message -> pure (errTxt (Text.pack message))
    Right raw ->
      case DependencyManager.parsePackageRequirement raw of
        Nothing -> pure (errTxt "Invalid package requirement. Expected author/project, author/project@MAJOR, or author/project@MAJOR.MINOR.PATCH.")
        Just requirement@(DependencyManager.PackageRequirement package _) ->
          do  selection <- resolveProjectFromDirArg args state connId
              case selection of
                Left message -> pure (errTxt message)
                Right pc@(Client.ProjectCache project _ _ _ testVar) ->
                  let root = Ext.Dev.Project.getRoot project
                  in do
                    planned <- DependencyManager.planInstallRequirements root scope [requirement]
                    case planned of
                      Left problem -> pure (dependencyErrorResponse problem)
                      Right plan ->
                        if not (DependencyManager.planChanged plan)
                        then pure (legacyInstallResponse unchangedMessage package)
                        else do
                          applied <- DependencyManager.applyPlan root plan
                          case applied of
                            Left problem -> pure (dependencyErrorResponse problem)
                            Right () ->
                              do  refreshDependencyProject state pc root testVar
                                  pure (legacyInstallResponse successMessage package)


legacyInstallResponse :: Text -> Pkg.Name -> MCP.ToolCallResponse
legacyInstallResponse message package =
  let
    linkInfo =
      MCP.ResourceLinkInfo
        { MCP.resourceLinkUri = Text.concat ["elm://docs/package/", Text.pack (Pkg.toUrl package)]
        , MCP.resourceLinkName = Text.concat [Text.pack (Pkg.toChars package), " docs"]
        , MCP.resourceLinkDescription = Just "Open package docs rendered by server"
        , MCP.resourceLinkMimeType = Nothing
        }
  in
  MCP.ToolCallResponse [MCP.ToolResponseText Nothing message, MCP.ToolResponseResourceLink Nothing linkInfo]


legacyInstallResponseForTests :: Text -> Pkg.Name -> MCP.ToolCallResponse
legacyInstallResponseForTests = legacyInstallResponse

-- 
toolTestRun :: MCP.Tool
toolTestRun = MCP.Tool
  { MCP.toolName = "test_run"
  , MCP.toolDescription = "Discover, compile, and run Elm tests."
  , MCP.toolInputSchema =
      JSON.object
        [ "type" .= ("object" :: Text)
        , "properties" .= JSON.object
            [ "timeoutSeconds" .= JSON.object
                [ "type" .= ("integer" :: Text)
                , "description" .= ("Optional max seconds to wait for tests (default 10s)" :: Text)
                ]
            , "glob" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "description"
                    .= ( "Optional glob to filter which tests run. "
                       <> "Matches against fully-qualified test IDs like \"Module.value\". "
                       <> "Supports '*' (zero or more chars) and '?' (exactly one char). "
                       <> "Examples: \"My.Module.*\", \"Tests.*.foo\", \"*Bar\"."
                       :: Text
                       )
                ]
            , "seed" .= JSON.object
                [ "type" .= ("integer" :: Text)
                , "description" .= ("Optional random seed for fuzz tests" :: Text)
                ]
            , "fuzz" .= JSON.object
                [ "type" .= ("integer" :: Text)
                , "description" .= ("Optional number of fuzz runs per test (default 100)" :: Text)
                ]
            ]
        , "required" .= ([] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      selection <- ProjectLookup.resolveProjectFromSession Nothing connId state
      case selection of
        Left msg -> pure (errTxt msg)
        Right (Client.ProjectCache proj _ _ _ mTestVar) -> do
          let dir = Ext.Dev.Project.getRoot proj
          Ext.Log.log Ext.Log.Test ("Running tests in project: " ++ dir)
          let mTimeoutSeconds =
                case KeyMap.lookup "timeoutSeconds" args of
                  Just (JSON.Number n) -> (Scientific.toBoundedInteger n :: Maybe Int)
                  _ -> Nothing
          let mGlobString =
                case KeyMap.lookup "glob" args of
                  Just (JSON.String t) -> Just (Text.unpack t)
                  _ -> Nothing
          let mGlobs = fmap (\g -> [g]) mGlobString
          let mSeed =
                case KeyMap.lookup "seed" args of
                  Just (JSON.Number n) -> (Scientific.toBoundedInteger n :: Maybe Int)
                  _ -> Nothing
          let mFuzz =
                case KeyMap.lookup "fuzz" args of
                  Just (JSON.Number n) -> (Scientific.toBoundedInteger n :: Maybe Int)
                  _ -> Nothing
          let timeoutSeconds = maybe 10 id mTimeoutSeconds
          startPs <- CPUTime.getCPUTime
          result <- TestRunner.run (Just timeoutSeconds) mGlobs mSeed mFuzz dir
          case result of
            Left e -> case e of
              TestRunner.TimedOut _ -> do
                let timeoutMs = timeoutSeconds * 1000
                pure (errTxt (Text.pack ("Tests timed out after " ++ show timeoutMs ++ "ms")))
              TestRunner.RunFailed msg ->
                pure (errTxt (Text.pack msg))
            Right (TestRunner.RunSuccess info reports seed fuzz) -> do
              endPs <- CPUTime.getCPUTime
              let durationMs :: Int
                  durationMs = fromInteger ((endPs - startPs) `div` 1000000000)
              let rendered = TestReport.renderReportsWithDuration False (Just durationMs) (Just seed) (Just fuzz) reports
              -- Update test var with info from the test run
              let summary = TestRunner.tiSummary info
                  total = TestRunner.summaryTotal summary
                  passed = TestRunner.summaryPassed summary
                  failed = TestRunner.summaryFailed summary
                  failures = TestRunner.summaryFailures summary
                  tr = Client.TestResults total passed failed failures
                  clientInfo = Client.TestInfo
                                 { Client.testFiles = TestRunner.tiFiles info
                                 , Client.testResults = Just tr
                                 , Client.testCompilation = Just Client.TestSuccess
                                 }
              Control.Concurrent.STM.atomically $ Control.Concurrent.STM.writeTVar mTestVar (Just clientInfo)
              Ext.Log.log Ext.Log.Test rendered
              pure (ok (Text.pack rendered))
  }


-- select current project by root (discover if needed)
toolProjectSelect :: MCP.Tool
toolProjectSelect = MCP.Tool
  { MCP.toolName = "project_select"
  , MCP.toolDescription = "Rarely needed. Select the current project for this connection by root directory. Most tools auto-select a project; only call this when multiple projects exist and automatic selection is not what you want."
  , MCP.toolInputSchema =
      JSON.object
        [ "type" .= ("object" :: Text)
        , "properties" .= JSON.object
            [ "root" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Absolute path to the project root" :: Text)
                ]
            ]
        , "required" .= (["root"] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "root" args of
        Left e -> pure (errTxt (Text.pack e))
        Right rootDir -> do
          canonicalRoot <- Dir.canonicalizePath rootDir
          let (Client.State _ mProjects _ _ _ _ _ _ _) = state
          projects0 <- Control.Concurrent.STM.readTVarIO mProjects
          case Client.findByRoot canonicalRoot projects0 of
            Just (Client.ProjectCache proj _ _ mCr _) -> do
              let pid = Ext.Dev.Project._shortId proj
              okSet <- Client.setFocusedProjectId state connId pid
              if okSet
                then do
                  Watchtower.Server.DevWS.broadcastServiceStatus state
                  currentResult <- Control.Concurrent.STM.readTVarIO mCr
                  let (compStatus :: Text, compMsg :: Maybe Text) =
                        case currentResult of
                          Client.Success _ -> ("ok", Nothing)
                          Client.NotCompiled -> ("compiling", Nothing)
                          Client.Error (Client.GenerationError errMsg) -> ("generationError", Just (Text.pack errMsg))
                          Client.Error (Client.ReactorError reactor) ->
                            case reactor of
                              Exit.ReactorBadBuild _ -> ("errors", Nothing)
                              _ -> ("error", Nothing)
                          Client.TargetResults results ->
                            if all (Client.compilationResultSucceeded . snd) results
                              then ("ok", Nothing)
                              else ("errors", Nothing)
                  let linesOut =
                        [ Text.concat ["Focused project set to root: ", Text.pack canonicalRoot]
                        , Text.concat ["Compilation status: ", compStatus]
                        ] ++ maybe [] (\m -> ["Message: " <> m]) compMsg
                  pure (ok (Text.intercalate "\n" linesOut))
                else pure (errTxt "Unable to set focused project")
            Nothing -> do
              -- Not found, try discovery at this root
              Watchtower.State.Discover.discover state canonicalRoot
              projects1 <- Control.Concurrent.STM.readTVarIO mProjects
              case Client.findByRoot canonicalRoot projects1 of
                Just (Client.ProjectCache proj _ _ mCr _) -> do
                  let pid = Ext.Dev.Project._shortId proj
                  okSet <- Client.setFocusedProjectId state connId pid
                  if okSet
                    then do
                      Watchtower.Server.DevWS.broadcastServiceStatus state
                      currentResult <- Control.Concurrent.STM.readTVarIO mCr
                      let (compStatus :: Text, compMsg :: Maybe Text) =
                            case currentResult of
                              Client.Success _ -> ("ok", Nothing)
                              Client.NotCompiled -> ("compiling", Nothing)
                              Client.Error (Client.GenerationError errMsg) -> ("generationError", Just (Text.pack errMsg))
                              Client.Error (Client.ReactorError reactor) ->
                                case reactor of
                                  Exit.ReactorBadBuild _ -> ("errors", Nothing)
                                  _ -> ("error", Nothing)
                              Client.TargetResults results ->
                                if all (Client.compilationResultSucceeded . snd) results
                                  then ("ok", Nothing)
                                  else ("errors", Nothing)
                      let linesOut =
                            [ Text.concat ["Focused project set to root: ", Text.pack canonicalRoot]
                            , Text.concat ["Compilation status: ", compStatus]
                            ] ++ maybe [] (\m -> ["Message: " <> m]) compMsg
                      pure (ok (Text.intercalate "\n" linesOut))
                    else pure (errTxt "Unable to set focused project")
                Nothing -> do
                  let known = ProjectLookup.listKnownProjectsText projects1
                  pure (errTxt (Text.concat ["No Elm project found at root: ", Text.pack canonicalRoot, "\nKnown projects:\n", known]))
  }

-- url -> Elm module name
urlToElmModuleName :: String -> String
urlToElmModuleName url =
  let base = takeWhile (/= '?') url
      parts = filter (not . null) $ splitOn '/' base
      valid = map capitalize $ filter (\p -> case p of { ':' : _ -> False; _ -> True }) parts
  in concat valid
  where
    splitOn :: Char -> String -> [String]
    splitOn c = words . map (\x -> if x == c then ' ' else x)
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs

-- * Available Resources

availableResources :: [MCP.Resource]
availableResources =
  [ resourceOverview
  , resourceProjectList
  , resourceArchitecture
  , resourceWellFormedElmCode
  -- , resourceDiagnostics
  , resourceFileDocs
  , resourceModuleGraph
  , resourcePackageDocs
  , resourceModuleDocs
  , resourceValueDocs
  ]

-- Toplevel resource values with inline readers

resourceOverview :: MCP.Resource
resourceOverview =
  let pat = Uri.pattern "elm" [Uri.s "overview"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm Dev Project Overview"
      , MCP.resourceDescription = Just "Overview for an Elm project. Examples: elm://overview, file://architecture"
      , MCP.resourceMimeType = Just "text/markdown"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceAssistant] MCP.High Nothing)
      , MCP.read = \req state _emit connId -> do
          projectFound <- ProjectLookup.resolveProjectFromSession Nothing connId state
          case projectFound of
            Left msg -> do
              -- Build project list for frontmatter even when no project is selected
              let (Client.State _ mProjects _ _ _ _ _ _ _) = state
              projects <- Control.Concurrent.STM.readTVarIO mProjects
              mFocused <- Client.getFocusedProjectId state connId

              items <- mapM
                (\(Client.ProjectCache p _ _ mCr _) -> do
                    cr <- Control.Concurrent.STM.readTVarIO mCr
                    let compiling = case cr of
                                      Client.NotCompiled -> True
                                      _ -> False
                        pid = Ext.Dev.Project._shortId p
                        root = Ext.Dev.Project.getRoot p
                        isCurrent = maybe False (== pid) mFocused
                    pure (pid, root, compiling, isCurrent)
                )
                projects

              let frontmatter :: [Text]
                  frontmatter =
                    [ "---"
                    , "title: Elm Project Overview"
                    , "kind: overview"
                    , "language: elm"
                    , Text.concat ["error: ", msg]
                    ]
                    ++ ["projectList:", "  projects:"]
                    ++ concatMap
                        (\(pid, root, compiling, isCurrent) ->
                           let base =
                                 [ Text.concat ["  - id: ", Text.pack (show pid)]
                                 , Text.concat ["    root: ", Text.pack root]
                                 , Text.concat ["    compiling: ", if compiling then "true" else "false"]
                                 ]
                               currentLine = if isCurrent then ["    focused: true"] else []
                           in base ++ currentLine
                        )
                        items
                    ++ ["---"]
                  body :: Text
                  body = Text.intercalate "\n" frontmatter
              pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
            Right pc@(Client.ProjectCache proj _ _ mCompileResult mTest) -> do
              -- For MCP: ensure disk, VFS, and compile state are current before reporting status.
              _ <- Watchtower.State.Compile.ensureProjectFresh state "mcp.project_overview" pc
              currentResult <- Control.Concurrent.STM.readTVarIO mCompileResult

              -- Determine compilation status text (for YAML)
              let (compStatus :: Text, compMsgLine :: [Text]) = case currentResult of
                    Client.Success _ -> ("ok", [])
                    Client.NotCompiled -> ("compiling", [])
                    Client.Error (Client.GenerationError errMsg) -> ("generationError", [ Text.concat ["  message: ", Text.pack errMsg] ])
                    Client.Error (Client.ReactorError reactor) -> case reactor of
                      Exit.ReactorBadBuild _ -> ("errors", [])
                      _ -> ("error", [])
                    Client.TargetResults results ->
                      if all (Client.compilationResultSucceeded . snd) results
                        then ("ok", [])
                        else ("errors", [])

              -- Test summary (if any)
              mTests <- Control.Concurrent.STM.readTVarIO mTest
              let testYaml :: [Text]
                  testYaml = case mTests >>= Client.testResults of
                    Nothing -> []
                    Just (Client.TestResults total passed failed _failures) ->
                      [ "tests:"
                      , Text.concat ["  total: ", Text.pack (show total)]
                      , Text.concat ["  passed: ", Text.pack (show passed)]
                      , Text.concat ["  failed: ", Text.pack (show failed)]
                      ]

              -- Architecture note if this is an elm-dev project
              isElmDev <- Ext.FileProxy.exists (Ext.Dev.Project.getRoot proj </> "elm.dev.json")
              let archNoteLine = if isElmDev then
                                   ["note: This project is using the Elm Dev App Architecture. Read the `file://architecture` resource for further details."]
                                 else []

              -- Build project list for frontmatter
              let (Client.State _ mProjects _ _ _ _ _ _ _) = state
              projects <- Control.Concurrent.STM.readTVarIO mProjects
              mFocused <- Client.getFocusedProjectId state connId
              items <- mapM
                (\(Client.ProjectCache p _ _ mCr _) -> do
                    cr <- Control.Concurrent.STM.readTVarIO mCr
                    let compiling = case cr of
                                      Client.NotCompiled -> True
                                      _ -> False
                        pid = Ext.Dev.Project._shortId p
                        root = Ext.Dev.Project.getRoot p
                        isCurrent = maybe False (== pid) mFocused
                    pure (pid, root, compiling, isCurrent)
                )
                projects

              let entrypoints :: [Text]
                  entrypoints = fmap Text.pack (NE.toList (Ext.Dev.Project._entrypoints proj))

              let frontmatter :: [Text]
                  frontmatter =
                    [ "---"
                    , "title: Elm Dev Project Overview"
                    , "kind: overview"
                    , "language: elm"
                    , Text.concat ["projectId: ", Text.pack (show (Ext.Dev.Project._shortId proj))]
                    , Text.concat ["root: ", Text.pack (Ext.Dev.Project.getRoot proj)]
                    , "entrypoints:"
                    ]
                    ++ fmap (\e -> Text.concat ["  - ", e]) entrypoints
                    ++ [ "compilation:"
                       , Text.concat ["  status: ", compStatus]
                       ]
                    ++ compMsgLine
                    ++ testYaml
                    ++ archNoteLine
                    ++ ["projectList:", "  projects:"]
                    ++ concatMap
                        (\(pid, root, compiling, isCurrent) ->
                           let base =
                                 [ Text.concat ["  - id: ", Text.pack (show pid)]
                                 , Text.concat ["    root: ", Text.pack root]
                                 , Text.concat ["    compiling: ", if compiling then "true" else "false"]
                                 ]
                               currentLine = if isCurrent then ["    focused: true"] else []
                           in base ++ currentLine
                        )
                        items
                    ++ ["---"]
                  body :: Text
                  body = Text.intercalate "\n" frontmatter
              pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
      }

resourceArchitecture :: MCP.Resource
resourceArchitecture =
  let pat = Uri.pattern "file" [Uri.s "architecture"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm Project Architecture"
      , MCP.resourceDescription = Just "Describes how this specific Elm project is structured (important information for understanding the project)"
      , MCP.resourceMimeType = Just "text/markdown"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceAssistant] MCP.High Nothing)
      , MCP.read = \req state _emit connId -> do
          projectFound <- ProjectLookup.resolveProjectFromSession Nothing connId state
          case projectFound of
            Left _ -> do
              let msg = ("This is a standard Elm app using the Elm Architecture." :: Text)
              pure (MCP.ReadResourceResponse [ MCP.markdown req msg ])
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let cfgPath = Ext.Dev.Project.getRoot proj </> "elm.dev.json"
              hasCfg <- Dir.doesFileExist cfgPath
              if hasCfg then
                pure (MCP.ReadResourceResponse [ MCP.markdown req Guides.architectureElmDevAppMd ])
              else do
                let root = Ext.Dev.Project.getRoot proj
                outline <- Elm.Outline.read root
                case outline of
                  Right (Elm.Outline.Pkg _) ->
                    pure (MCP.ReadResourceResponse [ MCP.markdown req Guides.architectureElmPackageMd ])
                  Right (Elm.Outline.App _) ->
                    pure (MCP.ReadResourceResponse [ MCP.markdown req Guides.architectureElmAppMd ])
                  Left _ -> do
                    let msg = ("This is a standard Elm app using the Elm Architecture." :: Text)
                    pure (MCP.ReadResourceResponse [ MCP.markdown req msg ])
      }


resourceWellFormedElmCode :: MCP.Resource
resourceWellFormedElmCode =
  let pat = Uri.pattern "file" [Uri.s "guides", Uri.s "well-formed-elm-code"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Well‑Formed Elm Code"
      , MCP.resourceDescription = Just "Guidelines for writing clear, robust Elm code."
      , MCP.resourceMimeType = Just "text/markdown"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceAssistant] MCP.High Nothing)
      , MCP.read = \req _state _emit _connId -> do
          pure (MCP.ReadResourceResponse [ MCP.markdown req Guides.wellFormedElmCodeMd ])
      }


lookupInt :: Map.Map Text Text -> Text -> Maybe Int
lookupInt queryParams key =
    case Map.lookup key queryParams of
        Just t ->
          case reads (Text.unpack t) of
            [(i, "")] -> Just i
            _ -> Nothing
        Nothing -> Nothing

resolveProject :: Map.Map Text Text -> Live.State -> IO (Either Text Client.ProjectCache)
resolveProject queryParams state =
  ProjectLookup.resolveProject
    (lookupInt queryParams "projectId") 
    state


resourceDiagnostics :: MCP.Resource
resourceDiagnostics =
  let pat = Uri.pattern "elm" [Uri.s "diagnostics"] ["file"]
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm Diagnostics"
      , MCP.resourceDescription = Just "Project or file diagnostics. Examples: elm://diagnostics, elm://diagnostics?file=/abs/path/File.elm"
      , MCP.resourceMimeType = Just "text/markdown"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req state _emit connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch _pathVals queryParams) -> do
              let mFile = Map.lookup "file" queryParams

              projectFound <- ProjectLookup.resolveProjectFromSession Nothing connId state
              case projectFound of
                Left msg -> do
                  let body = Text.concat ["Error: ", msg]
                  pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
                Right pc -> do
                  _ <- Watchtower.State.Compile.ensureProjectFresh state "mcp.diagnostics.resource" pc
                  case mFile of
                    Just fileTxt -> do
                      let filePath = Text.unpack fileTxt
                      diags <- Helpers.getDiagnosticsForProject state pc (Just filePath)
                      let body = renderDiagnosticsForFile (Text.unpack fileTxt) diags
                      pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
                    Nothing -> do
                      byFile <- Helpers.getProjectDiagnosticsByFile state pc
                      let body = renderDiagnosticsProject byFile
                      pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
            Nothing -> do
              let body = "Bad URI: expected elm://diagnostics or elm://diagnostics?file=/abs/path/File.elm"
              pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
      }


resourceModuleGraph :: MCP.Resource
resourceModuleGraph =
  let pat = Uri.pattern "elm" [Uri.s "graph/module/", Uri.var "Module"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm Module Graph"
      , MCP.resourceDescription = Just "Imports/exports/dependents (elm://graph/module/{Module}), e.g. elm://graph/module/App.Main"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req state _emit connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch pathVals _queryParams) -> do
              projectFound <- ProjectLookup.resolveProjectFromSession Nothing connId state
                               
              case projectFound of
                Left msg -> do
                  let val = JSON.object [ "error" .= msg ]
                  pure (MCP.ReadResourceResponse [ MCP.json req val ])
                Right (Client.ProjectCache proj _ _ _ _) -> do
                  case Map.lookup "Module" pathVals of
                    Nothing -> do
                      let val = JSON.object [ "error" .= ("missing module" :: Text) ]
                      pure (MCP.ReadResourceResponse [ MCP.json req val ])
                    Just moduleTxt -> do
                      let root = Ext.Dev.Project.getRoot proj
                      details <- Ext.CompileProxy.loadProject root
                      let wantName = Name.fromChars (Text.unpack moduleTxt)
                      let locals = Elm.Details._locals details
                      let maybeLocal = Map.lookup wantName locals
                      let importsList = maybe [] Elm.Details._deps maybeLocal
                      let toObj n =
                            JSON.object
                              [ "module" .= Text.pack (Name.toChars n)
                              , "path" .= maybe JSON.Null (JSON.String . Text.pack) (Ext.Dev.Project.lookupModulePath details n)
                              ]
                      let importObjs = map toObj importsList
                      let dependents = Set.toList (Ext.Dev.Project.importersOf details wantName)
                      let dependentObjs = map toObj dependents
                      let modulePath = Ext.Dev.Project.lookupModulePath details wantName
                      let val = JSON.object
                                [ "kind" .= ("moduleGraph" :: Text)
                                , "module" .= moduleTxt
                                , "path" .= maybe JSON.Null (JSON.String . Text.pack) modulePath
                                , "imports" .= importObjs
                                , "dependents" .= dependentObjs
                                ]
                      pure (MCP.ReadResourceResponse [ MCP.json req val ])
            Nothing -> do
              let val = JSON.object [ "error" .= ("bad uri" :: Text) ]
              pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }

resourcePackageDocs :: MCP.Resource
resourcePackageDocs =
  let pat = Uri.pattern "elm" [Uri.s "docs/package/", Uri.var "author", Uri.s "/", Uri.var "project"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm Package Docs"
      , MCP.resourceDescription = Just "Docs for a package e.g. elm://docs/package/elm/json"
      , MCP.resourceMimeType = Just "text/markdown"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req state _emit connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch pathVals _q) -> do
              let maybeAuthor = Map.lookup "author" pathVals
              let maybeProject = Map.lookup "project" pathVals
              case (maybeAuthor, maybeProject) of
                (Just author, Just project) -> do
                  let pkgName = Pkg.toName (Utf8.fromChars (Text.unpack author)) (Text.unpack project)
                  pkgs <- Control.Concurrent.STM.readTVarIO (Client.packages state)
                  case Map.lookup pkgName pkgs of
                    Nothing -> do
                      -- Try to resolve a usable version: currently used in focused project, or latest from registry
                      maybeRoot <- do
                        resolved <- ProjectLookup.resolveProjectFromSession Nothing connId state
                        case resolved of
                          Right (Client.ProjectCache proj _ _ _ _) ->
                            pure (Just (Ext.Dev.Project.getRoot proj))
                          Left _ ->
                            pure Nothing

                      mVersion <- case maybeRoot of
                        Nothing ->
                          Ext.Dev.Package.getPackageNewestPackageVersionFromRegistry pkgName
                        Just rootDir -> do
                          fromOutline <- Ext.Dev.Package.getElmJsonVersion rootDir pkgName
                          case fromOutline of
                            Just v -> pure (Just v)
                            Nothing -> Ext.Dev.Package.getPackageNewestPackageVersionFromRegistry pkgName
                      case mVersion of
                        Nothing -> do
                          let body = "Package not found in cache or registry: " <> author <> "/" <> project
                          pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
                        Just vsn -> do
                          docsResult <- Ext.Dev.Package.getDocs pkgName vsn
                          case docsResult of
                            Left _err -> do
                              let body = "Failed to fetch docs for: " <> author <> "/" <> project
                              pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
                            Right docsMap -> do
                              -- Insert into in-memory package cache
                              let mods = Map.map (\m -> Client.PackageModule { Client.packageModuleDocs = m }) docsMap
                                  pkgInfo = Client.PackageInfo { Client.name = pkgName, Client.readme = Nothing, Client.packageModules = mods }
                              Control.Concurrent.STM.atomically $ do
                                cur <- Control.Concurrent.STM.readTVar (Client.packages state)
                                Control.Concurrent.STM.writeTVar (Client.packages state) (Map.insert pkgName pkgInfo cur)
                              -- Render docs now that it's cached
                              let modulesDocs = [ Client.packageModuleDocs pm | pm <- Map.elems mods ]
                                  readme = Nothing
                                  body = DocsRender.renderPackage (DocsRender.PackageMeta (Text.pack (Pkg.toChars pkgName)) Nothing) readme modulesDocs
                              pure (MCP.ReadResourceResponse [ MCP.markdown req body ])

                    Just (Client.PackageInfo { Client.readme = r, Client.packageModules = mods }) -> do
                      let modulesDocs = [ Client.packageModuleDocs pm | pm <- Map.elems mods ]
                          readme = fmap Text.pack r
                          body = DocsRender.renderPackage (DocsRender.PackageMeta (Text.pack (Pkg.toChars pkgName)) Nothing) readme modulesDocs
                      pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
                    
                _ -> do
                  let body = "Bad package name format: " <> MCP.readResourceUri req <> ".  Expected format: (elm://docs/package/{author/project}), e.g. elm://docs/package/elm/json"
                  pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
                  
            Nothing -> do
              let body = "Bad URI format. (elm://docs/package/{author/project}), e.g. elm://docs/package/elm/json"
              pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
        }

resourceModuleDocs :: MCP.Resource
resourceModuleDocs =
  let pat = Uri.pattern "elm" [Uri.s "docs/module/", Uri.var "Module"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm Module Docs"
      , MCP.resourceDescription = Just "Documentation for a module (elm://docs/module/{Module}), e.g. elm://docs/module/App.Main"
      , MCP.resourceMimeType = Just "text/markdown"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req state _emit connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch pathVals _q) -> do
              case Map.lookup "Module" pathVals of
                Nothing -> do
                  pure (MCP.ReadResourceResponse
                          [ MCP.markdown req "No module provided.  Call this resource like elm://docs/module/{Module}, e.g. elm://docs/module/App.Main"
                          ])                  
                Just moduleTxt -> do
                  let wantName = Name.fromChars (Text.unpack moduleTxt)
                  r <- resolveModuleSpec state (ModuleByName wantName)
                  case r of
                    Right (ModuleResolved _ maybePath maybePkg docsMod) -> do
                      let meta = DocsRender.ModuleMeta moduleTxt Nothing maybePath (fmap (Text.pack . Pkg.toChars) maybePkg)
                          body = DocsRender.renderModule meta docsMod
                      pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
                    Left _ -> do
                      pure (MCP.ReadResourceResponse
                            [ MCP.markdown req ("No module found named " <> moduleTxt)
                            ])
            Nothing -> do
              pure (MCP.ReadResourceResponse
                    [ MCP.markdown req "Unknown URI format. (elm://docs/module/{Module}), e.g. elm://docs/module/App.Main"
                    ])
      }


resourceFileDocs :: MCP.Resource
resourceFileDocs =
  let pat = Uri.pattern "elm" [Uri.s "docs/file"] ["file"]
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm File Docs"
      , MCP.resourceDescription = Just "Docs for a specific file (elm://docs/file?file=/abs/path/to/File.elm)"
      , MCP.resourceMimeType = Just "text/markdown"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req state _emit _connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch _pathVals queryParams) -> do
              case Map.lookup "file" queryParams of
                Nothing -> do
                  pure (MCP.ReadResourceResponse
                          [ MCP.markdown req "No file path provided.  Call this resource like elm://docs/file?file=/abs/path/to/File.elm" ]
                        )
                Just fileTxt -> do
                  let filePath = Text.unpack fileTxt
                  r <- resolveModuleSpec state (ModuleByFilePath filePath)
                  case r of
                    Right (ModuleResolved moduleName _ _ docsMod) -> do
                      let body = DocsRender.renderModule (DocsRender.ModuleMeta (Text.pack (Name.toChars moduleName)) Nothing (Just (Text.unpack fileTxt)) Nothing) docsMod
                      pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
                    
                    Left msg -> do
                      pure (MCP.ReadResourceResponse
                              [ MCP.markdown req ("No module found at path " <> fileTxt <> " (" <> msg <> ")" )
                              ]
                           )
            Nothing -> do
              pure (MCP.ReadResourceResponse
                    [ MCP.markdown req "Unknown URI format. (elm://docs/file?file=/abs/path/to/File.elm)"
                    ])
      }



resourceValueDocs :: MCP.Resource
resourceValueDocs =
  let pat = Uri.pattern "elm" [Uri.s "docs/value/", Uri.var "Module.name"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Value Docs"
      , MCP.resourceDescription = Just "Docs for a value (elm://docs/value/{Module.name}), e.g. elm://docs/value/List.map"
      , MCP.resourceMimeType = Just "text/markdown"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req state _emit connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch pathVals _q) -> do
              case Map.lookup "Module.name" pathVals >>= splitModuleAndValue of
                Nothing -> 
                  pure (MCP.ReadResourceResponse
                          [ MCP.markdown req ("No module name provided.  Call this resource like elm://docs/value/{Module.name}, e.g. elm://docs/value/List.map" )
                          ])
                Just (modName, valName) -> do
                  r <- resolveModuleSpec state (ModuleByName modName)
                  case r of
                    Right (ModuleResolved _ maybePath maybePkg docsMod) -> do
                      let meta = DocsRender.ValueMeta 
                                    (Text.pack (Name.toChars modName))
                                    (Text.pack (Name.toChars valName))
                                    Nothing
                                    maybePath
                                    (fmap (Text.pack . Pkg.toChars) maybePkg)
                          body = DocsRender.renderValue meta docsMod valName
                      pure (MCP.ReadResourceResponse [ MCP.markdown req body ])

                    Left _ -> do
                      pure (MCP.ReadResourceResponse
                              [ MCP.markdown req ("No value found that matches " <> Text.pack (Name.toChars valName) )
                              ]
                           )
            Nothing -> do
              let msg :: Text
                  msg = "Unknown URI format. (elm://docs/value/{Module.name}), e.g. elm://docs/value/List.map"
              pure (MCP.ReadResourceResponse [ MCP.markdown req msg ])
      }


-- List known projects as a markdown resource with embedded YAML
resourceProjectList :: MCP.Resource
resourceProjectList =
  let pat = Uri.pattern "elm" [Uri.s "projects"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm Projects"
      , MCP.resourceDescription = Just "Lists known projects, indicates compiling status and current selection"
      , MCP.resourceMimeType = Just "text/markdown"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.High Nothing)
      , MCP.read = \req state _emit connId -> do
          let (Client.State _ mProjects _ _ _ _ _ _ _) = state
          projects <- Control.Concurrent.STM.readTVarIO mProjects
          mFocused <- Client.getFocusedProjectId state connId

          items <- mapM
            (\(Client.ProjectCache proj _ _ mCompileResult _) -> do
                cr <- Control.Concurrent.STM.readTVarIO mCompileResult
                let compiling = case cr of
                                  Client.NotCompiled -> True
                                  _ -> False
                    pid = Ext.Dev.Project._shortId proj
                    root = Ext.Dev.Project.getRoot proj
                    isCurrent = maybe False (== pid) mFocused
                pure (pid, root, compiling, isCurrent)
            )
            projects

          let header :: Text
              header = "Projects"
              note :: [Text]
              note = case mFocused of
                       Nothing -> ["\nNote: No project is currently selected."]
                       Just _ -> []
              yamlBlock :: [Text]
              yamlBlock =
                ["\n```yaml", "projects:"]
                ++ concatMap
                    (\(pid, root, compiling, isCurrent) ->
                        let base =
                              [ Text.concat ["  - id: ", Text.pack (show pid)]
                              , Text.concat ["    root: ", Text.pack root]
                              , Text.concat ["    compiling: ", if compiling then "true" else "false"]
                              ]
                            currentLine = if isCurrent then ["    focused: true"] else []
                        in base ++ currentLine
                    )
                    items
                ++ ["```\n"]
              footer :: Text
              footer = "Selection is automatic in most cases. Use `project_select` only when there are multiple projects and you need to switch focus explicitly."
              body :: Text
              body = Text.intercalate "\n" ([header] ++ note) <> Text.concat yamlBlock <> "\n" <> footer
          pure (MCP.ReadResourceResponse [ MCP.markdown req body ])
      }

-- * Available Prompts

availablePrompts :: [MCP.Prompt]
availablePrompts = []


-- | Main MCP server handler
serve :: Live.State -> JSONRPC.EventEmitter -> JSONRPC.ConnectionId -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)
serve state emitter connId req = do
  MCP.serve availableTools availableResources availablePrompts state emitter connId req

-- Docs helpers


-- Formatting helpers for diagnostics and unused warnings

renderDiagnosticsForFile :: FilePath -> [LSPProtocol.Diagnostic] -> Text
renderDiagnosticsForFile filePath diags =
  let header = Text.concat ["### Diagnostics for ", Text.pack filePath]
      bodyLines =
        if null diags then
          ["No diagnostics."]
        else
          map formatDiag diags
  in Text.intercalate "\n" (header : "" : bodyLines)

formatDiag :: LSPProtocol.Diagnostic -> Text
formatDiag d =
  let start = LSPProtocol.rangeStart (LSPProtocol.diagnosticRange d)
      line = 1 + LSPProtocol.positionLine start
      col  = 1 + LSPProtocol.positionCharacter start
      msg  = LSPProtocol.diagnosticMessage d
  in Text.concat ["- ", Text.pack (show line), ":", Text.pack (show col), " - ", msg]

renderDiagnosticsProject :: Map.Map FilePath [LSPProtocol.Diagnostic] -> Text
renderDiagnosticsProject byFile =
  let nonEmpty = filter (not . null . snd) (Map.toList byFile)
  in if null nonEmpty
       then "No diagnostics."
       else
          let sections = map (\(fp, ds) -> renderDiagnosticsForFile fp ds) nonEmpty
          in Text.intercalate "\n\n" sections

resolveDiagnosticsProjectFromArgs :: JSON.Object -> Client.State -> JSONRPC.ConnectionId -> IO (Either Text Client.ProjectCache)
resolveDiagnosticsProjectFromArgs args state connId =
  do
    resolved <- resolveProjectFromDirArg args state connId
    case resolved of
      Left msg -> pure (Left msg)
      Right pc -> do
        _ <- Watchtower.State.Compile.ensureProjectFresh state "mcp.diagnostics" pc
        pure (Right pc)

resolveProjectFromDirArg :: JSON.Object -> Client.State -> JSONRPC.ConnectionId -> IO (Either Text Client.ProjectCache)
resolveProjectFromDirArg args state connId =
  case getStringArg args "dir" of
    Just dir -> do
      canonical <- Dir.canonicalizePath dir
      Watchtower.State.Discover.discover state canonical
      existing <- Client.getExistingProject canonical state
      case existing of
        Right (pc, _) -> pure (Right pc)
        Left _ -> pure (Left ("No Elm project found for dir: " <> Text.pack canonical))
    Nothing -> do
      ProjectLookup.resolveProjectFromSession Nothing connId state

diagnosticsReport :: Client.State -> Client.ProjectCache -> Maybe FilePath -> Bool -> Int -> IO Text
diagnosticsReport state pc@(Client.ProjectCache proj _ _ _ _) maybeFile includeWarnings limitRaw = do
  errorsByFile <- case maybeFile of
    Just filePath -> do
      diags <- Helpers.getDiagnosticsForProject state pc (Just filePath)
      pure [(filePath, diags)]
    Nothing -> Map.toList <$> Helpers.getProjectDiagnosticsByFile state pc

  warningsByFile <-
    if not includeWarnings
      then pure []
      else case maybeFile of
        Just filePath -> do
          diags <- warningDiagnosticsForFile state proj filePath
          pure [(filePath, diags)]
        Nothing -> do
          allInfos <- Client.getAllFileInfos state
          let projectFiles = fmap fst $ filter (\(p, _) -> Ext.Dev.Project.affectsCompilation p proj) (Map.toList allInfos)
          mapM (\p -> do
                  diags <- warningDiagnosticsForFile state proj p
                  pure (p, diags)
               ) projectFiles

  let errors = concatMap (\(fp, ds) -> fmap (\d -> (fp, d)) ds) errorsByFile
      warnings = concatMap (\(fp, ds) -> fmap (\d -> (fp, d)) ds) warningsByFile
      rendered = if includeWarnings then errors ++ warnings else errors
      limit = max 0 limitRaw
      shown = take limit rendered
      truncated = length rendered - length shown
      body = renderDiagnosticsToolBody shown truncated limit
      summary = renderDiagnosticsSummary (length errors) (length warnings) includeWarnings
  pure (Text.intercalate "\n\n" [body, summary])

warningDiagnosticsForFile :: Client.State -> Ext.Dev.Project.Project -> FilePath -> IO [LSPProtocol.Diagnostic]
warningDiagnosticsForFile state proj filePath = do
  (_loc, warns) <- Helpers.getWarningsForFile state filePath
  unusedModule <- Helpers.unusedModuleDiagnosticForFile proj filePath
  pure (concatMap Helpers.warningToUnusedDiagnostic warns ++ unusedModule)

renderDiagnosticsToolBody :: [(FilePath, LSPProtocol.Diagnostic)] -> Int -> Int -> Text
renderDiagnosticsToolBody shown truncated limit =
  if null shown then
    "No diagnostics to show."
  else
    let grouped = Map.toList (Map.fromListWith (++) [ (fp, [diag]) | (fp, diag) <- shown ])
        sections = map (\(fp, ds) -> renderDiagnosticsToolFile fp (reverse ds)) grouped
        truncLine = if truncated > 0 then ["", "_Output limited to " <> Text.pack (show limit) <> " diagnostics; " <> Text.pack (show truncated) <> " not shown._"] else []
    in Text.intercalate "\n\n" sections <> Text.intercalate "\n" truncLine

renderDiagnosticsToolFile :: FilePath -> [LSPProtocol.Diagnostic] -> Text
renderDiagnosticsToolFile filePath diags =
  Text.intercalate "\n" (Text.concat ["### ", Text.pack filePath] : "" : map formatDiagWithSeverity diags)

formatDiagWithSeverity :: LSPProtocol.Diagnostic -> Text
formatDiagWithSeverity d =
  let start = LSPProtocol.rangeStart (LSPProtocol.diagnosticRange d)
      line = 1 + LSPProtocol.positionLine start
      col  = 1 + LSPProtocol.positionCharacter start
      msg  = LSPProtocol.diagnosticMessage d
  in Text.concat ["- ", diagnosticSeverityLabel d, " ", Text.pack (show line), ":", Text.pack (show col), " - ", msg]

diagnosticSeverityLabel :: LSPProtocol.Diagnostic -> Text
diagnosticSeverityLabel d =
  case LSPProtocol.diagnosticSeverity d of
    Just (LSPProtocol.DiagnosticSeverity 2) -> "warning"
    Just (LSPProtocol.DiagnosticSeverity 3) -> "info"
    Just (LSPProtocol.DiagnosticSeverity 4) -> "hint"
    _ -> "error"

renderDiagnosticsSummary :: Int -> Int -> Bool -> Text
renderDiagnosticsSummary errorCount warningCount includeWarnings =
  Text.intercalate "\n"
    [ "## Summary"
    , ""
    , "- Total errors: " <> Text.pack (show errorCount)
    , if includeWarnings
        then "- Total warnings: " <> Text.pack (show warningCount) <> " (included)"
        else "- Warnings: not computed (suppressed)"
    ]

partitionUnused :: [Warning.Warning] -> ([Text], [(Int, Text)])
partitionUnused warns =
  let step w (importsAcc, valuesAcc) =
        case w of
          Warning.UnusedImport region moduleName ->
            ( Text.pack (Name.toChars moduleName) : importsAcc
            , valuesAcc
            )
          Warning.UnusedVariable region _context name ->
            let (Ann.Region (Ann.Position row _col) _end) = region
            in ( importsAcc
               , (fromIntegral row, Text.pack (Name.toChars name)) : valuesAcc
               )
          _ -> (importsAcc, valuesAcc)
      (importsRaw, valuesRaw) = foldr step ([], []) warns
      imports = List.sort (List.nub importsRaw)
      values  = List.sortOn fst valuesRaw
  in (imports, values)

renderUnusedMarkdown :: [(FilePath, [Text], [(Int, Text)])] -> Text
renderUnusedMarkdown items =
  let sections = map renderOne items
  in Text.intercalate "\n\n" sections
  where
    renderOne (fp, imports, values) =
      let header = Text.concat ["### ", Text.pack fp]
          importsBlock =
            if null imports then []
            else
              [ "**Unused imports**"
              ] ++ map (\m -> Text.concat ["- ", m]) imports
          valuesBlock =
            if null values then []
            else
              [ "**Unused values**"
              ] ++ map (\(line, name) -> Text.concat ["- ", Text.pack (show line), " | ", name]) values
      in Text.intercalate "\n" (header : "" : importsBlock ++ (if null importsBlock || null valuesBlock then [] else [""]) ++ valuesBlock)


parsePkgName :: Text -> Maybe Pkg.Name
parsePkgName t =
  case Text.splitOn "/" t of
    [author, project] -> Just (Pkg.toName (Utf8.fromChars (Text.unpack author)) (Text.unpack project))
    _ -> Nothing

parseVersionText :: Text -> Maybe V.Version
parseVersionText t =
  case traverse readInt (Text.splitOn "." t) of
    Just [major, minor, patch] -> Just (V.Version (fromIntegral major) (fromIntegral minor) (fromIntegral patch))
    _ -> Nothing
  where
    readInt part =
      case reads (Text.unpack part) of
        [(n, "")] | n >= (0 :: Int) && n <= 65535 -> Just n
        _ -> Nothing

projectFromCache :: Client.ProjectCache -> Ext.Dev.Project.Project
projectFromCache (Client.ProjectCache proj _ _ _ _) = proj

compileDocsProject :: Client.State -> Client.ProjectCache -> IO ()
compileDocsProject state pc = do
  _ <- Watchtower.State.Compile.ensureProjectFresh state "mcp.docs" pc
  pure ()

resolveDocsProjectFromArgs :: JSON.Object -> Client.State -> JSONRPC.ConnectionId -> IO (Either Text (Maybe Client.ProjectCache))
resolveDocsProjectFromArgs args state connId =
  case getStringArg args "dir" of
    Just dir -> do
      canonical <- Dir.canonicalizePath dir
      Watchtower.State.Discover.discover state canonical
      existing <- Client.getExistingProject canonical state
      case existing of
        Right (pc, _) -> do
          compileDocsProject state pc
          pure (Right (Just pc))
        Left _ -> pure (Left ("No Elm project found for dir: " <> Text.pack canonical))
    Nothing -> do
      resolved <- ProjectLookup.resolveProjectFromSession Nothing connId state
      case resolved of
        Left msg -> pure (Left msg)
        Right pc -> do
          compileDocsProject state pc
          pure (Right (Just pc))

docsRootFromArgs :: JSON.Object -> Client.State -> JSONRPC.ConnectionId -> IO (Maybe FilePath)
docsRootFromArgs args state connId =
  case getStringArg args "dir" of
    Just dir -> do
      resolved <- resolveDocsProjectFromArgs args state connId
      case resolved of
        Right (Just pc) -> pure (Just (Ext.Dev.Project.getRoot (projectFromCache pc)))
        _ -> Dir.canonicalizePath dir >>= pure . Just
    Nothing -> do
      resolved <- ProjectLookup.resolveProjectFromSession Nothing connId state
      case resolved of
        Right pc -> pure (Just (Ext.Dev.Project.getRoot (projectFromCache pc)))
        Left _ -> pure Nothing

loadPackageDocsForTool :: Client.State -> Pkg.Name -> Maybe V.Version -> Maybe FilePath -> IO (Either Text Docs.Documentation)
loadPackageDocsForTool state pkgName mExplicitVersion mRoot = do
  mVersion <- case mExplicitVersion of
    Just v -> pure (Just v)
    Nothing -> case mRoot of
      Just root -> Ext.Dev.Package.getCurrentlyUsedOrLatestVersion root pkgName
      Nothing -> Ext.Dev.Package.getPackageNewestPackageVersionFromRegistry pkgName
  case mVersion of
    Nothing -> pure (Left ("Package not found in cache or registry: " <> Text.pack (Pkg.toChars pkgName)))
    Just vsn -> do
      docsResult <- Ext.Dev.Package.getDocs pkgName vsn
      case docsResult of
        Left _ -> pure (Left ("Failed to fetch docs for: " <> Text.pack (Pkg.toChars pkgName)))
        Right docsMap -> do
          case mExplicitVersion of
            Just _ -> pure ()
            Nothing -> do
              let mods = Map.map (\m -> Client.PackageModule { Client.packageModuleDocs = m }) docsMap
                  pkgInfo = Client.PackageInfo { Client.name = pkgName, Client.readme = Nothing, Client.packageModules = mods }
              Control.Concurrent.STM.atomically $ do
                cur <- Control.Concurrent.STM.readTVar (Client.packages state)
                Control.Concurrent.STM.writeTVar (Client.packages state) (Map.insert pkgName pkgInfo cur)
          pure (Right docsMap)

renderPackageDocsTool :: Client.State -> Pkg.Name -> Maybe V.Version -> Maybe FilePath -> IO MCP.ToolCallResponse
renderPackageDocsTool state pkgName mExplicitVersion mRoot = do
  result <- renderPackageDocsText state pkgName mExplicitVersion mRoot
  case result of
    Left msg -> pure (errTxt msg)
    Right body -> pure (ok body)

renderPackageDocsText :: Client.State -> Pkg.Name -> Maybe V.Version -> Maybe FilePath -> IO (Either Text Text)
renderPackageDocsText state pkgName mExplicitVersion mRoot = do
  cached <- Control.Concurrent.STM.readTVarIO (Client.packages state)
  case (mExplicitVersion, Map.lookup pkgName cached) of
    (Nothing, Just (Client.PackageInfo { Client.readme = r, Client.packageModules = mods })) -> do
      let modulesDocs = [ Client.packageModuleDocs pm | pm <- Map.elems mods ]
          readme = fmap Text.pack r
          body = DocsRender.renderPackage (DocsRender.PackageMeta (Text.pack (Pkg.toChars pkgName)) mRoot) readme modulesDocs
      pure (Right body)
    _ -> do
      docsResult <- loadPackageDocsForTool state pkgName mExplicitVersion mRoot
      case docsResult of
        Left msg -> pure (Left msg)
        Right docsMap -> do
          let body = DocsRender.renderPackage (DocsRender.PackageMeta (Text.pack (Pkg.toChars pkgName)) mRoot) Nothing (Map.elems docsMap)
          pure (Right body)

localModulesForProject :: Client.State -> Client.ProjectCache -> IO [(FilePath, Docs.Module)]
localModulesForProject state pc = do
  infos <- Client.getAllFileInfos state
  let proj = projectFromCache pc
      isInProject fp = Ext.Dev.Project.contains fp proj
      pick (fp, Client.FileInfo { Client.docs = Just m }) | isInProject fp = Just (fp, m)
      pick _ = Nothing
  pure (List.sortOn (moduleSortName . snd) (Maybe.mapMaybe pick (Map.toList infos)))

moduleSortName :: Docs.Module -> String
moduleSortName (Docs.Module name _ _ _ _ _) = Name.toChars name

findLocalModuleDocsInProject :: Client.State -> Client.ProjectCache -> Name.Name -> IO (Maybe (FilePath, Docs.Module))
findLocalModuleDocsInProject state pc wantName = do
  modules <- localModulesForProject state pc
  pure (List.find (\(_, Docs.Module name _ _ _ _ _) -> name == wantName) modules)

findLocalModuleDocs :: Client.State -> Name.Name -> IO (Maybe (FilePath, Docs.Module))
findLocalModuleDocs state wantName = do
  infos <- Client.getAllFileInfos state
  pure $
    let matches (_, Client.FileInfo { Client.docs = Just m@(Docs.Module name _ _ _ _ _) }) =
          name == wantName
        matches _ = False
    in case filter matches (Map.toList infos) of
         ((fp, Client.FileInfo { Client.docs = Just m }):_) -> Just (fp, m)
         _ -> Nothing

findPackageModuleDocs :: Client.State -> Name.Name -> IO (Maybe (Pkg.Name, Docs.Module))
findPackageModuleDocs state wantName = do
  pkgsMap <- Control.Concurrent.STM.readTVarIO (Client.packages state)
  pure $
    let go list =
          case list of
            [] -> Nothing
            (pkgName, Client.PackageInfo { Client.packageModules = mods }) : rest ->
              case Map.lookup wantName mods of
                Just (Client.PackageModule { Client.packageModuleDocs = m }) -> Just (pkgName, m)
                Nothing -> go rest
    in go (Map.toList pkgsMap)


-- Shared docs resolver types
data ModuleSpec
  = ModuleByName Name.Name
  | ModuleByFilePath FilePath

data ModuleResolved = ModuleResolved
  { resolvedName :: Name.Name
  , resolvedPath :: Maybe FilePath
  , resolvedPackage :: Maybe Pkg.Name
  , resolvedDocs :: Docs.Module
  }

-- Resolve module docs either by module name or file path.
-- Tries local project first, compiling when needed; then falls back to packages when given a name.
resolveModuleSpec :: Client.State -> ModuleSpec -> IO (Either Text ModuleResolved)
resolveModuleSpec state spec =
  resolveModuleSpecScoped state Nothing spec

resolveModuleSpecScoped :: Client.State -> Maybe Client.ProjectCache -> ModuleSpec -> IO (Either Text ModuleResolved)
resolveModuleSpecScoped state maybeProject spec =
  case spec of
    ModuleByName wantName -> do
      mLocal <- case maybeProject of
        Just pc -> findLocalModuleDocsInProject state pc wantName
        Nothing -> findLocalModuleDocs state wantName
      case mLocal of
        Just (fp, m) -> pure (Right (ModuleResolved wantName (Just fp) Nothing m))
        Nothing -> do
          mPkg <- findPackageModuleDocs state wantName
          case mPkg of
            Just (pkgName, m) -> pure (Right (ModuleResolved wantName Nothing (Just pkgName) m))
            Nothing -> pure (Left "module not found")

    ModuleByFilePath fp -> do
      mInfo <- Client.getFileInfo fp state
      case mInfo of
        Just (Client.FileInfo { Client.docs = Just m@(Docs.Module name _ _ _ _ _) }) ->
          pure (Right (ModuleResolved name (Just fp) Nothing m))
        _ -> do
          -- Try compiling the containing project, then retry
          projectResult <- Client.getExistingProject fp state
          case projectResult of
            Left _ -> pure (Left "file not associated with a known project")
            Right (pc, _) -> do
              _ <- Watchtower.State.Compile.ensureProjectFresh state "mcp.module_by_filepath" pc
              mInfo2 <- Client.getFileInfo fp state
              case mInfo2 of
                Just (Client.FileInfo { Client.docs = Just m@(Docs.Module name _ _ _ _ _) }) ->
                  pure (Right (ModuleResolved name (Just fp) Nothing m))
                _ -> pure (Left "no docs for file")

renderDocsModules :: Maybe Client.ProjectCache -> Maybe (Either a Elm.Outline.Outline) -> [(FilePath, Docs.Module)] -> Map.Map Pkg.Name Client.PackageInfo -> Text
renderDocsModules maybeProject maybeOutline localDocs pkgs =
  let exposed = case maybeOutline of
        Just (Right (Elm.Outline.Pkg pkg)) -> Set.fromList (Elm.Outline.flattenExposed (Elm.Outline._pkg_exposed pkg))
        _ -> Set.empty
      isPackage = case maybeOutline of
        Just (Right (Elm.Outline.Pkg _)) -> True
        _ -> False
      localName (Docs.Module name _ _ _ _ _) = name
      localPublic = filter (\(_, m) -> Set.member (localName m) exposed) localDocs
      localPrivate = filter (\(_, m) -> not (Set.member (localName m) exposed)) localDocs
      localSection =
        if null localDocs then []
        else if isPackage then
          section "Public Local Modules" (map (moduleLink . snd) localPublic)
          ++ section "Private Local Modules" (map (moduleLink . snd) localPrivate)
        else section "Local Project Modules" (map (moduleLink . snd) localDocs)
      packageSection (pkgName, Client.PackageInfo { Client.packageModules = mods }) =
        section (Text.pack (Pkg.toChars pkgName)) (map (moduleLink . Client.packageModuleDocs) (Map.elems mods))
      header = case maybeProject of
        Just pc -> ["# Modules", "", "Project: `" <> Text.pack (Ext.Dev.Project.getRoot (projectFromCache pc)) <> "`"]
        Nothing -> ["# Modules"]
      dependencySections = concatMap packageSection (Map.toList pkgs)
  in Text.intercalate "\n" (header ++ [""] ++ localSection ++ ["", "## Dependencies", ""] ++ if null dependencySections then ["- None loaded"] else dependencySections)

renderDocsProject :: Client.ProjectCache -> Either a Elm.Outline.Outline -> [(FilePath, Docs.Module)] -> Map.Map Pkg.Name Client.PackageInfo -> Text
renderDocsProject pc outlineResult localDocs pkgs =
  let proj = projectFromCache pc
      root = Ext.Dev.Project.getRoot proj
      srcDirs = Ext.Dev.Project._srcDirs proj
      projectKind = case outlineResult of
        Right (Elm.Outline.App _) -> "application"
        Right (Elm.Outline.Pkg _) -> "package"
        Left _ -> "unknown"
      packageMeta = case outlineResult of
        Right (Elm.Outline.Pkg pkg) ->
          [ "Package: `" <> Text.pack (Pkg.toChars (Elm.Outline._pkg_name pkg)) <> "`"
          , "Version: `" <> Text.pack (V.toChars (Elm.Outline._pkg_version pkg)) <> "`"
          ]
        _ -> []
      dependencies = case outlineResult of
        Right (Elm.Outline.App app) -> Map.keys (Elm.Outline._app_deps_direct app)
        Right (Elm.Outline.Pkg pkg) -> Map.keys (Elm.Outline._pkg_deps pkg)
        Left _ -> []
      exposed = case outlineResult of
        Right (Elm.Outline.Pkg pkg) -> Set.fromList (Elm.Outline.flattenExposed (Elm.Outline._pkg_exposed pkg))
        _ -> Set.empty
      publicCount = length (filter (\(_, Docs.Module name _ _ _ _ _) -> Set.member name exposed) localDocs)
      privateCount = if Set.null exposed then length localDocs else length localDocs - publicCount
  in Text.intercalate "\n"
      ( [ "# Elm Project Docs"
        , ""
        , "Root: `" <> Text.pack root <> "`"
        , "Type: `" <> projectKind <> "`"
        ]
        ++ packageMeta
        ++ [ ""
           , "## Source Directories"
           , "" ]
        ++ map (\d -> "- `" <> Text.pack d <> "`") srcDirs
        ++ [ ""
           , "## Local Modules"
           , ""
           , "- Total: " <> Text.pack (show (length localDocs))
           , "- Public: " <> Text.pack (show publicCount)
           , "- Private/app-local: " <> Text.pack (show privateCount)
           , ""
           , "## Direct Dependencies"
           , "" ]
        ++ (if null dependencies then ["- None"] else map (\p -> "- `" <> Text.pack (Pkg.toChars p) <> "`") dependencies)
        ++ [ ""
           , "## Loaded Dependency Docs"
           , "" ]
        ++ (if Map.null pkgs then ["- None loaded"] else map (\(p, info) -> "- `" <> Text.pack (Pkg.toChars p) <> "` (" <> Text.pack (show (Map.size (Client.packageModules info))) <> " modules)") (Map.toList pkgs))
        ++ [ ""
           , "## Follow-up Tools"
           , ""
           , "- `docs_modules` to list modules grouped by visibility/package."
           , "- `docs_search` to find modules, values, types, and operators."
           , "- `docs_module` with an exact module name for full module docs."
           , "- `docs_value` with `Module.name` for a specific declaration."
           ]
      )

section :: Text -> [Text] -> [Text]
section title items =
  ["## " <> title, ""] ++ (if null items then ["- None"] else items) ++ [""]

moduleLink :: Docs.Module -> Text
moduleLink (Docs.Module name _ _ _ _ _) =
  let t = Text.pack (Name.toChars name)
  in "- [`" <> t <> "`](elm://docs/module/" <> t <> ")"

renderDocsSearch :: Text -> [(FilePath, Docs.Module)] -> [(Pkg.Name, [Docs.Module])] -> Text
renderDocsSearch query localDocs packageDocsList =
  let q = Text.toLower query
      localResults = concatMap (searchLocalModule q) localDocs
      packageResults = concatMap (\(pkgName, docsMods) -> concatMap (searchPackageModule q pkgName) docsMods) packageDocsList
      results = take 50 (localResults ++ packageResults)
  in Text.intercalate "\n" (["# Docs Search", "", "Query: `" <> query <> "`", ""] ++ if null results then ["No matches."] else results)

searchLocalModule :: Text -> (FilePath, Docs.Module) -> [Text]
searchLocalModule q (fp, modu) =
  searchModule q (Just (Text.pack fp)) Nothing modu

searchPackageModule :: Text -> Pkg.Name -> Docs.Module -> [Text]
searchPackageModule q pkgName modu =
  searchModule q Nothing (Just (Text.pack (Pkg.toChars pkgName))) modu

searchModule :: Text -> Maybe Text -> Maybe Text -> Docs.Module -> [Text]
searchModule q _maybePath maybePkg (Docs.Module modName comment unions aliases values binops) =
  let modTxt = Text.pack (Name.toChars modName)
      origin = maybe "local" (\p -> "package " <> p) maybePkg
      moduleHit = if matchesText q modTxt || matchesText q (commentText comment)
        then ["- module `" <> modTxt <> "` (" <> origin <> ") -> `docs_module`"]
        else []
      declaration kind name docs =
        let nameTxt = Text.pack (Name.toChars name)
            full = modTxt <> "." <> nameTxt
        in if matchesText q nameTxt || matchesText q full || matchesText q docs
          then Just ("- " <> kind <> " `" <> full <> "` (" <> origin <> ") -> `docs_value`")
          else Nothing
      unionHits = Maybe.mapMaybe (\(n, Docs.Union c _ _) -> declaration "type" n (commentText c)) (Map.toList unions)
      aliasHits = Maybe.mapMaybe (\(n, Docs.Alias c _ _) -> declaration "alias" n (commentText c)) (Map.toList aliases)
      valueHits = Maybe.mapMaybe (\(n, Docs.Value c _) -> declaration "value" n (commentText c)) (Map.toList values)
      binopHits = Maybe.mapMaybe (\(n, Docs.Binop c _ _ _) -> declaration "operator" n (commentText c)) (Map.toList binops)
  in moduleHit ++ unionHits ++ aliasHits ++ valueHits ++ binopHits

matchesText :: Text -> Text -> Bool
matchesText needle haystack =
  Text.toLower needle `Text.isInfixOf` Text.toLower haystack

commentText :: Docs.Comment -> Text
commentText comment =
  Text.pack (Json.toChars comment)

splitModuleAndValue :: Text -> Maybe (Name.Name, Name.Name)
splitModuleAndValue t =
  let parts = Text.splitOn "." t
  in case parts of
       [] -> Nothing
       [_] -> Nothing
       _ ->
         let valTxt = last parts
             modTxt = Text.intercalate "." (init parts)
         in Just (Name.fromChars (Text.unpack modTxt), Name.fromChars (Text.unpack valTxt))
