{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Watchtower.Server.MCP (serve) where

{-
Can test via https://modelcontextprotocol.io/docs/tools/inspector
  
Run: `npx @modelcontextprotocol/inspector`.

In the app, select STDIO and put `elm-dev` with the arg `mcp` and then hit connect.

-}

import Control.Exception (SomeException)
import qualified Control.Exception as Exception
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Scientific as Scientific
import qualified Elm.Package as Pkg
import qualified Ext.Common
import qualified Ext.CompileMode
import qualified Ext.CompileProxy
import qualified Ext.Dev.Project
import qualified Ext.Test.Install as TestInstall
import qualified Ext.Test.Result.Report as TestReport
import qualified Ext.Test.Runner as TestRunner
import qualified Gen.Commands.Init as GenInit
import qualified Gen.Config as Config
import qualified Gen.Generate
import qualified Gen.Templates as Templates
import GHC.Generics
import qualified Stuff
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.Process as Process
import qualified Data.Map as Map
import qualified Data.Utf8 as Utf8
import qualified Watchtower.Live as Live
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Server.JSONRPC as JSONRPC
import qualified Watchtower.Server.MCP.Protocol as MCP
import qualified Watchtower.Server.MCP.Uri as Uri
import qualified Watchtower.Server.MCP.ProjectLookup as ProjectLookup
import qualified Watchtower.State.Compile as StateCompile
import qualified Watchtower.Server.LSP as LSP


availableTools :: [MCP.Tool]
availableTools =
  [ toolInit
  , toolCompile
  , toolInstall
  , toolAddPage
  , toolAddStore
  , toolAddEffect
  , toolAddListener
  , toolAddTheme
  , toolTestInit
  , toolTestRun
  ]

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
    , "properties" .= JSON.object
        [ projectIdSchema ]
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
    fields = projectIdSchema : extraPairs
    requiredFields :: [Text]
    requiredFields = fmap fst extras
  in
  JSON.object
    [ "type" .= ("object" :: Text)
    , "properties" .= JSON.object fields
    , "required" .= requiredFields
    ]

toolInit :: MCP.Tool
toolInit = MCP.Tool
  { MCP.toolName = "init"
  , MCP.toolDescription = "Create a new Elm project (elm-dev prefab) in the given directory."
  , MCP.toolInputSchema =
      JSON.object
        [ "type" .= ("object" :: Text)
        , "properties" .= JSON.object
            [ rootDirSchema
            ]
        , "required" .= (["dir"] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args _state _emit -> do
      case requireStringArg "dir" args of
        Left e -> pure (errTxt (Text.pack e))
        Right dir -> do
          -- Run generator init in target dir
          r <- Exception.try (withDir dir (GenInit.run () Nothing)) :: IO (Either SomeException ())
          case r of
            Left _ -> pure (errTxt "Failed to initialize project")
            Right _ -> pure (ok "Initialized project")
  }

-- compile
toolCompile :: MCP.Tool
toolCompile = MCP.Tool
  { MCP.toolName = "compile"
  , MCP.toolDescription = "Compile the Elm project (typecheck/build)."
  , MCP.toolInputSchema = schemaProject
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit -> do
      let mPid = getIntArg args "projectId"
      selection <- ProjectLookup.resolveProject mPid state
      case selection of
        Left msg -> pure (errTxt msg)
        Right projCache -> do
          compileResult <- StateCompile.compile state projCache []
          case compileResult of
            Left _ -> pure (errTxt "Compilation failed")
            Right _ -> pure (ok "Compiled successfully")
  }

-- install elm package
toolInstall :: MCP.Tool
toolInstall = MCP.Tool
  { MCP.toolName = "install"
  , MCP.toolDescription = "Install an Elm package (author/project)."
  , MCP.toolInputSchema = schemaProjectPlus [("package", "Elm package name, e.g. elm/json")]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit -> do
      case requireStringArg "package" args of
        Left e -> pure (errTxt (Text.pack e))
        Right pkg -> do
          let mPid = getIntArg args "projectId"
          selection <- ProjectLookup.resolveProject mPid state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              (code, out, errOut) <- withDir dir (Process.readProcessWithExitCode "elm" ["install", pkg, "--yes"] "")
              let body = if null out then errOut else out
              pure (ok (Text.pack body))
  }

-- add_page
toolAddPage :: MCP.Tool
toolAddPage = MCP.Tool
  { MCP.toolName = "add_page"
  , MCP.toolDescription = "Add a new page to the project."
  , MCP.toolInputSchema = schemaProjectPlus [("url", "Page url path, e.g. /about")]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit -> do
      case requireStringArg "url" args of
        Left e -> pure (errTxt (Text.pack e))
        Right url -> do
          let mPid = getIntArg args "projectId"
          selection <- ProjectLookup.resolveProject mPid state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              withDir dir $ do
                cfg <- Gen.Generate.readConfigOrFail
                let name = urlToElmModuleName url
                Templates.write "Page" Config.elmSrc name
                let urlText = Text.pack url
                let updated = case Config.configApp cfg of
                      Nothing -> cfg { Config.configApp = Just (Config.AppConfig (Map.singleton (Text.pack name) (Config.PageConfig urlText [] False))) }
                      Just appCfg -> cfg { Config.configApp = Just (appCfg { Config.appPages = Map.insert (Text.pack name) (Config.PageConfig urlText [] False) (Config.appPages appCfg) }) }
                LBS.writeFile "elm.generate.json" (Aeson.encodePretty updated)
                pure (ok (Text.pack ("Created page " ++ name)))
  }

-- add_store
toolAddStore :: MCP.Tool
toolAddStore = MCP.Tool
  { MCP.toolName = "add_store"
  , MCP.toolDescription = "Add a new store module."
  , MCP.toolInputSchema = schemaProjectPlus [("module", "Module name, e.g. App.Store.Session")]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit -> do
      case requireStringArg "module" args of
        Left e -> pure (errTxt (Text.pack e))
        Right modul -> do
          let mPid = getIntArg args "projectId"
          selection <- ProjectLookup.resolveProject mPid state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              withDir dir $ do
                _ <- Gen.Generate.readConfigOrFail
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
  , MCP.call = \args state _emit -> do
      case requireStringArg "module" args of
        Left e -> pure (errTxt (Text.pack e))
        Right modul -> do
          let mPid = getIntArg args "projectId"
          selection <- ProjectLookup.resolveProject mPid state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              withDir dir $ do
                _ <- Gen.Generate.readConfigOrFail
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
  , MCP.call = \args state _emit -> do
      case requireStringArg "module" args of
        Left e -> pure (errTxt (Text.pack e))
        Right modul -> do
          let mPid = getIntArg args "projectId"
          selection <- ProjectLookup.resolveProject mPid state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              withDir dir $ do
                _ <- Gen.Generate.readConfigOrFail
                Templates.write "Listen" Config.elmSrc modul
                pure (ok (Text.pack ("Created listener " ++ modul)))
  }

-- add_theme
toolAddTheme :: MCP.Tool
toolAddTheme = MCP.Tool
  { MCP.toolName = "add_theme"
  , MCP.toolDescription = "Add a theme section to elm.generate.json if not present."
  , MCP.toolInputSchema = schemaProject
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit -> do
      let mPid = getIntArg args "projectId"
      selection <- ProjectLookup.resolveProject mPid state
      case selection of
        Left msg -> pure (errTxt msg)
        Right (Client.ProjectCache proj _ _ _ _) -> do
          let dir = Ext.Dev.Project.getRoot proj
          withDir dir $ do
            cfg <- Gen.Generate.readConfigOrFail
            case Config.configTheme cfg of
              Just _ -> pure (errTxt "Theme already exists")
              Nothing -> do
                let updated = cfg { Config.configTheme = Nothing }
                LBS.writeFile "elm.generate.json" (Aeson.encodePretty updated)
                pure (ok "Added theme")
  }

-- test_init
toolTestInit :: MCP.Tool
toolTestInit = MCP.Tool
  { MCP.toolName = "test_init"
  , MCP.toolDescription = "Install elm-explorations/test and scaffold tests directory."
  , MCP.toolInputSchema = schemaProject
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit -> do
      let mPid = getIntArg args "projectId"
      selection <- ProjectLookup.resolveProject mPid state
      case selection of
        Left msg -> pure (errTxt msg)
        Right (Client.ProjectCache proj _ _ _ _) -> do
          let dir = Ext.Dev.Project.getRoot proj
          withDir dir $ do
            let pkg = Pkg.toName (Utf8.fromChars "elm-explorations") "test"
            _ <- TestInstall.installTestDependency pkg
            Dir.createDirectoryIfMissing True ("tests" :: FilePath)
            pure (ok "Initialized tests")
  }

-- test_run
toolTestRun :: MCP.Tool
toolTestRun = MCP.Tool
  { MCP.toolName = "test_run"
  , MCP.toolDescription = "Discover, compile, and run Elm tests."
  , MCP.toolInputSchema = schemaProject
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit -> do
      let mPid = getIntArg args "projectId"
      selection <- ProjectLookup.resolveProject mPid state
      case selection of
        Left msg -> pure (errTxt msg)
        Right (Client.ProjectCache proj _ _ _ _) -> do
          let dir = Ext.Dev.Project.getRoot proj
          withDir dir $ do
            r <- TestRunner.run "."
            case r of
              Left msg -> pure (errTxt (Text.pack msg))
              Right reports -> do
                let rendered = TestReport.renderReports reports
                pure (ok (Text.pack rendered))
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
  , resourceArchitecture
  , resourceDiagnostics
  , resourceModuleGraph
  , resourcePackageDocs
  , resourceModuleDocs
  , resourceValueDocs
  , resourceTestStatus
  ]

-- Toplevel resource values with inline readers

resourceOverview :: MCP.Resource
resourceOverview =
  let pat = Uri.pattern "elm" [Uri.s "overview"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Project Overview"
      , MCP.resourceDescription = Just "Overview for a project (?root=...)"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.High Nothing)
      , MCP.read = \req _state _emit -> do
          -- Placeholder overview for now
          let val = JSON.object [ "message" .= ("coming soon" :: Text) ]
          pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }

resourceArchitecture :: MCP.Resource
resourceArchitecture =
  let pat = Uri.pattern "file" [Uri.s "architecture"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Prefab Architecture"
      , MCP.resourceDescription = Just "Describes how elm-prefab works"
      , MCP.resourceMimeType = Just "text/markdown"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req _state _emit -> do
          let md = "# Elm Prefab Architecture\n\n(coming soon)"
          pure (MCP.ReadResourceResponse [ MCP.markdown req md ])
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
resolveProject queryParams state = do
  let maybeProjectId = lookupInt queryParams "projectId"
  ProjectLookup.resolveProject maybeProjectId state


resourceDiagnostics :: MCP.Resource
resourceDiagnostics =
  let pat = Uri.pattern "elm" [Uri.s "diagnostics"] ["projectId"]
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Diagnostics"
      , MCP.resourceDescription = Just "Project diagnostics (?projectId=...)"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.High Nothing)
      , MCP.read = \req state _emit -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch _pathVals queryParams) -> do
              -- projectFound <- resolveProject queryParams state
              -- case projectFound of
              --   Left msg -> do
              --     let val = JSON.object [ "error" .= msg ]
              --     pure (MCP.ReadResourceResponse [ MCP.json req val ])
              --   Right (Client.ProjectCache proj _ _ _ _) -> do
              --     allInfos <- Client.getAllFileInfos state
              --     let projectFiles = fmap fst $ filter (\(p, _) -> Ext.Dev.Project.contains p proj) (Map.toList allInfos)
              --     diagsLists <- mapM (\p -> LSP.getDiagnosticsForUri state (Text.concat ["file://", Text.pack p])) projectFiles
              --     let items = concat diagsLists
              --     let val = JSON.object [ "kind" .= ("project" :: Text)
              --                           , "items" .= items
              --                           ]
              --     pure (MCP.ReadResourceResponse [ MCP.json req val ])
               -- Placeholder
              let val = JSON.object [ "message" .= ("coming soon" :: Text) ]
              pure (MCP.ReadResourceResponse [ MCP.json req val ])
            Nothing -> do
              let val = JSON.object [ "error" .= ("bad uri" :: Text) ]
              pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }


resourceModuleGraph :: MCP.Resource
resourceModuleGraph =
  let pat = Uri.pattern "elm" [Uri.s "graph/module/", Uri.var "Module"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Module Graph"
      , MCP.resourceDescription = Just "Imports/exports/dependents (?root=...)"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req _state _emit -> do
          -- Placeholder
          let val = JSON.object [ "message" .= ("coming soon" :: Text) ]
          pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }

resourcePackageDocs :: MCP.Resource
resourcePackageDocs =
  let pat = Uri.pattern "elm" [Uri.s "docs/package/", Uri.var "pkg"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Package Docs"
      , MCP.resourceDescription = Just "Docs for a package"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req _state _emit -> do
          -- Placeholder
          let val = JSON.object [ "message" .= ("coming soon" :: Text) ]
          pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }

resourceModuleDocs :: MCP.Resource
resourceModuleDocs =
  let pat = Uri.pattern "elm" [Uri.s "docs/module/", Uri.var "Module"] ["root"]
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Module Docs"
      , MCP.resourceDescription = Just "Docs for a module"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req _state _emit -> do
          -- Placeholder
          let val = JSON.object [ "message" .= ("coming soon" :: Text) ]
          pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }

resourceValueDocs :: MCP.Resource
resourceValueDocs =
  let pat = Uri.pattern "elm" [Uri.s "docs/value/", Uri.var "Module.name"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Value Docs"
      , MCP.resourceDescription = Just "Docs for a value"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req _state _emit -> do
          -- Placeholder
          let val = JSON.object [ "message" .= ("coming soon" :: Text) ]
          pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }

resourceTestStatus :: MCP.Resource
resourceTestStatus =
  let pat = Uri.pattern "elm" [Uri.s "tests/status"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Test Status"
      , MCP.resourceDescription = Just "Status of tests (?projectId=...)"
      , MCP.resourceMimeType = Just "text/plain"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Low Nothing)
      , MCP.read = \req _state _emit -> do
          pure (MCP.ReadResourceResponse [ MCP.markdown req "Tests: (coming soon)" ])
      }

-- * Available Prompts

availablePrompts :: [MCP.Prompt]
availablePrompts = []

-- (old stub readers removed; read handlers are inline in each resource)


-- | Main MCP server handler
serve :: Live.State -> JSONRPC.EventEmitter -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)
serve state emitter req = do
  MCP.serve availableTools availableResources availablePrompts state emitter req