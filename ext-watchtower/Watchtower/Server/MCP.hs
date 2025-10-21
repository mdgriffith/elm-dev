
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
import qualified Control.Concurrent.STM
import Data.Aeson ((.=))
import qualified Data.Name as Name
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
import qualified Elm.Docs as Docs
import qualified Ext.Common
import qualified Ext.CompileMode
import qualified Ext.CompileProxy
import qualified Ext.FileProxy
import qualified Ext.Dev.Project
import qualified Ext.Test.Install as TestInstall
import qualified Ext.Test.Result.Report as TestReport
import qualified Ext.Test.Runner as TestRunner
import qualified Ext.Reporting.Error
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
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import qualified Watchtower.Live as Live
import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Server.JSONRPC as JSONRPC
import qualified Watchtower.Server.MCP.Protocol as MCP
import qualified Watchtower.Server.MCP.Uri as Uri
import qualified Watchtower.Server.MCP.ProjectLookup as ProjectLookup
import qualified Data.Text as T
import qualified Watchtower.Server.MCP.Guides as Guides
import qualified Watchtower.State.Compile
import qualified Watchtower.Server.LSP as LSP
import qualified Watchtower.Server.LSP.Helpers as Helpers
import qualified Ext.Encode
import qualified Elm.Details
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Exit as Exit
import qualified Reporting.Render.Code as RenderCode
import qualified Reporting.Error as Err
import qualified Data.NonEmptyList as NE


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
  , toolProjectList
  , toolProjectSet
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
  , MCP.call = \args _state _emit connId -> do
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
  , MCP.call = \args state _emit connId -> do
      let mPid = getIntArg args "projectId"
      selection <- ProjectLookup.resolveProjectFromSession mPid connId state
      case selection of
        Left msg -> pure (errTxt msg)
        Right projCache -> do
          compileResult <- Watchtower.State.Compile.compile state projCache []
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
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "package" args of
        Left e -> pure (errTxt (Text.pack e))
        Right pkg -> do
          let mPid = getIntArg args "projectId"
          
          selection <- ProjectLookup.resolveProjectFromSession mPid connId state
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
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "url" args of
        Left e -> pure (errTxt (Text.pack e))
        Right url -> do
          let mPid = getIntArg args "projectId"
          
          selection <- ProjectLookup.resolveProjectFromSession mPid connId state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              withDir dir $ do
                cfg <- Gen.Generate.readConfigOrFail
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
          let mPid = getIntArg args "projectId"
          selection <- ProjectLookup.resolveProjectFromSession mPid connId state
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
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "module" args of
        Left e -> pure (errTxt (Text.pack e))
        Right modul -> do
          let mPid = getIntArg args "projectId"
          
          selection <- ProjectLookup.resolveProjectFromSession mPid connId state
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
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "module" args of
        Left e -> pure (errTxt (Text.pack e))
        Right modul -> do
          let mPid = getIntArg args "projectId"
          selection <- ProjectLookup.resolveProjectFromSession mPid connId state
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
  , MCP.toolDescription = "Add a theme section to elm.dev.json if not present."
  , MCP.toolInputSchema = schemaProject
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      let mPid = getIntArg args "projectId"
      selection <- ProjectLookup.resolveProjectFromSession mPid connId state
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
                LBS.writeFile "elm.dev.json" (Aeson.encodePretty updated)
                pure (ok "Added theme")
  }

-- test_init
toolTestInit :: MCP.Tool
toolTestInit = MCP.Tool
  { MCP.toolName = "test_init"
  , MCP.toolDescription = "Install elm-explorations/test and scaffold tests directory."
  , MCP.toolInputSchema = schemaProject
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      let mPid = getIntArg args "projectId"
      selection <- ProjectLookup.resolveProjectFromSession mPid connId state
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
  , MCP.call = \args state _emit connId -> do
      let mPid = getIntArg args "projectId"
      selection <- ProjectLookup.resolveProjectFromSession mPid connId state
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

-- list projects
toolProjectList :: MCP.Tool
toolProjectList = MCP.Tool
  { MCP.toolName = "project_list"
  , MCP.toolDescription = "List known projects with ids and roots."
  , MCP.toolInputSchema = JSON.object [ "type" .= ("object" :: Text) ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \_args state _emit connId -> do
      let (Client.State _ mProjects _ _ _ _) = state
      projects <- Control.Concurrent.STM.readTVarIO mProjects
      let items = fmap (\(Client.ProjectCache proj _ _ _ _) -> JSON.object
                        [ "id" .= Ext.Dev.Project._shortId proj
                        , "root" .= Ext.Dev.Project.getRoot proj
                        ]) projects
      pure (ok (Text.pack (show (length items))) ) >> pure (MCP.ToolCallResponse [MCP.ToolResponseStructured Nothing (JSON.object ["projects" .= items])])
  }

-- set current project
toolProjectSet :: MCP.Tool
toolProjectSet = MCP.Tool
  { MCP.toolName = "project_set"
  , MCP.toolDescription = "Set the current project for this connection."
  , MCP.toolInputSchema = JSON.object [ "type" .= ("object" :: Text), "properties" .= JSON.object [ "projectId" .= JSON.object [ "type" .= ("integer" :: Text) ] ], "required" .= (["projectId"] :: [Text]) ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      case getIntArg args "projectId" of
        Nothing -> pure (errTxt "Missing projectId")
        Just pid -> do
          let (Client.State _ mProjects _ _ _ _) = state
          projects <- Control.Concurrent.STM.readTVarIO mProjects
          case filter (\(Client.ProjectCache proj _ _ _ _) -> Ext.Dev.Project._shortId proj == pid) projects of
            (Client.ProjectCache _proj _ _ _ _ : _) -> do
              okSet <- Client.setFocusedProjectId state connId pid
              if okSet
                then pure (ok (Text.pack ("Focused project set to " ++ show pid)))
                else pure (errTxt "Project id not found")
            _ -> do
              let known = ProjectLookup.listKnownProjectsText projects
              pure (errTxt (Text.concat ["Project id not found: ", Text.pack (show pid), "\nKnown projects:\n", known]))
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
  , resourceFileDocs
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
      , MCP.resourceName = "Elm Dev Project Overview"
      , MCP.resourceDescription = Just "Overview for an Elm project. Examples: elm://overview, file://architecture"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceAssistant] MCP.High Nothing)
      , MCP.read = \req state _emit connId -> do
          projectFound <- ProjectLookup.resolveProjectFromSession Nothing connId state
          case projectFound of
            Left msg -> do
              let val = JSON.object [ "error" .= msg ]
              pure (MCP.ReadResourceResponse [ MCP.json req val ])
            Right pc@(Client.ProjectCache proj _ _ mCompileResult mTestResults) -> do
              -- ensure we have an up-to-date compile result
              _ <- Watchtower.State.Compile.compile state pc []
              currentResult <- Control.Concurrent.STM.readTVarIO mCompileResult

              -- Build compilation summary
              let compilationVal = case currentResult of
                    Client.Success _ ->
                      JSON.object [ "status" .= ("ok" :: Text) ]
                    Client.NotCompiled ->
                      JSON.object [ "status" .= ("compiling" :: Text) ]
                    Client.Error (Client.GenerationError errMsg) ->
                      JSON.object [ "status" .= ("generationError" :: Text)
                                  , "message" .= errMsg
                                  ]
                    Client.Error (Client.ReactorError reactor) ->
                      case reactor of
                        Exit.ReactorBadBuild (Exit.BuildBadModules _root firstMod otherMods) ->
                          let mods = firstMod : otherMods
                              moduleSummaries =
                                fmap
                                  (\(Err.Module name path _time source err) ->
                                     let reports = NE.toList (Ext.Reporting.Error.toReports (RenderCode.toSource source) err)
                                         cnt :: Int
                                         cnt = length reports
                                         obj = JSON.object [ "module" .= Text.pack (ModuleName.toChars name)
                                                           , "path" .= path
                                                           , "count" .= cnt
                                                           ]
                                     in (cnt, obj)
                                  )
                                  mods
                              perModule = fmap snd moduleSummaries
                              totalErrors :: Int
                              totalErrors = sum (fmap fst moduleSummaries)
                          in JSON.object [ "status" .= ("errors" :: Text)
                                         , "totalErrors" .= totalErrors
                                         , "byModule" .= perModule
                                         ]
                        _ -> JSON.object [ "status" .= ("error" :: Text) ]

              -- Test summary (if any)
              mTests <- Control.Concurrent.STM.readTVarIO mTestResults
              let testField = case mTests of
                    Nothing -> []
                    Just (Client.TestResults total passed failed _failures) ->
                      [ "tests" .= JSON.object [ "total" .= total
                                               , "passed" .= passed
                                               , "failed" .= failed
                                               ]
                      ]

              -- Architecture note if this is an elm-dev project
              isElmDev <- Ext.FileProxy.exists (Ext.Dev.Project.getRoot proj </> "elm.dev.json")
              let archNoteField = if isElmDev then
                                    [ "note" .= ("This project is using the Elm Dev App Architecture.  Read the `architecture` resource for further details." :: Text) ]
                                  else []

              let capabilitiesVal = JSON.object
                    [ "fileScoped" .= ([ "diagnostics", "docs.file", "hover", "references" ] :: [Text])
                    , "projectScoped" .= ([ "compile", "diagnostics", "graph.module", "tests" ] :: [Text])
                    ]

              let val = JSON.object (
                          [ "kind" .= ("overview" :: Text)
                          , "language" .= ("elm" :: Text)
                          , "capabilities" .= capabilitiesVal
                          , "projectId" .= Ext.Dev.Project._shortId proj
                          , "root" .= Ext.Dev.Project.getRoot proj
                          , "entrypoints" .= NE.toList (Ext.Dev.Project._entrypoints proj)
                          , "compilation" .= compilationVal
                          ] ++ testField ++ archNoteField
                        )
              pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }

resourceArchitecture :: MCP.Resource
resourceArchitecture =
  let pat = Uri.pattern "file" [Uri.s "architecture"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm Dev Project Architecture"
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
              hasCfg <- Ext.FileProxy.exists cfgPath
              if hasCfg then
                pure (MCP.ReadResourceResponse [ MCP.markdown req Guides.architectureMd ])
              else do
                let msg = ("This is a standard Elm app using the Elm Architecture." :: Text)
                pure (MCP.ReadResourceResponse [ MCP.markdown req msg ])
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
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req state _emit connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch _pathVals queryParams) -> do
              let mFile = Map.lookup "file" queryParams

              projectFound <- ProjectLookup.resolveProjectFromSession Nothing connId state
              case projectFound of
                Left msg -> do
                  let val = JSON.object [ "error" .= msg ]
                  pure (MCP.ReadResourceResponse [ MCP.json req val ])
                Right pc@(Client.ProjectCache proj _ _ _ _) -> do
                  _ <- Watchtower.State.Compile.compile state pc []
                  case mFile of
                    Just fileTxt -> do
                      let filePath = Text.unpack fileTxt
                      diagErrors <- Helpers.getDiagnosticsForProject state pc (Just filePath)
                      (_loc, warns) <- Helpers.getWarningsForFile state filePath
                      let warnDiags = concatMap Helpers.warningToUnusedDiagnostic warns
                      let items = diagErrors ++ warnDiags
                      let val = JSON.object [ "kind" .= ("file" :: Text)
                                            , "file" .= fileTxt
                                            , "items" .= items
                                            ]
                      pure (MCP.ReadResourceResponse [ MCP.json req val ])
                    Nothing -> do
                      allInfos <- Client.getAllFileInfos state
                      diagErrors <- Helpers.getDiagnosticsForProject state pc Nothing
                      let projectFiles = fmap fst $ filter (\(p, _) -> Ext.Dev.Project.contains p proj) (Map.toList allInfos)
                      warnDiagLists <- mapM (\p -> do { (_loc, warns) <- Helpers.getWarningsForFile state p; pure (concatMap Helpers.warningToUnusedDiagnostic warns) }) projectFiles
                      let items = diagErrors ++ concat warnDiagLists
                      let val = JSON.object [ "kind" .= ("project" :: Text)
                                            , "items" .= items
                                            ]
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
  let pat = Uri.pattern "elm" [Uri.s "docs/package/", Uri.var "pkg"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm Package Docs"
      , MCP.resourceDescription = Just "Docs for a package (elm://docs/package/{author/project}), e.g. elm://docs/package/elm/json"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req state _emit connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch pathVals _q) -> do
              let mPkgTxt = Map.lookup "pkg" pathVals
              case mPkgTxt of
                Nothing -> do
                  let val = JSON.object [ "error" .= ("missing package" :: Text) ]
                  pure (MCP.ReadResourceResponse [ MCP.json req val ])
                Just pkgTxt -> do
                  let parts = Text.splitOn "/" pkgTxt
                  case parts of
                    [author, project] -> do
                      let pkgName = Pkg.toName (Utf8.fromChars (Text.unpack author)) (Text.unpack project)
                      pkgs <- Control.Concurrent.STM.readTVarIO (Client.packages state)
                      case Map.lookup pkgName pkgs of
                        Nothing -> do
                          let val = JSON.object
                                    [ "error" .= ("package not found" :: Text)
                                    , "package" .= Text.pack (Pkg.toChars pkgName)
                                    ]
                          pure (MCP.ReadResourceResponse [ MCP.json req val ])

                        Just (Client.PackageInfo { Client.readme = r, Client.packageModules = mods }) -> do
                          let modulesDocs = [ Client.packageModuleDocs pm | pm <- Map.elems mods ]
                              val = JSON.object
                                      [ "kind" .= ("package" :: Text)
                                      , "package" .= Text.pack (Pkg.toChars pkgName)
                                      , "readme" .= maybe JSON.Null (JSON.String . Text.pack) r
                                      , "docs" .= Ext.Encode.docs (Docs.toDict modulesDocs)
                                      ]
                          pure (MCP.ReadResourceResponse [ MCP.json req val ])
                    _ -> do
                      let val = JSON.object [ "error" .= ("bad package id" :: Text) ]
                      pure (MCP.ReadResourceResponse [ MCP.json req val ])
            Nothing -> do
              let val = JSON.object [ "error" .= ("bad uri" :: Text) ]
              pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }

resourceModuleDocs :: MCP.Resource
resourceModuleDocs =
  let pat = Uri.pattern "elm" [Uri.s "docs/module/", Uri.var "Module"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm Module Docs"
      , MCP.resourceDescription = Just "Documentation for a module (elm://docs/module/{Module}), e.g. elm://docs/module/App.Main"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req state _emit connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch pathVals _q) -> do
              case Map.lookup "Module" pathVals of
                Nothing -> do
                  let val = JSON.object [ "error" .= ("missing module" :: Text) ]
                  pure (MCP.ReadResourceResponse [ MCP.json req val ])
                Just moduleTxt -> do
                  let wantName = Name.fromChars (Text.unpack moduleTxt)
                  r <- resolveModuleSpec state (ModuleByName wantName)
                  case r of
                    Right (ModuleResolved _ maybePath maybePkg docsMod) -> do
                      let base =
                            [ "kind" .= ("module" :: Text)
                            , "module" .= moduleTxt
                            , "docs" .= Ext.Encode.docs (Docs.toDict [docsMod])
                            ]
                          withPath = maybe base (\fp -> ("path" .= Text.pack fp) : base) maybePath
                          withPkg = case maybePkg of
                                      Just pkgName -> ("package" .= Text.pack (Pkg.toChars pkgName))
                                                     : ("packageResource" .= Text.concat ["elm://docs/package/", Text.pack (Pkg.toChars pkgName)])
                                                     : withPath
                                      Nothing -> withPath
                          val = JSON.object withPkg
                      pure (MCP.ReadResourceResponse [ MCP.json req val ])
                    Left _ -> do
                      let val = JSON.object [ "error" .= ("module not found" :: Text)
                                            , "module" .= moduleTxt
                                            ]
                      pure (MCP.ReadResourceResponse [ MCP.json req val ])
            Nothing -> do
              let val = JSON.object [ "error" .= ("bad uri" :: Text) ]
              pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }


resourceFileDocs :: MCP.Resource
resourceFileDocs =
  let pat = Uri.pattern "elm" [Uri.s "docs/file"] ["file"]
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Elm File Docs"
      , MCP.resourceDescription = Just "Docs for a specific file (elm://docs/file?file=/abs/path/to/File.elm)"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req state _emit _connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch _pathVals queryParams) -> do
              case Map.lookup "file" queryParams of
                Nothing -> do
                  let val = JSON.object [ "error" .= ("missing file" :: Text) ]
                  pure (MCP.ReadResourceResponse [ MCP.json req val ])
                Just fileTxt -> do
                  let filePath = Text.unpack fileTxt
                  r <- resolveModuleSpec state (ModuleByFilePath filePath)
                  case r of
                    Right (ModuleResolved _ _ _ docsMod) -> do
                      let val = JSON.object
                                [ "kind" .= ("fileDocs" :: Text)
                                , "file" .= fileTxt
                                , "docs" .= Ext.Encode.docs (Docs.toDict [docsMod])
                                ]
                      pure (MCP.ReadResourceResponse [ MCP.json req val ])
                    Left msg -> do
                      let val = JSON.object [ "error" .= msg, "file" .= fileTxt ]
                      pure (MCP.ReadResourceResponse [ MCP.json req val ])
            Nothing -> do
              let val = JSON.object [ "error" .= ("bad uri" :: Text) ]
              pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }



resourceValueDocs :: MCP.Resource
resourceValueDocs =
  let pat = Uri.pattern "elm" [Uri.s "docs/value/", Uri.var "Module.name"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Value Docs"
      , MCP.resourceDescription = Just "Docs for a value (elm://docs/value/{Module.name}), e.g. elm://docs/value/List.map"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Medium Nothing)
      , MCP.read = \req state _emit connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch pathVals _q) -> do
              case Map.lookup "Module.name" pathVals >>= splitModuleAndValue of
                Nothing -> do
                  let val = JSON.object [ "error" .= ("missing module or value" :: Text) ]
                  pure (MCP.ReadResourceResponse [ MCP.json req val ])
                Just (modName, valName) -> do
                  -- Try local first
                  mLocal <- findLocalModuleDocs state modName
                  case mLocal of
                    Just (fp, m) -> do
                      let filtered = filterModuleForValue valName m
                          note = if Map.null (case filtered of Docs.Module _ _ _ _ vs _ -> vs)
                                 then Just ("value not found in local module" :: Text) else Nothing
                          base =
                            [ "module" .= Text.pack (Name.toChars modName)
                            , "path" .= Text.pack fp
                            , "docs" .= Ext.Encode.docs (Docs.toDict [filtered])
                            ]
                          val = JSON.object (maybe base (\n -> ("note" .= n):base) note)
                      pure (MCP.ReadResourceResponse [ MCP.json req val ])
                    Nothing -> do
                      -- packages
                      pkgs <- Control.Concurrent.STM.readTVarIO (Client.packages state)
                      let found = [ (pkg, m)
                                  | (pkg, Client.PackageInfo { Client.packageModules = mods }) <- Map.toList pkgs
                                  , Just (Client.PackageModule { Client.packageModuleDocs = m }) <- [Map.lookup modName mods]
                                  ]
                      case found of
                        (pkgName, m):_ -> do
                          let filtered = filterModuleForValue valName m
                              note = if Map.null (case filtered of Docs.Module _ _ _ _ vs _ -> vs)
                                     then Just ("value not found in package module" :: Text) else Nothing
                              pkgUri = Text.concat ["elm://docs/package/", Text.pack (Pkg.toChars pkgName)]
                              base =
                                [ "module" .= Text.pack (Name.toChars modName)
                                , "name" .= Text.pack (Name.toChars valName)
                                , "package" .= Text.pack (Pkg.toChars pkgName)
                                , "packageResource" .= pkgUri
                                , "docs" .= Ext.Encode.docs (Docs.toDict [filtered])
                                ]
                              val = JSON.object (maybe base (\n -> ("note" .= n):base) note)
                          pure (MCP.ReadResourceResponse [ MCP.json req val ])
                        _ -> do
                          let val = JSON.object [ "error" .= ("value not found" :: Text)
                                                , "module" .= Text.pack (Name.toChars modName)
                                                , "name" .= Text.pack (Name.toChars valName)
                                                ]
                          pure (MCP.ReadResourceResponse [ MCP.json req val ])
            Nothing -> do
              let val = JSON.object [ "error" .= ("bad uri" :: Text) ]
              pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }

resourceTestStatus :: MCP.Resource
resourceTestStatus =
  let pat = Uri.pattern "elm" [Uri.s "tests/status"] []
  in MCP.Resource
      { MCP.resourceUri = pat
      , MCP.resourceName = "Test Status"
      , MCP.resourceDescription = Just "Status of tests for the current project (elm://tests/status)"
      , MCP.resourceMimeType = Just "application/json"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceUser] MCP.Low Nothing)
      , MCP.read = \req state _emit connId -> do
          case Uri.match pat (MCP.readResourceUri req) of
            Just (Uri.PatternMatch _pathVals _queryParams) -> do
              projectFound <- ProjectLookup.resolveProjectFromSession Nothing connId state
              case projectFound of
                Left msg -> do
                  let val = JSON.object [ "error" .= msg ]
                  pure (MCP.ReadResourceResponse [ MCP.json req val ])
                Right (Client.ProjectCache proj _ _ _ mResults) -> do
                  m <- Control.Concurrent.STM.readTVarIO mResults
                  case m of
                    Nothing -> do
                      let val = JSON.object
                                [ "error" .= ("no test results" :: Text)
                                , "hint" .= ("Run the test_run tool for this projectId" :: Text)
                                ]
                      pure (MCP.ReadResourceResponse [ MCP.json req val ])
                    Just (Client.TestResults total passed failed failures) -> do
                      let val = JSON.object
                                [ "kind"   .= ("tests" :: Text)
                                , "projectId" .= Ext.Dev.Project._shortId proj
                                , "root"   .= Ext.Dev.Project.getRoot proj
                                , "total"  .= total
                                , "passed" .= passed
                                , "failed" .= failed
                                , "failures" .= failures
                                ]
                      pure (MCP.ReadResourceResponse [ MCP.json req val ])
            Nothing -> do
              let val = JSON.object [ "error" .= ("bad uri" :: Text) ]
              pure (MCP.ReadResourceResponse [ MCP.json req val ])
      }

-- * Available Prompts

availablePrompts :: [MCP.Prompt]
availablePrompts = []


-- | Main MCP server handler
serve :: Live.State -> JSONRPC.EventEmitter -> JSONRPC.ConnectionId -> JSONRPC.Request -> IO (Either JSONRPC.Error JSONRPC.Response)
serve state emitter connId req = do
  MCP.serve availableTools availableResources availablePrompts state emitter connId req

-- Docs helpers


parsePkgName :: Text -> Maybe Pkg.Name
parsePkgName t =
  case Text.splitOn "/" t of
    [author, project] -> Just (Pkg.toName (Utf8.fromChars (Text.unpack author)) (Text.unpack project))
    _ -> Nothing

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
    let findInPkg (pkgName, Client.PackageInfo { Client.packageModules = mods }) =
          case Map.lookup wantName mods of
            Just (Client.PackageModule { Client.packageModuleDocs = m }) -> Just (pkgName, m)
            Nothing -> Nothing
    in case mapM findInPkg (Map.toList pkgsMap) of
         Just (x:_) -> Just x
         _ ->
           -- manual search
           let candidates = [ (pkg, m)
                            | (pkg, Client.PackageInfo { Client.packageModules = mods }) <- Map.toList pkgsMap
                            , Just (Client.PackageModule m) <- [Nothing] -- placeholder, never matches
                            ]
           in Nothing


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
  case spec of
    ModuleByName wantName -> do
      mLocal <- findLocalModuleDocs state wantName
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
              _ <- Watchtower.State.Compile.compile state pc []
              mInfo2 <- Client.getFileInfo fp state
              case mInfo2 of
                Just (Client.FileInfo { Client.docs = Just m@(Docs.Module name _ _ _ _ _) }) ->
                  pure (Right (ModuleResolved name (Just fp) Nothing m))
                _ -> pure (Left "no docs for file")

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

filterModuleForValue :: Name.Name -> Docs.Module -> Docs.Module
filterModuleForValue valName (Docs.Module nm comment _unions _aliases values _binops) =
  let one = case Map.lookup valName values of
              Just v -> Map.singleton valName v
              Nothing -> Map.empty
  in Docs.Module nm comment Map.empty Map.empty one Map.empty