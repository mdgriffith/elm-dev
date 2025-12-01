
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
import qualified Watchtower.State.Versions as Versions
import qualified Watchtower.Server.LSP as LSP
import qualified Watchtower.Server.LSP.Helpers as Helpers
import qualified Watchtower.Server.DevWS
import qualified Ext.Encode
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
import qualified Ext.Install
import qualified Watchtower.Server.LSP.Protocol as LSPProtocol
import qualified Data.List as List
import qualified Data.Maybe as Maybe


availableTools :: [MCP.Tool]
availableTools =
  [ toolScaffoldElmApp
  , toolScaffoldElmPackage
  , toolCompile
  , toolUnused
  , toolInstall
  , toolAddPage
  , toolAddStore
  , toolAddEffect
  , toolAddListener
  , toolAddTheme
  , toolTestInstall
  , toolTestRun
  , toolProjectSelect
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
          r <- Exception.try (GenInit.run dir) :: IO (Either SomeException ())
          case r of
            Left _ -> pure (errTxt "Failed to initialize project")
            Right _ -> do
              -- Ensure elm-explorations/test is available in test-dependencies
              _ <- withDir dir $ do
                let testPkg = Pkg.toName (Utf8.fromChars "elm-explorations") "test"
                _ <- TestInstall.installTestDependency testPkg
                pure ()
              canonicalRoot <- Dir.canonicalizePath dir
              Watchtower.State.Discover.discover state canonicalRoot
              -- Select the newly created project for this connection, if discover found it
              let (Client.State _ mProjects _ _ _ _ _ _) = state
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

-- unused warnings (as a tool)
toolUnused :: MCP.Tool
toolUnused = MCP.Tool
  { MCP.toolName = "unused"
  , MCP.toolDescription = "List unused warnings for the project or for a specific file."
  , MCP.toolInputSchema =
      JSON.object
        [ "type" .= ("object" :: Text)
        , "properties" .= JSON.object
            [ Key.fromText "file" .= JSON.object
                [ "type" .= ("string" :: Text)
                , "description" .= ("Absolute path to a file to filter warnings (optional)" :: Text)
                ]
            ]
        , "required" .= ([] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      selection <- ProjectLookup.resolveProjectFromSession Nothing connId state
      case selection of
        Left msg -> pure (errTxt msg)
        Right pc@(Client.ProjectCache proj _ _ _ _) -> do
          _ <- Watchtower.State.Compile.compile state pc []
          case getStringArg args "file" of
            Just filePath -> do
              (_loc, warns) <- Helpers.getWarningsForFile state filePath
              let (unusedImports, unusedValues) = partitionUnused warns
              let body = renderUnusedMarkdown [(filePath, unusedImports, unusedValues)]
              pure (MCP.ToolCallResponse [MCP.ToolResponseText Nothing body])
            Nothing -> do
              allInfos <- Client.getAllFileInfos state
              let projectFiles = fmap fst $ filter (\(p, _) -> Ext.Dev.Project.contains p proj) (Map.toList allInfos)
              perFile <- mapM
                          (\p -> do
                              (_loc, warns) <- Helpers.getWarningsForFile state p
                              let (uimps, uvals) = partitionUnused warns
                              pure (p, uimps, uvals)
                          )
                          projectFiles
              let nonEmpty = filter (\(_, imps, vals) -> not (null imps) || not (null vals)) perFile
              let body = if null nonEmpty
                           then "No unused warnings."
                           else renderUnusedMarkdown nonEmpty
              pure (MCP.ToolCallResponse [MCP.ToolResponseText Nothing body])
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
              let (Client.State _ mProjects _ _ _ _ _ _) = state
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
          _ <- Watchtower.State.Compile.updateVfsFromFs proj
          -- Perform compile
          compileResult <- Watchtower.State.Compile.compile state projCache []
          -- Also compile tests regardless of main compile result
          _ <- Watchtower.State.Compile.compileTests state projCache
          -- Mark compiled version = current fs version snapshot, regardless of success or error
          let projectRoot = Ext.Dev.Project.getRoot proj
          cur <- Versions.readVersions projectRoot
          Versions.setCompileVersionTo projectRoot (Versions.fsVersion cur)
          case compileResult of
            Left clientErr -> do
              -- Render errors as terminal-style text, like `elm make`
              let doc =
                    case clientErr of
                      Client.ReactorError reactor ->
                        Help.reportToDoc (Exit.reactorToReport reactor)
                      Client.GenerationError msg ->
                        Help.reportToDoc (Help.report "GENERATION ERROR" Nothing msg [])
              let txt = Text.pack (Help.toString doc)
              pure (errTxt txt)
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
          selection <- ProjectLookup.resolveProjectFromSession Nothing connId state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              withDir dir $ do
                case parsePkgName (Text.pack pkg) of
                  Nothing ->
                    pure (errTxt "Invalid package name. Expected author/project")
                  Just pkgName -> do
                    result <- Ext.Install.installDependency pkgName
                    case result of
                      Left _ -> pure (errTxt "Failed to install package")
                      Right Ext.Install.AlreadyInstalled -> do
                        let docsUriElm = Text.concat ["elm://docs/package/", Text.pack (Pkg.toUrl pkgName)]
                        let linkInfoElm =
                              MCP.ResourceLinkInfo
                                { MCP.resourceLinkUri = docsUriElm
                                , MCP.resourceLinkName = Text.concat [Text.pack (Pkg.toChars pkgName), " docs"]
                                , MCP.resourceLinkDescription = Just "Open package docs rendered by server"
                                , MCP.resourceLinkMimeType = Nothing
                                }
                        pure (MCP.ToolCallResponse [MCP.ToolResponseText Nothing "Already installed", MCP.ToolResponseResourceLink Nothing linkInfoElm])
                      Right Ext.Install.SuccessfullyInstalled -> do
                        let docsUriElm = Text.concat ["elm://docs/package/", Text.pack (Pkg.toUrl pkgName)]
                        let linkInfoElm =
                              MCP.ResourceLinkInfo
                                { MCP.resourceLinkUri = docsUriElm
                                , MCP.resourceLinkName = Text.concat [Text.pack (Pkg.toChars pkgName), " docs"]
                                , MCP.resourceLinkDescription = Just "Open package docs rendered by server"
                                , MCP.resourceLinkMimeType = Nothing
                                }
                        pure (MCP.ToolCallResponse [MCP.ToolResponseText Nothing "Installed successfully", MCP.ToolResponseResourceLink Nothing linkInfoElm])
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
  , MCP.call = \args state _emit connId -> do
      case requireStringArg "package" args of
        Left e -> pure (errTxt (Text.pack e))
        Right pkgStr -> do
          selection <- ProjectLookup.resolveProjectFromSession Nothing connId state
          case selection of
            Left msg -> pure (errTxt msg)
            Right (Client.ProjectCache proj _ _ _ _) -> do
              let dir = Ext.Dev.Project.getRoot proj
              withDir dir $ do
                case parsePkgName (Text.pack pkgStr) of
                  Nothing ->
                    pure (errTxt "Invalid package name. Expected author/project")
                  Just pkgName -> do
                    result <- TestInstall.installTestDependency pkgName
                    case result of
                      Left _ ->
                        pure (errTxt "Failed to install test dependency")
                      Right TestInstall.AlreadyInstalled -> do
                        let docsUriElm = Text.concat ["elm://docs/package/", Text.pack (Pkg.toUrl pkgName)]
                        let linkInfoElm =
                              MCP.ResourceLinkInfo
                                { MCP.resourceLinkUri = docsUriElm
                                , MCP.resourceLinkName = Text.concat [Text.pack (Pkg.toChars pkgName), " docs"]
                                , MCP.resourceLinkDescription = Just "Open package docs rendered by server"
                                , MCP.resourceLinkMimeType = Nothing
                                }
                        pure (MCP.ToolCallResponse [ MCP.ToolResponseText Nothing "Already installed"
                                                   , MCP.ToolResponseResourceLink Nothing linkInfoElm
                                                   ])
                      Right TestInstall.SuccessfullyInstalled -> do
                        let docsUriElm = Text.concat ["elm://docs/package/", Text.pack (Pkg.toUrl pkgName)]
                        let linkInfoElm =
                              MCP.ResourceLinkInfo
                                { MCP.resourceLinkUri = docsUriElm
                                , MCP.resourceLinkName = Text.concat [Text.pack (Pkg.toChars pkgName), " docs"]
                                , MCP.resourceLinkDescription = Just "Open package docs rendered by server"
                                , MCP.resourceLinkMimeType = Nothing
                                }
                        pure (MCP.ToolCallResponse [ MCP.ToolResponseText Nothing "Installed successfully"
                                                   , MCP.ToolResponseResourceLink Nothing linkInfoElm
                                                   ])
  }

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
                , "description" .= ("Optional max seconds to wait for tests (default 10)" :: Text)
                ]
            ]
        , "required" .= ([] :: [Text])
        ]
  , MCP.toolOutputSchema = Nothing
  , MCP.call = \args state _emit connId -> do
      selection <- ProjectLookup.resolveProjectFromSession Nothing connId state
      case selection of
        Left msg -> pure (errTxt msg)
        Right (Client.ProjectCache proj _ _ _ _) -> do
          let dir = Ext.Dev.Project.getRoot proj
          -- Ensure VFS is updated from disk so tests see latest changes
          _ <- Watchtower.State.Compile.updateVfsFromFs proj
          let mTimeoutSeconds =
                case KeyMap.lookup "timeoutSeconds" args of
                  Just (JSON.Number n) -> (Scientific.toBoundedInteger n :: Maybe Int)
                  _ -> Nothing
          let timeoutSeconds = maybe 10 id mTimeoutSeconds
          startPs <- CPUTime.getCPUTime
          result <- TestRunner.run (Just timeoutSeconds) dir
          case result of
            Left e -> case e of
              TestRunner.TimedOut waited ->
                pure (errTxt (Text.pack ("Timed out running tests after " ++ show waited ++ "s")))
              TestRunner.RunFailed msg ->
                pure (errTxt (Text.pack msg))
            Right reports -> do
              endPs <- CPUTime.getCPUTime
              let durationMs :: Int
                  durationMs = fromInteger ((endPs - startPs) `div` 1000000000)
              let rendered = TestReport.renderReportsWithDuration False (Just durationMs) reports
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
          let (Client.State _ mProjects _ _ _ _ _ _) = state
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
      , MCP.resourceMimeType = Just "text/markdown"
      , MCP.resourceAnnotations = Just (MCP.Annotations [MCP.AudienceAssistant] MCP.High Nothing)
      , MCP.read = \req state _emit connId -> do
          projectFound <- ProjectLookup.resolveProjectFromSession Nothing connId state
          case projectFound of
            Left msg -> do
              -- Build project list for frontmatter even when no project is selected
              let (Client.State _ mProjects _ _ _ _ _ _) = state
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
              -- For MCP: ensure VFS is fresh, then compile only if versions indicate staleness
              _ <- Watchtower.State.Compile.updateVfsFromFs proj
              let projectRoot = Ext.Dev.Project.getRoot proj
              vers <- Versions.readVersions projectRoot
              when (Versions.compileVersion vers < Versions.fsVersion vers) $ do
                _ <- Watchtower.State.Compile.compile state pc []
                cur <- Versions.readVersions projectRoot
                Versions.setCompileVersionTo projectRoot (Versions.fsVersion cur)
              currentResult <- Control.Concurrent.STM.readTVarIO mCompileResult

              -- Determine compilation status text (for YAML)
              let (compStatus :: Text, compMsgLine :: [Text]) = case currentResult of
                    Client.Success _ -> ("ok", [])
                    Client.NotCompiled -> ("compiling", [])
                    Client.Error (Client.GenerationError errMsg) -> ("generationError", [ Text.concat ["  message: ", Text.pack errMsg] ])
                    Client.Error (Client.ReactorError reactor) -> case reactor of
                      Exit.ReactorBadBuild _ -> ("errors", [])
                      _ -> ("error", [])

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
              let (Client.State _ mProjects _ _ _ _ _ _) = state
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
                Right pc@(Client.ProjectCache proj _ _ _ _) -> do
                  _ <- Watchtower.State.Compile.compile state pc []
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
                Right (Client.ProjectCache proj _ _ _ mTest) -> do
                  m <- Control.Concurrent.STM.readTVarIO mTest
                  case m >>= Client.testResults of
                    Nothing -> do
                      let val = JSON.object
                                [ "error" .= ("no test results" :: Text)
                                , "hint" .= ("Run the test_run tool" :: Text)
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
          let (Client.State _ mProjects _ _ _ _ _ _) = state
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
