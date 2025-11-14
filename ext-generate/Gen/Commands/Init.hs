{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Gen.Commands.Init (flags, args, run) where

import qualified CommandParser
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Text.IO as TIO
import qualified Data.Utf8 as Utf8
import qualified Deps.Solver as Solver
import qualified Elm.Constraint as Con
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Gen.Config as Config
import qualified Gen.Templates
import qualified Gen.Templates.Loader
import qualified Gen.Generate
import qualified Ext.Log
import qualified Make
import qualified Reporting.Exit as Exit
import qualified System.Directory as Dir (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, removeFile)
import System.Exit
import System.FilePath ((</>))
import qualified System.Process
import qualified Terminal.Colors
import Text.RawString.QQ (r)

flags :: CommandParser.ParsedArgs -> Either String ((), CommandParser.ParsedArgs)
flags = CommandParser.noFlag

args :: CommandParser.ArgParser ()
args =
  CommandParser.noArg

-- INIT COMMAND
run :: () -> () -> IO ()
run () () = do

  -- Create elm.dev.json
  let defaultConfig =
        Config.Config
          { Config.configPages =
              Just (Map.singleton "Home" (Config.PageConfig "/" [] False)),
            Config.configAssets =
              Just (Map.fromList [("./public", "assets")]),
            Config.configTheme = Nothing,
            Config.configGraphQL = Nothing,
            Config.configDocs = Nothing
          }
  BS.writeFile "elm.dev.json" (Aeson.encodePretty defaultConfig)

  -- Create README.md
  TIO.writeFile "README.md" defaultReadme

  -- Create elm.json
  initResult <- initElmJson

  Gen.Templates.writeGroupCustomizable Gen.Templates.Loader.Customizable "./src/app" "./elm-stuff/generated"
  Gen.Templates.writeGroup Gen.Templates.Loader.ToHidden "./elm-stuff/generated"
  Gen.Templates.writeGroup Gen.Templates.Loader.ToSrc "./src/app"
  Gen.Templates.writeGroup Gen.Templates.Loader.ToJs "./src"
  Gen.Templates.writeGroup Gen.Templates.Loader.ToRoot "."

  -- Create Page/Home.elm
  Gen.Templates.write "Page" "./src/app" "Home"
  
  cwd <- Dir.getCurrentDirectory
  Gen.Generate.run cwd

  putStrLn "I've generated a new Elm project for you using the Elm Dev codegen!\n\nCheckout the README.md to get started!"

defaultPackages :: Map.Map Pkg.Name Con.Constraint
defaultPackages =
  Map.fromList
    [ (Pkg.core, Con.anything),
      (Pkg.browser, Con.anything),
      (Pkg.html, Con.anything),
      (Pkg.url, Con.anything),
      (Pkg.json, Con.anything),
      (Pkg.http, Con.anything),
      (Pkg.toName Pkg.elm "bytes", Con.anything),
      (Pkg.toName Pkg.elm "file", Con.anything),
      (Pkg.toName Pkg.elm "virtual-dom", Con.anything),
      (Pkg.toName Pkg.elm "project-metadata-utils", Con.anything),
      (Pkg.toName Pkg.elm "random", Con.anything),
      (Pkg.toName Pkg.elm "time", Con.anything),
      (Pkg.toName (Utf8.fromChars "dillonkearns") "elm-markdown", Con.anything),
      (Pkg.toName (Utf8.fromChars "avh4") "elm-color", Con.anything),
      (Pkg.toName (Utf8.fromChars "mdgriffith") "elm-bezier", Con.anything),
      (Pkg.toName (Utf8.fromChars "lydell") "elm-app-url", Con.anything)
    ]

defaultReadme :: Text.Text
defaultReadme =
  Text.pack $
    [r|# Elm Dev App

This project was created with [elm-dev](https://github.com/mdgriffith/elm-dev).

## Install dependencies

```bash
npm install
yarn install
pnpm install
bun install
```

## Development

Run the development server:

```bash
npm run dev
yarn dev
pnpm dev
bun run dev
```

## Building

Create a production build:

```bash
npm run build
yarn build
pnpm build
bun run build
```

### Install in VS Code / Cursor

- Open the Extensions view and install the [**Elm Dev**](https://marketplace.visualstudio.com/items?itemName=ElmDev.elm-dev-vscode) extension.

### MCP Setup in VS Code/Cursor/Claude Code

To use the elm-dev mcp, you just need your agent to run `elm-dev mcp`.

In Cursor, this means creating `~/.cursor/mcp.json` with:
```json
{
  "mcpServers": {
    "elm-dev": {
      "command": "elm-dev",
      "args": ["mcp"]
    }
  }
}
```

### CLI help
```
Welcome to Elm Dev

  elm-dev init ...................... Create a new Elm project
  elm-dev make [module] ............... Build your Elm project
  elm-dev install [author/project] ......... Install a package

Add to your Elm app:

  elm-dev add page <url> ...................... Add a new page
  elm-dev add store <name> ................... Add a new store
  elm-dev add effect <name> ................. Add a new effect
  elm-dev add listener <name> ............. Add a new listener

Move an elm-dev-generated file into your project:

  elm-dev customize <module> .... Customize project components

Testing:

  elm-dev test .......... Discover, compile, and run Elm tests
  elm-dev test init ............................ Setup testing
  elm-dev test install <author/project> ..... Install test dep

```
|]

initElmJson :: IO (Either Exit.Init ())
initElmJson =
  do
    eitherEnv <- Solver.initEnv
    case eitherEnv of
      Left problem ->
        return (Left (Exit.InitRegistryProblem problem))
      Right (Solver.Env cache _ connection registry) ->
        do
          result <- Solver.verify cache connection registry defaultPackages
          case result of
            Solver.Err exit ->
              return (Left (Exit.InitSolverProblem exit))
            Solver.NoSolution ->
              return (Left (Exit.InitNoSolution (Map.keys defaultPackages)))
            Solver.NoOfflineSolution ->
              return (Left (Exit.InitNoOfflineSolution (Map.keys defaultPackages)))
            Solver.Ok details ->
              let solution = Map.map (\(Solver.Details vsn _) -> vsn) details
                  directs = Map.intersection solution defaultPackages
                  indirects = Map.difference solution defaultPackages
               in do
                    Dir.createDirectoryIfMissing True "src"
                    Outline.write "." $
                      Outline.App $
                        Outline.AppOutline
                          V.compiler
                          (NE.List (Outline.RelativeSrcDir "src/app") [Outline.RelativeSrcDir "elm-stuff/generated"])
                          directs
                          indirects
                          Map.empty
                          Map.empty
                    return (Right ())


-- | Helper function to run shell commands
runCommand :: Maybe FilePath -> String -> [String] -> IO ()
runCommand workingDir cmd args = do
  let process =
        (System.Process.proc cmd args)
          { System.Process.cwd = workingDir,
            System.Process.std_out = System.Process.NoStream
          }
  (_, _, _, ph) <- System.Process.createProcess process
  exitCode <- System.Process.waitForProcess ph
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> error $ "Command failed with exit code: " ++ show code

-- | Add scripts to package.json
addScripts :: [(String, String)] -> IO ()
addScripts scripts = do
  -- Read package.json
  packageJsonExists <- Dir.doesFileExist "package.json"
  if not packageJsonExists
    then error "package.json does not exist"
    else do
      content <- BS.readFile "package.json"
      case Aeson.eitherDecode content of
        Left err -> error $ "Failed to parse package.json: " ++ err
        Right (Aeson.Object obj) -> do
          -- Get existing scripts or create empty object
          let existingScripts = case KeyMap.lookup (Key.fromString "scripts") obj of
                Just (Aeson.Object scriptsObj) -> scriptsObj
                _ -> KeyMap.empty

          -- Add new scripts
          let newScripts =
                foldr
                  ( \(name, cmd) acc ->
                      KeyMap.insert (Key.fromString name) (Aeson.String $ Text.pack cmd) acc
                  )
                  existingScripts
                  scripts

          -- Create new package.json content
          let newContent = Aeson.Object $ KeyMap.insert (Key.fromString "scripts") (Aeson.Object newScripts) obj

          -- Write back to file
          BS.writeFile "package.json" (Aeson.encodePretty newContent)
        _ -> error "package.json is not a valid JSON object"
