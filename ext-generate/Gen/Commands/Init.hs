{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Gen.Commands.Init (flags, args, run) where


import qualified CommandParser
import qualified Gen.Config as Config
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Elm.Version as V
import qualified Make
import System.FilePath ((</>))
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Elm.Constraint as Con
import qualified Elm.Package as Pkg
import qualified Data.Map as Map
import qualified Deps.Solver as Solver
import qualified Elm.Outline as Outline
import qualified Gen.Templates
import qualified Data.Text.Encoding
import qualified System.Directory as Dir (getCurrentDirectory, createDirectoryIfMissing, doesFileExist, removeFile)
import qualified Reporting.Exit as Exit
import qualified Data.NonEmptyList as NE
import qualified Terminal.Colors
import qualified Data.Utf8 as Utf8
import qualified System.Process
import Data.Maybe (fromMaybe)
import System.Exit
import qualified Control.Monad as Monad
import Text.RawString.QQ (r)
import qualified Gen.Templates.Loader


flags = CommandParser.parseFlag
    (CommandParser.flagWithArg "package-manager" "The package manager to use"
        (\str -> case str of
            "npm" -> Just Config.NPM
            "yarn" -> Just Config.Yarn
            "pnpm" -> Just Config.PNPM
            "bun" -> Just Config.Bun
            _ -> Nothing
        )
    )

args :: CommandParser.ArgParser ()
args =
    CommandParser.noArg



-- INIT COMMAND
run :: () -> Maybe Config.PackageManager -> IO ()
run () maybePkgManager = do
  let pkgManager = fromMaybe Config.Bun maybePkgManager
  
  -- Create elm.generate.json
  let defaultConfig = Config.Config
        { Config.configPackageManager = Just pkgManager
        , Config.configApp = Just $ Config.AppConfig {
            Config.appPages = Map.singleton "Home" (Config.PageConfig "/" [] False)
          }
        , Config.configAssets = Just $ Map.singleton "Assets" $ Config.AssetConfig {
            Config.assetSrc = "./public",
            Config.assetOnServer = "assets"
          }
        , Config.configTheme = Nothing
        , Config.configGraphQL = Nothing
        , Config.configDocs = Nothing
        }
  BS.writeFile "elm.generate.json" (Aeson.encodePretty defaultConfig)

  -- Create README.md
  TIO.writeFile "README.md" (defaultReadme pkgManager)

  -- Create elm.json
  initResult <- initElmJson
  
  Gen.Templates.writeGroupCustomizable Gen.Templates.Loader.Customizable "./src/app" "./elm-stuff/generated"
  Gen.Templates.writeGroup Gen.Templates.Loader.ToHidden "./elm-stuff/generated"
  Gen.Templates.writeGroup Gen.Templates.Loader.ToSrc "./src/app"
  Gen.Templates.writeGroup Gen.Templates.Loader.ToJs "./src"
  Gen.Templates.writeGroup Gen.Templates.Loader.ToRoot "."

  -- Create Page/Home.elm
  Gen.Templates.write "Page" "./src/app" "Home"



  -- Create package.json and install dependencies
  installDependencies pkgManager (DependencyOptions { dev = True, cwd = Nothing }) ["vite", "vite-plugin-elm", "typescript"]


 
  putStrLn "Created elm.generate.json with default configuration"



defaultPackages :: Map.Map Pkg.Name Con.Constraint
defaultPackages =
  Map.fromList
    [ (Pkg.core, Con.anything)
    , (Pkg.browser, Con.anything)
    , (Pkg.html, Con.anything)
    , (Pkg.url, Con.anything)
    , (Pkg.json, Con.anything)
    , (Pkg.http, Con.anything)
    , (Pkg.toName Pkg.elm "bytes", Con.anything)
    , (Pkg.toName Pkg.elm "file", Con.anything)
    , (Pkg.toName Pkg.elm "virtual-dom", Con.anything)
    , (Pkg.toName Pkg.elm "project-metadata-utils", Con.anything)
    , (Pkg.toName Pkg.elm "random", Con.anything)
    , (Pkg.toName Pkg.elm "time", Con.anything)
    , (Pkg.toName (Utf8.fromChars "dillonkearns") "elm-markdown", Con.anything)
    , (Pkg.toName (Utf8.fromChars "avh4") "elm-color", Con.anything)
    , (Pkg.toName (Utf8.fromChars "mdgriffith") "elm-bezier", Con.anything)
    , (Pkg.toName (Utf8.fromChars "lydell") "elm-app-url", Con.anything)
    ]

defaultReadme :: Config.PackageManager -> Text.Text
defaultReadme pkgManager = Text.pack $ 
    [r|# Elm Dev App

This project was created with [elm-dev](https://github.com/mdgriffith/elm-dev).

## Development

Start the development server:

```bash
|] ++ case pkgManager of
        Config.NPM -> "npm run dev"
        Config.Yarn -> "yarn dev" 
        Config.PNPM -> "pnpm dev"
        Config.Bun -> "bun run dev"
    ++ [r|
```

## Building

Create a production build:

```bash
|] ++ case pkgManager of
        Config.NPM -> "npm run build"
        Config.Yarn -> "yarn build"
        Config.PNPM -> "pnpm build"
        Config.Bun -> "bun run build"
    ++ [r|
```
|]



initElmJson :: IO (Either Exit.Init ())
initElmJson =
  do  eitherEnv <- Solver.initEnv
      case eitherEnv of
        Left problem ->
          return (Left (Exit.InitRegistryProblem problem))

        Right (Solver.Env cache _ connection registry) ->
          do  result <- Solver.verify cache connection registry defaultPackages
              case result of
                Solver.Err exit ->
                  return (Left (Exit.InitSolverProblem exit))

                Solver.NoSolution ->
                  return (Left (Exit.InitNoSolution (Map.keys defaultPackages)))

                Solver.NoOfflineSolution ->
                  return (Left (Exit.InitNoOfflineSolution (Map.keys defaultPackages)))

                Solver.Ok details ->
                  let
                    solution = Map.map (\(Solver.Details vsn _) -> vsn) details
                    directs = Map.intersection solution defaultPackages
                    indirects = Map.difference solution defaultPackages
                  in
                  do  Dir.createDirectoryIfMissing True "src"
                      Outline.write "." $ Outline.App $
                        Outline.AppOutline V.compiler 
                          (NE.List (Outline.RelativeSrcDir "src/app") [Outline.RelativeSrcDir "elm-stuff/generated"])
                          directs
                          indirects
                          Map.empty
                          Map.empty
                      return (Right ())




-- NPM stuff


data DependencyOptions = DependencyOptions
    { dev :: Bool
    , cwd :: Maybe FilePath
    }

-- | Install dependencies using the specified package manager
installDependencies :: Config.PackageManager -> DependencyOptions -> [String] -> IO ()
installDependencies manager options packages = do
    let cwd' = cwd options
    let saveDevFlag = if dev options then "--save-dev" else "--save"
    let devFlag = if dev options then ["--dev"] else []
    
    -- Check if package.json exists
    packageJsonExists <- Dir.doesFileExist (maybe "." id cwd' </> "package.json")
    
    -- If no package.json exists, create one first
    Monad.unless packageJsonExists $ do
        case manager of
            Config.NPM -> runCommand cwd' "npm" ["init", "-y"]
            Config.Yarn -> runCommand cwd' "yarn" ["init", "-y"]
            Config.PNPM -> runCommand cwd' "pnpm" ["init"]
            Config.Bun -> runCommand cwd' "bun" ["init", "-y"]


    -- Delete index.ts if it exists (created by bun init)
    case manager of
        Config.Bun -> do
            let entryPath = maybe "." id cwd' </> "index.ts"
            entryExists <- Dir.doesFileExist entryPath
            Monad.when entryExists $ Dir.removeFile entryPath
        _ -> return ()
    
    
    -- Now install the dependencies
    case manager of
        Config.NPM -> do
            runCommand cwd' "npm" (["install", saveDevFlag] ++ packages)
        Config.Yarn -> do
            runCommand cwd' "yarn" (["add"] ++ devFlag ++ packages)
        Config.PNPM -> do
            runCommand cwd' "pnpm" (["add"] ++ devFlag ++ packages)
        Config.Bun -> do
            runCommand cwd' "bun" (["add"] ++ devFlag ++ packages)

-- | Helper function to run shell commands
runCommand :: Maybe FilePath -> String -> [String] -> IO ()
runCommand workingDir cmd args = do
    let process = (System.Process.proc cmd args) 
            { System.Process.cwd = workingDir
            , System.Process.std_out = System.Process.NoStream
            }
    (_, _, _, ph) <- System.Process.createProcess process
    exitCode <- System.Process.waitForProcess ph
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> error $ "Command failed with exit code: " ++ show code



