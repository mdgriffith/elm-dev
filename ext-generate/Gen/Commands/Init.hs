{-# LANGUAGE OverloadedStrings #-}
module Gen.Commands.Init (run) where


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
import qualified System.Directory as Dir (getCurrentDirectory, createDirectoryIfMissing)
import qualified Reporting.Exit as Exit
import qualified Data.NonEmptyList as NE
import qualified Terminal.Colors
import qualified Data.Utf8 as Utf8


-- Flag types
data InitFlags = InitFlags
data MakeFlags = MakeFlags
data AddFlags = AddFlags
data CustomizeFlags = CustomizeFlags

-- INIT COMMAND
run :: () -> () -> IO ()
run () () = do
  let defaultConfig = Config.Config
        { Config.configPackageManager = Just Config.Bun
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
  -- elm.generate.json
  BS.writeFile "elm.generate.json" (Aeson.encodePretty defaultConfig)


  -- Create elm.json
  initResult <- initElmJson

  -- Create package.json
  let packageJson = "package.json"
  let packageJsonContent = "{}"
  BS.writeFile packageJson packageJsonContent

  -- Create Page/Home.elm
  Gen.Templates.write "Page" (Text.unpack Config.src) "Home"
 
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
                        Outline.AppOutline V.compiler (NE.List (Outline.RelativeSrcDir "src") []) directs indirects Map.empty Map.empty
                      putStrLn "Okay, I created it. Now read that link!"
                      return (Right ())






