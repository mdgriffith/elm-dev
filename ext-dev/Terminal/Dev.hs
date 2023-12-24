{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Terminal.Dev where

import Terminal
import Text.Read (readMaybe)
import System.FilePath ((</>), (<.>))

import qualified System.IO (hPutStrLn, stdout, Handle)
import qualified System.Exit
import qualified System.Process
import qualified System.FilePath as Path
import qualified System.Directory as Dir

import qualified Data.Char as Char
import qualified Control.Monad as Monad
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Watchtower.Server
import qualified Watchtower.Live

import qualified Ext.Common

import qualified Data.Utf8 as Utf8
import qualified Data.ByteString.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.NonEmptyList as NE
import qualified Data.Name as Name
import qualified Data.ByteString
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar

import qualified Ext.Dev
import qualified Ext.Dev.Package
import qualified Ext.Dev.EntryPoints
import qualified Ext.Dev.Find
import qualified Ext.Dev.Lookup
import qualified Ext.Dev.Json.Encode
import qualified Ext.Dev.Imports
import qualified Ext.Dev.Project
import qualified Ext.Dev.Usage
import qualified Ext.Dev.CallGraph
import qualified Ext.Dev.Explain

import qualified Ext.CompileProxy

import qualified Data.ByteString.Builder


import qualified Reporting
import qualified Reporting.Render.Type.Localizer
import qualified Reporting.Exit as Exit
import qualified Elm.Docs as Docs

import qualified Json.String
import qualified Json.Decode
import qualified Json.Encode

import qualified Elm.ModuleName
import qualified Elm.Details
import qualified Elm.Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version

import qualified Stuff
import qualified File

import qualified Terminal.Dev.Args
import qualified Terminal.Dev.Out
import qualified Terminal.Dev.Error
import qualified Terminal.Helpers

import qualified Make
import qualified Build
import qualified BackgroundWriter

main :: IO ()
main =
  Terminal.app intro outro
    [ start
    , docs
    , warnings
    -- , find
    , imports
    , usage
    , entrypoints
    , explain
    -- call graph needs some more thought
    -- , callgraph
    ]


loadAndEnsureCompiled :: FilePath -> Maybe (NE.List Elm.ModuleName.Raw) -> IO (Either Terminal.Dev.Error.Error Elm.Details.Details)
loadAndEnsureCompiled root exposed = do
    result <- Ext.CompileProxy.ensureModulesAreCompiled root exposed
    case result of
      Left err ->
        pure (Left (Terminal.Dev.Error.CompilationError err))
        
      Right details ->
        pure (Right details)


intro :: P.Doc
intro =
  P.vcat
    [ P.fillSep
        [ "Hi,","thank","you","for","trying","out"
        , P.green "Elm Dev."
        , " I hope you like it!"
        ]
    , ""

    ]


outro :: P.Doc
outro =
  P.fillSep $ map P.text $ words $
    "Happy hacking!"





data Warnings 
    = WarningsFile FilePath

  
data WarningFlags =
  WarningFlags
    { _warningsOutput :: Maybe String
    }

{-|



-}
warnings :: Terminal.Command
warnings =
  let
    summary =
      "Report missing type annotations and unused code."

    details =
      "The `warnings` command reports missing type annotations and unused code."

    example =
      reflow
        "After running that command, watchtower is listening at <http://localhost:51213>\
        \ and ready to be connected to."

    docsFlags =
      flags WarningFlags
        |-- flag "output" output_ "An optional file to write the JSON form of warnings to.  If not supplied, the warnings will be printed."

    warningArgs =
      oneOf 
        [ require1 WarningsFile dir
        ]
  in
  Terminal.Command "warnings" (Common summary) details example warningArgs docsFlags getWarnings




getWarnings :: Warnings -> Terminal.Dev.WarningFlags -> IO ()
getWarnings arg (Terminal.Dev.WarningFlags maybeOutput) =
  case arg of
    WarningsFile path ->
      if Path.takeExtension path == ".elm" then
        do
          maybeRoot <-  Dir.withCurrentDirectory (Path.takeDirectory path) Stuff.findRoot
          case maybeRoot of
            Nothing ->
              Terminal.Dev.Out.json maybeOutput (Left Terminal.Dev.Error.CouldNotFindRoot)
            
            Just root -> do
              eitherWarnings <- Ext.Dev.warnings root path
              case eitherWarnings of
                Left _ ->
                  Terminal.Dev.Out.json maybeOutput
                      (Right (Json.Encode.list (\a -> a) []))

                Right (mod, warningList) ->
                  Terminal.Dev.Out.json maybeOutput
                      (Right 
                        (Json.Encode.list
                          (Watchtower.Live.encodeWarning (Reporting.Render.Type.Localizer.fromModule mod))
                          warningList
                        )
                      )
                    

      else
        -- Produce docs as a project
        System.IO.hPutStrLn System.IO.stdout "Given file does not have an .elm extension"




entrypoints_ :: Parser (NE.List Elm.ModuleName.Raw)
entrypoints_ =
  Parser
    { _singular = "entrypoint"
    , _plural = "entrypoints"
    , _parser = parseEntrypoints
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["Main, Style.Button"]
    }

parseEntrypoints :: String -> Maybe (NE.List Elm.ModuleName.Raw)
parseEntrypoints chars =
  case Data.Maybe.catMaybes $ fmap parseElmModule (splitOn ',' chars) of 
      [] -> Nothing
      (top : remain) -> 
           Just (NE.List top remain)
  


output_ :: Parser String
output_ =
  Parser
    { _singular = "output"
    , _plural = "output"
    , _parser = parseJsonOutput
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["out.json"]
    }

parseJsonOutput :: String -> Maybe FilePath
parseJsonOutput chars =
  Just chars

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x





{- Get the docs for a package, a local project, or a specific file. -}



data DocFlags =
  DocFlags
    { _output :: Maybe String
    }

data DocsArgs
    = DocsFileList FilePath [FilePath]
    | DocsModules Elm.ModuleName.Raw [Elm.ModuleName.Raw]
    | DocsPackage Pkg.Name
    | DocsPackageVersion Pkg.Name Elm.Version.Version 
    deriving (Show)


{-|

You can provide:

  A package: mdgriffith/elm-ui
  An Elm file: Tests.elm
  Or an elm.json file


-}
docs :: Terminal.Command
docs =
  let
    summary =
      "Report the docs.json for the given package"

    details =
      "The `docs` command generates docs.json for :"

    example =
      reflow
        "After running that command, watchtower is listening at <http://localhost:51213>\
        \ and ready to be connected to."

    docsFlags =
      flags DocFlags
        |-- flag "output" output_ "An optional file to write the JSON form of docs to.  If not supplied, the docs will be printed."

    docsArgs =
      oneOf 
        [ require1 DocsPackage Terminal.Helpers.package
        , require2 DocsPackageVersion Terminal.Helpers.package Terminal.Helpers.version
        , oneOrMoreWith DocsFileList Terminal.Helpers.elmFile
        , oneOrMoreWith DocsModules elmModuleList
        ]
  in
  Terminal.Command "docs" (Common summary) details example docsArgs docsFlags getDocs




elmModuleList :: Parser Elm.ModuleName.Raw
elmModuleList =
  Parser
    { _singular = "elm file"
    , _plural = "elm files"
    , _parser = parseElmModule
    , _suggest = \_ -> return []
    , _examples = exampleModules
    }


trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile Char.isSpace . reverse . dropWhile Char.isSpace


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter = go
  where
    go [] = []
    go xs = 
        let (before, remainder) = break (== delimiter) xs
        in before : case remainder of
                      [] -> []
                      _:after -> go after


parseElmModule :: String -> Maybe Elm.ModuleName.Raw
parseElmModule charsRaw =
  let 
      chars = trimWhitespace charsRaw
  in
  if length chars == 0 then
    Nothing
  else
    let 
        pieces = splitOn '.' chars
    in
    if all isValidElmPiece pieces then
      Just (Name.fromChars chars)
    else
      Nothing

isValidElmPiece :: String -> Bool
isValidElmPiece [] = False  -- An empty string doesn't meet the criteria
isValidElmPiece (x:xs) = Char.isUpper x && all isValidChar xs
  where
    isValidChar c = Char.isAlphaNum c || c == '_'
   
  

exampleModules :: String -> IO [String]
exampleModules _ =
  return ["Main", "Style.Button"]


gatherDocs :: Data.Map.Map Name.Name Docs.Module -> FilePath ->  IO (Data.Map.Map Name.Name Docs.Module)
gatherDocs moduleDict path = do
  maybeRoot <-  Stuff.findRoot
  case maybeRoot of
    Nothing ->
      pure moduleDict
    
    Just root -> do
      maybeDocs <- Ext.Dev.docs root path
      case maybeDocs of
        Nothing ->
          pure moduleDict

        Just docs ->
          pure (Data.Map.insert (Docs._name docs) docs moduleDict)

getDocs :: DocsArgs -> Terminal.Dev.DocFlags -> IO ()
getDocs arg (Terminal.Dev.DocFlags maybeOutput) =
  case arg of
    DocsFileList top fileList -> do
      docMap <- Monad.foldM gatherDocs Data.Map.empty (top : fileList)
      Terminal.Dev.Out.json maybeOutput (Right (Docs.encode docMap))

    DocsModules top moduleList -> do
        maybeRoot <- Stuff.findRoot
        case maybeRoot of
          Nothing ->
            Terminal.Dev.Out.json maybeOutput (Left Terminal.Dev.Error.CouldNotFindRoot)
          
          Just root -> do
            maybeDocs <- Ext.Dev.docsForProject root (NE.List top moduleList)
            case maybeDocs of
              Left err ->
                Terminal.Dev.Out.json maybeOutput (Left (Terminal.Dev.Error.ExitReactor err))
              
              Right docs ->
                Terminal.Dev.Out.json maybeOutput (Right (Docs.encode docs))
      
    DocsPackage packageName -> do
      -- Is there an `elm.json` file?  Read it for the exact version.
      maybeCurrentVersion <- Ext.Dev.Package.getCurrentlyUsedOrLatestVersion "." packageName
      case maybeCurrentVersion of
        Nothing ->
           Terminal.Dev.Out.json maybeOutput (Left (Terminal.Dev.Error.CouldNotFindCurrentVersionForPackage packageName))

        Just packageVersion -> do   
          docsResult <- Ext.Dev.Package.getDocs packageName packageVersion
          case docsResult of
            Left err ->
              Terminal.Dev.Out.json maybeOutput (Left (Terminal.Dev.Error.DocsProblem err))

            Right docs ->
              Terminal.Dev.Out.json maybeOutput (Right (Docs.encode docs))
           
        
    DocsPackageVersion name packageVersion -> do
      result <- Ext.Dev.Package.getDocs name packageVersion
      case result of
        Left err ->
          Terminal.Dev.Out.json maybeOutput (Left (Terminal.Dev.Error.DocsProblem err))

        Right docs ->
          Terminal.Dev.Out.json maybeOutput (Right (Docs.encode docs))



 {-  Start Watchtower Server -}

data Flags =
  Flags
    { _port :: Maybe Int
    }



start :: Terminal.Command
start =
  let
    summary =
      "The Elm development experience dreams are made of."

    details =
      "The `start` command starts the Elm Dev server on your computer:"

    example =
      reflow
        "After running that command, watchtower is listening at <http://localhost:51213>\
        \ and ready to be connected to."

    serverFlags =
      flags Flags
        |-- flag "port" port_ "The port of the Elm Dev server (default: 51213)"
  in
  Terminal.Command "start" (Common summary) details example (optional dir) serverFlags startServer


startServer :: Maybe FilePath -> Terminal.Dev.Flags -> IO ()
startServer maybeRoot (Terminal.Dev.Flags maybePort) =
  Watchtower.Server.serve maybeRoot (Watchtower.Server.Flags maybePort)





port_ :: Parser Int
port_ =
  Parser
    { _singular = "port"
    , _plural = "ports"
    , _parser = readMaybe
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["51213"]
    }


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string




dir :: Parser FilePath
dir =
  Parser
    { _singular = "elm project directory"
    , _plural = "elm project directories"
    , _parser = parseDir
    , _suggest = \_ -> return []
    , _examples = exampleProjectDir
    }


parseDir :: String -> Maybe FilePath
parseDir chars =
  Just chars



exampleProjectDir :: String -> IO [String]
exampleProjectDir _ =
  return ["/path/to/my/project" ]




{- 

# Usages

Given a starting point, report all usages of that file.

Will report:
  - Each top-level definition and where it is used.
  - A summary of all modules that import this module
  - Is the module in a package and is it exposed
  - A new, recommended exposing list for the module.

For each top-level definition, report:
  - The module that uses it.
      - 



 -}




data Find 
    = FindValue String

  
data FindFlags =
  FindFlags
    { _findOutput :: Maybe String
    }

{-| -}
find :: Terminal.Command
find =
  let
    summary =
      "Given a qualified value, find it's definition and usages."

    details =
      "The `find` command will report all usages of a given elm module."

    example =
      reflow
        ""

    findFlags =
      flags FindFlags
        |-- flag "output" output_ "An optional file to write the JSON form of warnings to.  If not supplied, the warnings will be printed."

    findArgs =
      oneOf 
        [ require1 FindValue dir
        ]
  in
  Terminal.Command "find" (Common summary) details example findArgs findFlags findRun


findRun :: Find -> Terminal.Dev.FindFlags -> IO ()
findRun arg (Terminal.Dev.FindFlags maybeOutput) =
  case arg of
    FindValue path -> do
      valueResult <- Terminal.Dev.Args.value path
      case valueResult of
        Left err ->
          Terminal.Dev.Out.json maybeOutput (Left err)

        Right (Terminal.Dev.Args.Value root modName valueName) -> do
            maybeDefinition <- Ext.Dev.Lookup.lookupDefinition root modName valueName
            case maybeDefinition of
              Nothing ->
                Terminal.Dev.Out.json maybeOutput (Left Terminal.Dev.Error.CouldNotFindModule)

              Just definition ->
                System.IO.hPutStrLn System.IO.stdout "Found!"





{- 

## Imports

Given a starting point, report all modules that are imported by that file.

Will report:
  - All direct imports 
  - All indirect imports that aren't visible.
  - All external depdendencies that are imported

For every imported thing, also report:
  - The literal filename of that file
  - The Module name
  - The Module alias



Terminal.Helpers.elmFiles

 -}


elmFileOrModule :: Parser String
elmFileOrModule =
  Parser
    { _singular = "elm file"
    , _plural = "elm files"
    , _parser = parseElmFile
    , _suggest = \_ -> return []
    , _examples = exampleFilesOrModules
    }


parseElmFile :: String -> Maybe String
parseElmFile chars =
  Just (filter (/= ',') chars)
  


exampleFilesOrModules :: String -> IO [String]
exampleFilesOrModules _ =
  return ["Main.elm","src/Main.elm", "Ui.Button"]


data ImportsFlags =
  ImportsFlags
    { _importOutput :: Maybe String
    }



{-| -}
imports :: Terminal.Command
imports =
  let
    summary =
      "Given a file, report everything it imports."

    details =
      "The `imports` command will report all imports of a given elm module."

    example =
      reflow
        ""

    importsFlags =
      flags ImportsFlags
        |-- flag "output" output_ "An optional file to write the JSON form of warnings to.  If not supplied, the warnings will be printed."

  in
  Terminal.Command "imports" (Common summary) details example (oneOrMore elmFileOrModule) importsFlags importsRun



importsRun :: (String, [ String ]) -> Terminal.Dev.ImportsFlags -> IO ()
importsRun moduleNames (Terminal.Dev.ImportsFlags maybeOutput) = do
    moduleResult <- Terminal.Dev.Args.moduleList moduleNames
    case moduleResult of
      Left err -> do
        Terminal.Dev.Out.json maybeOutput (Left err)
          
      Right (Terminal.Dev.Args.ModuleList root infoList details) -> do

        let allModulesNames = fmap Terminal.Dev.Args._modName infoList
    
        let importSummary = Ext.Dev.Imports.getImportSummaryForMany details allModulesNames
        
        Terminal.Dev.Out.json maybeOutput
          (Right 
            (Ext.Dev.Imports.encodeSummary
                importSummary
            )
          )



{- Call graph -}

{- 

 -}

data CallGraph 
    = CallGraphModule String

  
data CallGraphFlags =
  CallGraphFlags
    { _callgraphOutput :: Maybe String
    }

{-| -}
callgraph :: Terminal.Command
callgraph =
  let
    summary =
      "Given a file, report everything it callgraph."

    details =
      "The `callgraph` command will report all callgraph of a given elm module."

    example =
      reflow
        ""

    callgraphFlags =
      flags CallGraphFlags
        |-- flag "output" output_ "An optional file to write the JSON form of warnings to.  If not supplied, the warnings will be printed."

    callgraphArgs =
      oneOf 
        [ require1 CallGraphModule dir
        ]
  in
  Terminal.Command "callgraph" (Common summary) details example callgraphArgs callgraphFlags callgraphRun



callgraphRun :: CallGraph -> Terminal.Dev.CallGraphFlags -> IO ()
callgraphRun arg (Terminal.Dev.CallGraphFlags maybeOutput) =
  case arg of
    CallGraphModule path -> do
      moduleResult <- Terminal.Dev.Args.modul path
      case moduleResult of
        Left err ->
           Terminal.Dev.Out.json maybeOutput (Left err)
           
        Right (Terminal.Dev.Args.Module root (Terminal.Dev.Args.ModuleInfo moduleName modulePath) details) -> do
          callgraphSummaryResult <- Ext.Dev.CallGraph.callgraph root modulePath
          case callgraphSummaryResult of
            Nothing ->
              Terminal.Dev.Out.json maybeOutput (Left Terminal.Dev.Error.CouldNotFindModule)

            Just callgraphSummary ->
              Terminal.Dev.Out.json maybeOutput
                (Right 
                  (Ext.Dev.CallGraph.encode
                      callgraphSummary
                  )
                )



{- Usage -}


{- 

## Imports

Given a starting point, report all modules that are imported by that file.

Will report:
  - All direct imports 
  - All indirect imports that aren't visible.
  - All external depdendencies that are imported

For every imported thing, also report:
  - The literal filename of that file
  - The Module name
  - The Module alias



 -}

data Usage 
    = UsageModule String
    | UsageType () String

  
data UsageFlags =
  UsageFlags
    { _usageOutput :: Maybe String
    , _usageEntryPoints :: Maybe (NE.List Elm.ModuleName.Raw)
    }


typeUsage :: Parser ()
typeUsage =
  Parser
    { _singular = "type"
    , _plural = "types"
    , _parser = parseExact "type"
    , _suggest = \_ -> return [ "type" ]
    , _examples = \_ -> return ["type"]
    }


parseExact :: String -> String -> Maybe ()
parseExact target found =
  if target == found then 
    Just ()
  else 
    Nothing


{-| -}
usage :: Terminal.Command
usage =
  let
    summary =
      "Given a file, report all modules that use it in your project."

    details =
      "The `usage` command will report all usage of a given elm module."

    example =
      reflow
        ""

    usageFlags =
      flags UsageFlags
        |-- flag "output" output_ "An optional file to write the JSON form of warnings to.  If not supplied, the warnings will be printed."
        |-- flag "entypoints" entrypoints_ "Specify which modules are entrypoints to the project.  This will limit the scope of the search."

    usageArgs =
      oneOf 
        [ require1 UsageModule dir
        , require2 UsageType typeUsage dir
        ]
  in
  Terminal.Command "usage" (Common summary) details example usageArgs usageFlags usageRun


usageRun :: Usage -> Terminal.Dev.UsageFlags -> IO ()
usageRun arg (Terminal.Dev.UsageFlags maybeOutput maybeEntrypoints) =
  case arg of
    UsageType _ path -> do
      valueResult <- Terminal.Dev.Args.value path
      case valueResult of
        Left err ->
          Terminal.Dev.Out.json maybeOutput
                    (Left err)

        Right (Terminal.Dev.Args.Value root modName valueName) -> do
          compilationCheckResult <- loadAndEnsureCompiled root maybeEntrypoints
          case compilationCheckResult of
            Left err ->
              Terminal.Dev.Out.json maybeOutput
                    (Left err)

            Right details -> do
              usageSummary <- Ext.Dev.Usage.usageOfType root details modName valueName
              case usageSummary of
                Left err ->
                    Terminal.Dev.Out.json maybeOutput
                        (Left err)

                Right summary ->
                    Terminal.Dev.Out.json maybeOutput
                        (Right 
                          (Ext.Dev.Usage.encodeUsageOfType
                              summary
                          )
                        )

    UsageModule path -> do
      moduleResult <- Terminal.Dev.Args.modul path
      case moduleResult of
        Left err ->
           Terminal.Dev.Out.json maybeOutput (Left err)
           
        Right (Terminal.Dev.Args.Module root (Terminal.Dev.Args.ModuleInfo moduleName modulePath) details) -> do
          usageSummary <- Ext.Dev.Usage.usageOfModule root details moduleName
          case usageSummary of
            Nothing ->
                Terminal.Dev.Out.json maybeOutput
                    (Left Terminal.Dev.Error.CouldNotFindModule)

            Just summary ->
                Terminal.Dev.Out.json maybeOutput
                    (Right 
                      (Ext.Dev.Usage.encode
                          summary
                      )
                    )


{- Entrypoints 


If the project is an app:
  
  List all discovered entrypoints for a given project.
  
  For each entrypoint:
    - List the literal filepath to the entrypoint.
    - List the symbolic module name.
    - The base directory of the `elm.json` file.


    - All port definitions
    - The full type for the flags



-}



data EntryPoints 
    = EntryPoints

  
data EntryPointsFlags =
  EntryPointsFlags
    { _entrypointsOutput :: Maybe String
    }

{-| -}
entrypoints :: Terminal.Command
entrypoints =
  let
    summary =
      "Given a directory, scan for all entrypoints, recursively."

    details =
      "The `entrypoints` command will report all usages of a given elm module."

    example =
      reflow
        ""

    entrypointsFlags =
      flags EntryPointsFlags
        |-- flag "output" output_ "An optional file to write the JSON form of warnings to.  If not supplied, the warnings will be printed."

    entrypointsArgs =
      oneOf 
        [ require0 EntryPoints
        ]
  in
  Terminal.Command "entrypoints" (Common summary) details example entrypointsArgs entrypointsFlags entrypointsRun


entrypointsRun :: EntryPoints -> Terminal.Dev.EntryPointsFlags -> IO ()
entrypointsRun arg (Terminal.Dev.EntryPointsFlags maybeOutput) =
  case arg of
    EntryPoints -> do
      maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing ->
           Terminal.Dev.Out.json maybeOutput
              (Left (Terminal.Dev.Error.CouldNotFindRoot))
        
        Just root -> do
          entryResult <- Ext.Dev.entrypoints root
          case entryResult of
            Left err ->
               Terminal.Dev.Out.json maybeOutput
                  (Left (Terminal.Dev.Error.CompilationError err))
            
            Right entry ->
                Terminal.Dev.Out.json maybeOutput
                  (Right (Json.Encode.list Ext.Dev.EntryPoints.encode entry))




{- Explain -}




data Explain 
    = ExplainValue String

  
data ExplainFlags =
  ExplainFlags
    { _explainOutput :: Maybe String
    }

{-| -}
explain :: Terminal.Command
explain =
  let
    summary =
      "Given a qualified value, explain it's definition and usages."

    details =
      "The `explain` command will report all usages of a given elm module."

    example =
      reflow
        ""

    explainFlags =
      flags ExplainFlags
        |-- flag "output" output_ "An optional file to write the JSON form of warnings to.  If not supplied, the warnings will be printed."

    explainArgs =
      oneOf 
        [ require1 ExplainValue dir
        ]
  in
  Terminal.Command "explain" (Common summary) details example explainArgs explainFlags explainRun


explainRun :: Explain -> Terminal.Dev.ExplainFlags -> IO ()
explainRun arg (Terminal.Dev.ExplainFlags maybeOutput) =
  case arg of
    ExplainValue path -> do
      valueResult <- Terminal.Dev.Args.value path
      case valueResult of
        Left err ->
          Terminal.Dev.Out.json maybeOutput (Left err)

        Right (Terminal.Dev.Args.Value root modName valueName) -> do
          compilationCheckResult <- loadAndEnsureCompiled root (Just (NE.List modName []))
          case compilationCheckResult of
            Left err ->
              Terminal.Dev.Out.json maybeOutput
                    (Left err)

            Right details -> do
              maybeFound <- Ext.Dev.Explain.explain details root modName valueName
              case maybeFound of
                Nothing ->
                  Terminal.Dev.Out.json maybeOutput (Left (Terminal.Dev.Error.CouldNotFindModule))

                Just definition ->
                  Terminal.Dev.Out.json maybeOutput (Right (Ext.Dev.Explain.encode definition))