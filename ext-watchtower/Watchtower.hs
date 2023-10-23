{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Watchtower where

import Terminal
import Text.Read (readMaybe)
import System.FilePath ((</>), (<.>))

import qualified Control.Monad as Monad
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Watchtower.Server
import qualified Watchtower.Live
import qualified System.IO (hPutStrLn, stdout, Handle)
import qualified System.FilePath as Path
import qualified System.Directory as Dir
import qualified System.Exit
import qualified System.Process
import qualified Ext.Common
import qualified Data.ByteString
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar
import qualified System.Directory as Dir

import qualified Data.ByteString.Builder

import qualified Reporting.Render.Type.Localizer
import qualified Reporting.Exit as Exit
import qualified Elm.Docs as Docs

import qualified Json.String
import qualified Json.Decode
import qualified Json.Encode
import qualified Elm.Outline
import qualified Ext.Dev
import qualified Ext.Dev.Package

import qualified Terminal.Helpers (package, version)
import qualified Elm.Package as Pkg
import qualified Elm.Version
import qualified Data.ByteString.Builder
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Data.Map
import qualified Data.Maybe
import qualified Stuff
import qualified File




main :: IO ()
main =
  Terminal.app intro outro
    [ start
    , docs
    , warnings
    , generate
    ]



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



{- Get the docs for a package, a local project, or a specific file. -}



data DocFlags =
  DocFlags
    { _output :: Maybe String
    }

data DocsArgs
    = DocsCwdProject
    | DocsPath FilePath
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
        [ require0 DocsCwdProject
        , require1 DocsPackage Terminal.Helpers.package
        , require2 DocsPackageVersion Terminal.Helpers.package Terminal.Helpers.version
        , require1 DocsPath dir
        ]
  in
  Terminal.Command "docs" (Common summary) details example docsArgs docsFlags getDocs



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




getWarnings :: Warnings -> Watchtower.WarningFlags -> IO ()
getWarnings arg (Watchtower.WarningFlags maybeOutput) =
  case arg of
    WarningsFile path ->
      if Path.takeExtension path == ".elm" then
        do
          maybeRoot <-  Dir.withCurrentDirectory (Path.takeDirectory path) Stuff.findRoot
          case maybeRoot of
            Nothing ->
              System.IO.hPutStrLn System.IO.stdout "Was not able to find an elm.json!"
            
            Just root -> do
              eitherWarnings <- Ext.Dev.warnings root path
              case eitherWarnings of
                Left _ ->
                  System.IO.hPutStrLn System.IO.stdout "No warnings!"

                Right (mod, warningList) ->
                  System.IO.hPutStrLn System.IO.stdout
                      (show (Json.Encode.encode
                          (Json.Encode.list
                              (Watchtower.Live.encodeWarning (Reporting.Render.Type.Localizer.fromModule mod))
                              warningList
                          )
                      ))

      else
        -- Produce docs as a project
        System.IO.hPutStrLn System.IO.stdout "Given file does not have an .elm extension"





output_ :: Parser String
output_ =
  Parser
    { _singular = "output"
    , _plural = "output"
    , _parser = readMaybe
    , _suggest = \_ -> return []
    , _examples = \_ -> return ["docs.json"]
    }




getDocs :: DocsArgs -> Watchtower.DocFlags -> IO ()
getDocs arg (Watchtower.DocFlags maybeOutput) =
  case arg of
    DocsCwdProject ->
      do
          let path = "src/Main.elm"
          maybeRoot <- Stuff.findRoot
          case maybeRoot of
            Nothing ->
              System.IO.hPutStrLn System.IO.stdout "Was not able to find an elm.json!"
            
            Just root -> do
              maybeDocs <- Ext.Dev.docsForProject root path
              case maybeDocs of
                Left err ->
                  System.IO.hPutStrLn System.IO.stdout 
                      (Exit.toString (Exit.reactorToReport err))
                
                Right docs ->
                  outJson (Docs.encode docs)

    DocsPath path ->
      if Path.takeExtension path == ".elm" then do
        maybeRoot <-  Dir.withCurrentDirectory (Path.takeDirectory path) Stuff.findRoot
        case maybeRoot of
          Nothing ->
            System.IO.hPutStrLn System.IO.stdout "Was not able to find an elm.json!"
          
          Just root -> do
            maybeDocs <- Ext.Dev.docs root path
            case maybeDocs of
              Nothing ->
                System.IO.hPutStrLn System.IO.stdout "Docs are not available"

              Just docs ->
                outJson (Docs.encode (Docs.toDict [ docs ]))
      else do
        -- treat path as a directory path and produce docs as a project
        maybeRoot <-  Dir.withCurrentDirectory path Stuff.findRoot
        case maybeRoot of
          Nothing ->
            System.IO.hPutStrLn System.IO.stdout "Was not able to find an elm.json!"
          
          Just root -> do
            maybeDocs <- Ext.Dev.docsForProject root path
            case maybeDocs of
              Left err ->
                System.IO.hPutStrLn System.IO.stdout 
                    (Exit.toString (Exit.reactorToReport err))
              
              Right docs ->
                outJson (Docs.encode docs)
      

    DocsPackage packageName ->
      do
        -- Is there an `elm.json` file?  Read it for the exact version.
        maybeCurrentVersion <- Ext.Dev.Package.getCurrentlyUsedOrLatestVersion "." packageName
        case maybeCurrentVersion of
          Nothing ->
            System.IO.hPutStrLn System.IO.stdout "No version found!"

          Just packageVersion -> do   
            result <- Ext.Dev.Package.getDocs packageName packageVersion
            case result of
              Left err ->
                System.IO.hPutStrLn System.IO.stdout (Exit.toString (Exit.toDocsProblemReport err ""))

              Right docs ->
                outJson (Docs.encode docs)
        
    DocsPackageVersion name packageVersion ->
      do
        result <- Ext.Dev.Package.getDocs name packageVersion
        case result of
          Left err ->
            System.IO.hPutStrLn System.IO.stdout (Exit.toString (Exit.toDocsProblemReport err ""))

          Right docs ->
            outJson (Docs.encode docs)


outJson :: Json.Encode.Value -> IO ()
outJson jsonValue =
    System.IO.hPutStrLn System.IO.stdout (show (Json.Encode.encode jsonValue))


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


startServer :: Maybe FilePath -> Watchtower.Flags -> IO ()
startServer maybeRoot (Watchtower.Flags maybePort) =
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




{- Generator -}

data GenerateArgs =
    GenerateInteractivePath FilePath

data GenerateFlags =
  GenerateFlags
    { _generateOutput :: Maybe String
    }

{-|



-}
generate :: Terminal.Command
generate =
  let
    summary =
      "Generate some template code"

    details =
      "The `warnings` command reports missing type annotations and unused code."

    example =
      reflow
        "After running that command, watchtower is listening at <http://localhost:51213>\
        \ and ready to be connected to."

    generateFlags =
      flags GenerateFlags
        |-- flag "output" output_ "An optional file to write the JSON form of warnings to.  If not supplied, the warnings will be printed."

    generateArgs =
      oneOf 
        [ require1 GenerateInteractivePath dir
        ]
  in
  Terminal.Command "generate" (Common summary) details example generateArgs generateFlags generateFile



{-
  Going in:
    -> arbitrary JSON value



  Coming out:


type alias File =
    { path : String
    , contents : String
    , warnings :
        List
            { declaration : String
            , warning : String
            }
    }


type alias Error =
    { title : String
    , description : String
    }


or String
    





-}


generateFile :: GenerateArgs -> Watchtower.GenerateFlags -> IO ()
generateFile args (GenerateFlags maybeOutput) = 
  case args of
    GenerateInteractivePath path ->  do
      let output = Data.Maybe.fromMaybe "dev" maybeOutput
      -- treat path as a directory path and produce docs as a project
      maybeRoot <-  Dir.withCurrentDirectory path Stuff.findRoot
      case maybeRoot of
        Nothing ->
          System.IO.hPutStrLn System.IO.stdout "Was not able to find an elm.json!"
        
        Just root -> do
          maybeDocs <- Ext.Dev.docsForProject root path
          case maybeDocs of
            Left err ->
              System.IO.hPutStrLn System.IO.stdout 
                  (Exit.toString (Exit.reactorToReport err))
            
            Right docs -> do
              
              eitherElmFiles <- generateElmFiles (Docs.encode docs)

              case eitherElmFiles of
                Left err ->
                  System.IO.hPutStrLn System.IO.stdout err

                Right elmFiles ->
                  Monad.mapM_ (writeGeneratedFile (path </> output)) elmFiles



writeGeneratedFile :: String -> GeneratedFile -> IO ()
writeGeneratedFile base (GeneratedFile path contents) = do
  let dir = Path.dropFileName (base </> path)
  Dir.createDirectoryIfMissing True dir
  File.writeBuilder (base </> path) contents




{-ELM GENERATOR CODE-}

data GeneratedFile =
  GeneratedFile
      { _path :: String
      , _contents :: Data.ByteString.Builder.Builder
      }
      deriving (Show)



decodeGeneratedFile :: Json.Decode.Decoder a GeneratedFile
decodeGeneratedFile =
  GeneratedFile
      <$> (Json.String.toChars <$> (Json.Decode.field "path" Json.Decode.string))
      <*> (Data.ByteString.Builder.stringUtf8 <$> unescapeString <$> (Json.String.toChars <$> (Json.Decode.field "contents" Json.Decode.string)))


unescapeString :: String -> String
unescapeString = go
  where
    go :: String -> String
    go [] = []
    go ('\\':c:xs) = case c of
        'n' -> '\n' : go xs
        't' -> '\t' : go xs
        'r' -> '\r' : go xs
        '\\' -> '\\' : go xs
        '"' -> '"' : go xs
        _ -> '\\' : c : go xs
    go (x:xs) = x : go xs


generateElmFiles :: Json.Encode.Value -> IO (Either String [GeneratedFile])
generateElmFiles docsJson = do
  let generateCmd = "node ./out/generate.js '" ++ (show (Json.Encode.encode docsJson)) ++ "'"
  System.Process.withCreateProcess (System.Process.shell generateCmd){ System.Process.std_out = System.Process.CreatePipe }
    (\stdin maybeStdOut stderr processHandle ->
        case maybeStdOut of 
          Nothing ->
             pure (Left "No std out")
          
          Just stdout -> do
            (exitCode, output) <- gatherOutput processHandle stdout
            case exitCode of
              System.Exit.ExitSuccess ->
                -- System.IO.hPutStrLn System.IO.stdout (show output)
                case Json.Decode.fromByteString (Json.Decode.list decodeGeneratedFile) output of
                  Right json ->
                    pure (Right json)
                  
                  Left err ->
                    pure (Left "Failed to decode generated files")
              
              System.Exit.ExitFailure code ->
                pure (Left "Failed to generate Elm files")
    )



gatherOutput :: System.Process.ProcessHandle -> System.IO.Handle -> IO (System.Exit.ExitCode, Data.ByteString.ByteString)
gatherOutput prcessHandle handle = work mempty
  where
    work acc = do
        -- Read any outstanding input.
        string <- Data.ByteString.hGetNonBlocking handle (64 * 1024)
        let gathered = acc <> string
        -- Check on the process.
        maybeExitCode <- System.Process.getProcessExitCode prcessHandle
        -- Exit or loop.
        case maybeExitCode of
            Nothing -> work gathered
            Just code -> do
                -- Get any last bit written between the read and the status
                -- check.
                last <-  Data.ByteString.hGetContents handle
                return (code, gathered <> last)