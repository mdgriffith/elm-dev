{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Terminal.Dev where

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




getWarnings :: Warnings -> Terminal.Dev.WarningFlags -> IO ()
getWarnings arg (Terminal.Dev.WarningFlags maybeOutput) =
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




getDocs :: DocsArgs -> Terminal.Dev.DocFlags -> IO ()
getDocs arg (Terminal.Dev.DocFlags maybeOutput) =
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

