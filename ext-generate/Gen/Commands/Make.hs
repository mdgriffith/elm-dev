{-# LANGUAGE OverloadedStrings #-}
module Gen.Commands.Make (command, run) where

import qualified CommandParser
import qualified Terminal
import qualified Data.Char as Char
import Terminal ((!), (?), (...))
import Data.Text (Text)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Terminal.Helpers
import qualified Elm.ModuleName
import qualified Gen.Javascript
import qualified Gen.Generate
import qualified Gen.Config
import qualified Data.Name as Name
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Gen.Config as Config
import qualified Gen.Commands.Init
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BS
import qualified Make
import System.FilePath ((</>))
import Data.Text (pack)
import Data.Aeson (eitherDecodeStrict)
import qualified Data.Text as Text
import qualified Data.List (find)
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.FilePath ((<.>))
import qualified Gen.Templates
import qualified Gen.Templates.Loader
import Data.Function ((&))
import qualified Data.Text.Encoding
import qualified System.Directory as Dir (doesFileExist, removeFile, getCurrentDirectory, createDirectoryIfMissing, withCurrentDirectory)
import qualified Reporting.Exit as Exit
import Control.Monad (when)
import qualified System.FilePath as FP
import qualified Terminal.Colors
import qualified Ext.Log
import qualified System.IO as IO
import qualified Stuff
import qualified Ext.CompileProxy


-- MAKE COMMAND
command :: CommandParser.Command
command = CommandParser.command ["make"] 
                "Build your Elm project"
                Nothing
                parseMakeArgs
                parseMakeFlags
                (run Nothing)
  where 
    parseMakeFlags = CommandParser.parseFlag3
                        (CommandParser.flag "debug" "Debug mode")
                        (CommandParser.flag "optimize" "Make the code smaller and faster")
                        (CommandParser.flagWithArg "output" "Output file path" Make.parseOutput)
    parseMakeArgs =
       CommandParser.parseArgList (CommandParser.arg "module")
       

run :: Maybe FilePath -> (String, [String]) -> (Maybe Bool, Maybe Bool, Maybe Make.Output) -> IO ()
run maybeCwd (fstModule, modules) (debug, optimize, output) = do
    let cwd = fromMaybe "." maybeCwd
    Dir.withCurrentDirectory cwd $ do
        codegenResult <- Gen.Generate.run
        case codegenResult of
            Right () -> do
                Ext.Log.with [Ext.Log.ElmCompilerError] $
                    Make.run 
                        (fstModule : modules)
                        (Make.Flags
                            (fromMaybe False debug)
                            (fromMaybe False optimize)
                            output
                            Nothing
                            Nothing
                        )
                return ()
            Left err -> do
                putStrLn err
                return ()


