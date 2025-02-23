{-# LANGUAGE GADTs #-}
module Terminal.Interactive
    ( Command(..)
    , SubCommand(..)
    , Description(..)
    , ArgParser(..)
    , runCommand
    , printHelp
    ) where

import qualified System.IO as IO
import qualified System.Console.ANSI as ANSI
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Char as Char

-- | Main command structure
data Command = Command
    { commandName :: String
    , commandDescription :: String
    , subCommands :: [SubCommand]
    }

-- | Subcommand structure that can have either static or dynamic options
data SubCommand = SubCommand
    { subName :: String
    , subDescription :: Description
    , subOptions :: SubCommandOptions
    }

-- | Options for a subcommand - either static list or IO-generated list
data SubCommandOptions where
    StaticOptions :: [(String, Description, Maybe ArgParser)] -> SubCommandOptions
    DynamicOptions :: IO [(String, Description, Maybe ArgParser)] -> SubCommandOptions

-- | Description with optional details
data Description = Description
    { shortDesc :: String
    , longDesc :: Maybe String
    }

-- | Parser for additional arguments
data ArgParser = ArgParser
    { argName :: String
    , argDescription :: String
    , validate :: String -> Either String String
    }

-- ANSI color helpers
green, cyan, grey :: String -> String
green s = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green] ++ s ++ ANSI.setSGRCode [ANSI.Reset]
cyan s = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan] ++ s ++ ANSI.setSGRCode [ANSI.Reset]
grey s = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White] ++ s ++ ANSI.setSGRCode [ANSI.Reset]

-- | Run a command with given arguments
runCommand :: Command -> [String] -> IO ()
runCommand cmd args = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetBuffering IO.stdout IO.NoBuffering
    case args of
        [] -> interactiveSelect cmd
        (subcmd:rest) -> handleSubCommand cmd subcmd rest

-- | Interactive selection for subcommands
interactiveSelect :: Command -> IO ()
interactiveSelect cmd = do
    ANSI.clearScreen
    ANSI.setCursorPosition 0 0
    -- Implementation for interactive selection with arrows
    -- Would include cursor movement, option highlighting, etc.
    undefined -- TODO: Implement interactive selection

-- | Print help screen
printHelp :: Command -> IO ()
printHelp cmd = do
    putStrLn $ "Welcome to " ++ commandName cmd
    putStrLn ""
    mapM_ printSubCommand (subCommands cmd)
    where
        printSubCommand sub = do
            putStr $ "  " ++ green (subName sub) ++ " "
            putStrLn $ replicate (20 - length (subName sub)) '.' ++ " " ++ shortDesc (subDescription sub)

-- Example usage:
exampleCommand :: Command
exampleCommand = Command "elm-prefab" "Elm Dev Tools" 
    [ SubCommand "add" (Description "Add components to your app" Nothing) $
        StaticOptions
            [ ("page", Description "Add a new page" Nothing, Just $ ArgParser "url" "Page URL" Right)
            , ("store", Description "Add a new store" Nothing, Just $ ArgParser "name" "Store name" validateElmModule)
            , ("effect", Description "Add a new effect" Nothing, Just $ ArgParser "name" "Effect name" validateElmModule)
            ]
    , SubCommand "customize" (Description "Customize components" Nothing) $
        DynamicOptions (return [("module-name", Description "Customize a module" Nothing, Nothing)])
    ]

-- | Validate Elm module names
validateElmModule :: String -> Either String String
validateElmModule s = 
    if all (\part -> not (null part) && isValidElmIdentifier part) (splitOn "." s)
    then Right s
    else Left "Invalid Elm module name. Should be in format Like.This"
    where
        isValidElmIdentifier (c:cs) = Char.isUpper c && all isValidChar cs
        isValidElmIdentifier _ = False
        isValidChar c = Char.isAlphaNum c || c == '_'
        splitOn c = foldr (\x acc -> if x == c then []:acc else (x:head acc):tail acc) [[]]
