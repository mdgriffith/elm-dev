{-# LANGUAGE OverloadedStrings #-}

module Watchtower.MCP where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as Builder
import qualified Json.Encode as Json
import Json.Encode ((==>))
import Control.Monad.IO.Class (liftIO)
import Snap.Core
import qualified Snap.Util.CORS as CORS
import qualified System.Clock as Clock
import qualified Data.Time.Clock as Time

-- | Represents a command that can be executed by the MCP
data Command = Command
    { commandName :: String
    , commandHandler :: Snap ()
    , commandDescription :: String
    }

-- | The MCP state containing registered commands
data MCPState = MCPState
    { commands :: Map.Map String Command
    , startTime :: Time.UTCTime  -- Add start time to track uptime
    }

-- | Create a new empty MCP state
newMCPState :: IO MCPState
newMCPState = do
    now <- Time.getCurrentTime
    return $ MCPState Map.empty now

-- | Register a new command with the MCP
registerCommand :: MCPState -> Command -> MCPState
registerCommand state cmd = 
    state { commands = Map.insert (commandName cmd) cmd (commands state) }

-- | Handle MCP requests
handleMCP :: MCPState -> Snap ()
handleMCP state = CORS.applyCORS CORS.defaultOptions $ do
    maybeAction <- getParam "action"
    case maybeAction of
        Just "list" -> listCommands state
        Just action -> executeCommand state (BS.unpack action)
        Nothing -> writeBS "No action specified"

-- | List all available commands
listCommands :: MCPState -> Snap ()
listCommands state = do
    let cmdList = Map.elems (commands state)
    let json = Json.encodeUgly $ Json.list encodeCommand cmdList
    writeBuilder json
  where
    encodeCommand cmd = Json.object
        [ "name" ==> Json.chars (commandName cmd)
        , "description" ==> Json.chars (commandDescription cmd)
        ]

-- | Execute a specific command
executeCommand :: MCPState -> String -> Snap ()
executeCommand state cmdName = 
    case Map.lookup cmdName (commands state) of
        Just cmd -> commandHandler cmd
        Nothing -> writeBS $ BS.pack $ "Unknown command: " ++ cmdName

-- | Create a new command
createCommand :: String -> Snap () -> String -> Command
createCommand name handler desc = Command name handler desc

-- | Example command: Get server uptime
uptimeCommand :: MCPState -> Command
uptimeCommand state = createCommand "uptime" handler "Get server uptime in seconds"
  where
    handler = do
        now <- liftIO Time.getCurrentTime
        let uptime = Time.diffUTCTime now (startTime state)
        let json = Json.encodeUgly $ Json.object
                [ "uptime_seconds" ==> Json.number (realToFrac uptime)
                , "uptime_formatted" ==> Json.chars (formatUptime uptime)
                ]
        writeBuilder json

-- | Format uptime into a human-readable string
formatUptime :: Time.NominalDiffTime -> String
formatUptime diff = 
    let seconds = round diff
        minutes = seconds `div` 60
        hours = minutes `div` 60
        days = hours `div` 24
    in unwords $ filter (not . null) 
        [ if days > 0 then show days ++ " days" else ""
        , if hours `mod` 24 > 0 then show (hours `mod` 24) ++ " hours" else ""
        , if minutes `mod` 60 > 0 then show (minutes `mod` 60) ++ " minutes" else ""
        , show (seconds `mod` 60) ++ " seconds"
        ]
