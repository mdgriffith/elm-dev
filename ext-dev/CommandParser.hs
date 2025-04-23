module CommandParser (
  Flag(..),
  ArgInfo(..),
  CommandMetadata(..),
  CommandResult(..),
  Command,
  ParsedArgs(..),
  parseArgs,
  runCommands,
  -- Flag parsing functions
  parseFlag,
  parseFlag2,
  parseFlag3,
  parseFlag4,
  -- Flag constructors
  flag,
  flagWithArg,
  -- Common flags
  helpFlag
) where

{-- Command line parser!

This is a simpler command parser than the one within the elm compiler.



--}


import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.List (intercalate, partition, (\\))
import Data.Maybe (catMaybes, isJust, fromMaybe)
import Data.Either (Either(..))

-- | Information about a flag
data FlagInfo = FlagInfo
  { flagLong :: String     -- Long form like --verbose
  , flagDesc :: String     -- Description for help text
  , flagHasArg :: Bool     -- Whether the flag requires an argument
  } deriving (Show, Eq)

-- | Flag definition with a type parameter for the value it returns
data Flag a = Flag
  { flagInfo :: FlagInfo
  , flagParse :: Maybe String -> Maybe a
  }

-- | Information about positional arguments
data ArgInfo = ArgInfo
  { argName :: String            -- Argument name for help text
  , argDesc :: String            -- Argument description
  , argOptional :: Bool          -- Whether the argument is optional
  } deriving (Show, Eq)

-- | Command metadata for help text
data CommandMetadata = CommandMetadata
  { cmdName :: String            -- Command name
  , cmdDesc :: String            -- Command description
  , cmdArgs :: [ArgInfo]         -- Positional arguments
  , cmdSubcommands :: [CommandMetadata] -- Subcommands
  } deriving (Show)

-- | Result of attempting to parse a command
data CommandResult 
  = NotMe CommandMetadata        -- Command didn't match, here's its metadata
  | Run (() -> IO ())            -- Command matched, here's the action to run

-- | A command is a function from parsed args to a command result
type Command = ParsedArgs -> CommandResult

-- | Structured parsed arguments
data ParsedArgs = ParsedArgs
  { parsedFlags :: [(String, Maybe String)] -- (flag name, optional value)
  , parsedPositional :: [String]            -- Positional arguments
  , parsedCommands :: [String]              -- Command path taken
  } deriving (Show)

-- | Create a Flag for a boolean flag (present/not present)
flag :: String -> String -> Flag Bool
flag long desc = Flag 
  { flagInfo = FlagInfo
    { flagLong = long
    , flagDesc = desc
    , flagHasArg = False
    }
  , flagParse = \maybeVal -> case maybeVal of
      Nothing -> Just True  -- Flag is present with no value
      Just _ -> Nothing     -- Flag shouldn't have a value
  }

-- | Create a Flag that requires an argument
flagWithArg :: String -> String -> (String -> Maybe a) -> Flag a
flagWithArg long desc parse = Flag
  { flagInfo = FlagInfo
    { flagLong = long
    , flagDesc = desc
    , flagHasArg = True
    }
  , flagParse = \maybeVal -> case maybeVal of
      Nothing -> Nothing    -- Flag needs a value but doesn't have one
      Just val -> parse val -- Parse the value
  }

-- | Create an empty ParsedArgs
emptyArgs :: ParsedArgs
emptyArgs = ParsedArgs [] [] []

-- | Check if a string is a flag (starts with - or --)
isFlag :: String -> Bool
isFlag ('-':_) = True
isFlag _ = False


flagToName :: Flag a -> String
flagToName flag = "--" ++ flagLong (flagInfo flag)

-- | Find a flag by name (both short and long forms)
findFlagName :: String -> Flag a -> Bool
findFlagName arg flag = case arg of
  ('-':'-':name) -> flagLong (flagInfo flag) == name
  _ -> False

-- | Get all defined flag names from a list of flags
getAllFlagNames :: [Flag a] -> [String]
getAllFlagNames flags = concatMap (\f -> [flagToName f]) flags

-- | Parse arguments into a ParsedArgs structure
parseArgs :: [String] -> ParsedArgs
parseArgs args = 
  -- First split into command path and the rest
  let (cmdPath, restArgs) = span (not . isFlag) args
      -- Then parse flags and positional args
      (flags, positional) = parseFlags restArgs []
  in ParsedArgs flags positional cmdPath
  where
    parseFlags :: [String] -> [(String, Maybe String)] -> ([(String, Maybe String)], [String])
    parseFlags [] acc = (acc, [])
    parseFlags (arg:rest) acc
      | not (isFlag arg) = 
          let (flags, positional) = parseFlags rest acc
          in (flags, arg:positional)
      | otherwise = 
          let flagName = arg
              (value, remaining) = case rest of
                (v:rs) | not (isFlag v) -> (Just v, rs)
                _ -> (Nothing, rest)
              (flags, positional) = parseFlags remaining ((flagName, value):acc)
          in (flags, positional)

-- | Parse a single flag, returning the parsed value
parseFlag :: Flag a -> ParsedArgs -> Either String (Maybe a)
parseFlag flag parsed =
  let matches = filter (\(name, _) -> findFlagName name flag) (parsedFlags parsed)
      remainingFlags = filter (\(name, _) -> not (findFlagName name flag)) (parsedFlags parsed)
      updatedParams = parsed { parsedFlags = remainingFlags }
  in
  case matches of
    [] -> Right Nothing
    (_, value):_ -> 
      case flagParse flag value of
        Just v -> Right (Just v)
        Nothing -> Left $ "Invalid value for flag: " ++ flagToName flag


-- | Parse two flags
parseFlag2 :: Flag a -> Flag b -> ParsedArgs -> Either String (Maybe a, Maybe b)
parseFlag2 flag1 flag2 parsed = 
  case parseFlag flag1 parsed of
    Left err -> Left err
    Right maybeA -> 
      case parseFlag flag2 parsed of
        Left err -> Left err
        Right maybeB -> 
          if null (parsedFlags parsed)
          then Right (maybeA, maybeB)
          else Left $ "Unknown flags: " ++ intercalate ", " (map fst $ parsedFlags parsed)

-- | Parse three flags
parseFlag3 :: Flag a -> Flag b -> Flag c -> ParsedArgs -> Either String (Maybe a, Maybe b, Maybe c)
parseFlag3 flag1 flag2 flag3 parsed =
  case parseFlag flag1 parsed of
    Left err -> Left err
    Right maybeA ->
      case parseFlag flag2 parsed of
        Left err -> Left err
        Right maybeB ->
          case parseFlag flag3 parsed of
            Left err -> Left err
            Right maybeC ->
              if null (parsedFlags parsed)
              then Right (maybeA, maybeB, maybeC)
              else Left $ "Unknown flags: " ++ intercalate ", " (map fst $ parsedFlags parsed)

-- | Parse four flags
parseFlag4 :: Flag a -> Flag b -> Flag c -> Flag d -> ParsedArgs -> Either String (Maybe a, Maybe b, Maybe c, Maybe d)
parseFlag4 flag1 flag2 flag3 flag4 parsed =
  case parseFlag flag1 parsed of
    Left err -> Left err
    Right maybeA ->
      case parseFlag flag2 parsed of
        Left err -> Left err
        Right maybeB ->
          case parseFlag flag3 parsed of
            Left err -> Left err
            Right maybeC ->
              case parseFlag flag4 parsed of
                Left err -> Left err
                Right maybeD ->
                  if null (parsedFlags parsed)
                  then Right (maybeA, maybeB, maybeC, maybeD)
                  else Left $ "Unknown flags: " ++ intercalate ", " (map fst $ parsedFlags parsed)

-- | Run a list of commands with parsed arguments
runCommands :: ([CommandMetadata] -> [String] -> String) -> [Command] -> IO ()
runCommands showHelp commands = do
  args <- getArgs
  
  -- Special case for help command
  if not (null args) && (head args == "help" || head args == "--help" || head args == "-h") then
    let helpArgs = if length args > 1 then tail args else []
        helpParsed = ParsedArgs [] [] helpArgs
    in printHelp commands helpParsed >> exitSuccess
  else do
    let parsed = parseArgs args
        results = map (\cmd -> cmd parsed) commands
        
    case findRun results of
      Just action -> action ()
      Nothing -> do
        putStrLn "Unknown command or invalid arguments."
        printHelp commands parsed
  where
    findRun :: [CommandResult] -> Maybe (() -> IO ())
    findRun [] = Nothing
    findRun (NotMe _:rest) = findRun rest
    findRun (Run action:_) = Just action
    
    printHelp :: [Command] -> ParsedArgs -> IO ()
    printHelp cmds parsed = putStrLn $ showHelp (collectMetadata cmds parsed) (parsedCommands parsed)

-- | Collect metadata from all commands
collectMetadata :: [Command] -> ParsedArgs -> [CommandMetadata]
collectMetadata commands parsed = 
  [meta | NotMe meta <- map (\cmd -> cmd parsed) commands]

-- -- | Show help text based on collected metadata
-- showHelp :: [CommandMetadata] -> [String] -> String
-- showHelp metadata cmdPath =
--   let appName = "myapp"  -- This would come from your app config
--       usage = "Usage: " ++ appName ++ 
--               (if null cmdPath then "" else " " ++ intercalate " " cmdPath) ++
--               " [FLAGS] [ARGS] [COMMAND]"
      
--       description = case metadata of
--         (cmd:_) -> cmdDesc cmd
--         [] -> "Command line application"
      
--       argsHelp =
--         let args = concatMap cmdArgs metadata
--         in if null args then ""
--            else "\n\nArguments:\n" ++ unlines (map formatArg args)
      
--       commands = 
--         if null cmdPath 
--         then concatMap cmdSubcommands metadata
--         else []
      
--       commandsHelp =
--         if null commands then ""
--         else "\n\nCommands:\n" ++ unlines (map formatCommand commands)
--   in
--       usage ++ "\n\n" ++ description ++ argsHelp ++ commandsHelp
--   where    
--     formatArg arg =
--       let optional = if argOptional arg then " (optional)" else ""
--       in "  " ++ argName arg ++ optional ++ "\t" ++ argDesc arg
    
--     formatCommand cmd =
--       "  " ++ cmdName cmd ++ "\t" ++ cmdDesc cmd

-- | Create a help flag
helpFlag :: Flag Bool
helpFlag = flag "help" "Show help information"