module CommandParser (
  command,
  Flag,
  Arg,
  CommandMetadata(..),
  Command,
  run,
  -- Flag parsing functions
  noFlag,
  parseFlag,
  parseFlag2,
  parseFlag3,
  parseFlag4,
  -- Flag constructors
  flag,
  flagWithArg,
  -- Common flags
  helpFlag,
  -- Argument constructors
  noArg,
  arg,
  argWith,
  parseArg,
  parseArg2,
  parseArgList,
) where

{-- Command line parser!

This is a simpler command parser than the one within the elm compiler.

The parser distinguishes between three types of arguments:

1. Commands: The initial non-flag arguments that form the command path
2. Flags: Arguments starting with '-' or '--' that can have optional values
3. Positional args: Remaining non-flag arguments after commands and flags

Example:
  elm-dev server --port 3000 src/Main.elm

  Commands: ["server"]           <- Initial non-flag arguments
  Flags: [("--port", "3000")]   <- Arguments starting with - or --
  Positional: ["src/Main.elm"]  <- Remaining non-flag arguments

  ┌─────────┐  ┌───────────────┐  ┌───────────────┐
  │ Commands│  │     Flags     │  │  Positional   │
  ├─────────┤  ├───────────────┤  ├───────────────┤
  │ server  │  │ --port 3000   │  │ src/Main.elm  │
  └─────────┘  └───────────────┘  └───────────────┘

--}


import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Data.List (intercalate, partition, (\\), isPrefixOf)
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

-- | Command metadata for help text
data CommandMetadata = CommandMetadata
  { cmdName :: [String]            -- Command name
  , cmdArgs :: [String]
  , cmdGroup :: Maybe String
  , cmdDesc :: String            -- Command description
  } deriving (Show)


command ::
    [String]
    -> String
    -> Maybe String
    -> ArgParser args
    -> (ParsedArgs -> Either String (flags, ParsedArgs))
    -> (args -> flags -> IO ()) -> Command
command commandPieces desc group (ArgParser commandArgs parseCommandArgs) parseCommandFlags run = \parsedArgs -> 
  if commandPieces == parsedCommands parsedArgs then
    case parseCommandArgs parsedArgs of
      Left err -> Run (\() -> putStrLn err)
      Right (args, remainingArgs) ->
        case parseCommandFlags remainingArgs of
          Left err -> Run (\() -> putStrLn err)
          Right (flags, _) -> 
            Run (\() -> run args flags)
  else
    NotMe (CommandMetadata commandPieces commandArgs group desc)





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


data Arg a = Arg
  { argName :: String
  , argParse :: String -> Maybe a
  }


arg :: String -> Arg String
arg name = Arg
  { argName = name
  , argParse = Just
  }


argWith :: String -> (String -> Maybe a) -> Arg a
argWith name parse = Arg
  { argName = name
  , argParse = parse
  }

data ArgParser a = 
    ArgParser
        [String]
        (ParsedArgs -> Either String (a, ParsedArgs))


singleArgName :: Arg a -> String
singleArgName arg = "<" ++ argName arg ++ ">"

noArg :: ArgParser ()
noArg = ArgParser [] (\parsed -> Right ((), parsed))

parseArg :: Arg arg -> ArgParser arg
parseArg arg = ArgParser [singleArgName arg] (\parsed -> 
  case parsedPositional parsed of
    [] -> Left $ "Missing required argument: " ++ argName arg
    [value] -> 
      case argParse arg value of
        Just v -> Right (v, parsed { parsedPositional = [] })
        Nothing -> Left $ "Invalid value for arg: " ++ argName arg
    _ -> Left $ "Expected exactly one argument, but got multiple: " ++ argName arg
  )

runArgParser :: ArgParser a -> ParsedArgs -> Either String (a, ParsedArgs)
runArgParser (ArgParser names parse) raw = 
   parse raw

-- | Parse two arguments in sequence
parseArg2 :: Arg one -> Arg two -> ArgParser (one, two)
parseArg2 arg1 arg2 = ArgParser [singleArgName arg1, singleArgName arg2] (\parsed -> 
  case runArgParser (parseArg arg1) parsed of
    Left err -> Left err
    Right (value1, parsed1) ->
      case runArgParser (parseArg arg2) parsed1 of
        Left err -> Left err
        Right (value2, parsed2) -> Right ((value1, value2), parsed2)
   )

listArgName :: Arg a -> String
listArgName arg = "[" ++ argName arg ++ "]"

-- | Parse one required argument followed by any number of optional arguments
parseArgList :: Arg arg -> ArgParser (arg, [arg])
parseArgList arg = ArgParser [listArgName arg] (\parsed ->
  case runArgParser (parseArg arg) parsed of
    Left err -> Left err
    Right (value1, parsed1) -> do
      let (values, remaining) = parseOptionalArgs arg (parsedPositional parsed1)
      Right ((value1, values), parsed1 { parsedPositional = remaining })
  )
  where
    parseOptionalArgs :: Arg arg -> [String] -> ([arg], [String])
    parseOptionalArgs _ [] = ([], [])
    parseOptionalArgs arg (value:rest) =
      case argParse arg value of
        Just v -> 
          let (vs, remaining) = parseOptionalArgs arg rest
          in (v:vs, remaining)
        Nothing -> ([], value:rest)
  

noFlag :: ParsedArgs -> Either String ((), ParsedArgs)
noFlag parsed = Right ((), parsed)

-- | Parse a single flag, returning the parsed value
parseFlag :: Flag a -> ParsedArgs -> Either String (Maybe a, ParsedArgs)
parseFlag flag parsed =
  let matches = filter (\(name, _) -> findFlagName name flag) (parsedFlags parsed)
      remainingFlags = filter (\(name, _) -> not (findFlagName name flag)) (parsedFlags parsed)
      updatedParams = parsed { parsedFlags = remainingFlags }
  in
  if not (null remainingFlags)
  then Left $ "Unknown flags: " ++ intercalate ", " (map fst remainingFlags)
  else
    case matches of
      [] -> Right (Nothing, updatedParams)
      (_, value):_ -> Right (flagParse flag value, updatedParams)


-- | Parse two flags
parseFlag2 :: Flag a -> Flag b -> ParsedArgs -> Either String ((Maybe a, Maybe b), ParsedArgs)
parseFlag2 flag1 flag2 parsed = 
  case parseFlag flag1 parsed of
    Left err -> Left err
    Right (maybeA, parsed1) -> 
      case parseFlag flag2 parsed1 of
        Left err -> Left err
        Right (maybeB, parsed2) -> 
          if null (parsedFlags parsed2)
          then Right ((maybeA, maybeB), parsed2)
          else Left $ "Unknown flags: " ++ intercalate ", " (map fst $ parsedFlags parsed2)

-- | Parse three flags
parseFlag3 :: Flag a -> Flag b -> Flag c -> ParsedArgs -> Either String ((Maybe a, Maybe b, Maybe c), ParsedArgs)
parseFlag3 flag1 flag2 flag3 parsed =
  case parseFlag flag1 parsed of
    Left err -> Left err
    Right (maybeA, parsed1) ->
      case parseFlag flag2 parsed1 of
        Left err -> Left err
        Right (maybeB, parsed2) ->
          case parseFlag flag3 parsed2 of
            Left err -> Left err
            Right (maybeC, parsed3) ->
              if null (parsedFlags parsed3)
              then Right ((maybeA, maybeB, maybeC), parsed3)
              else Left $ "Unknown flags: " ++ intercalate ", " (map fst $ parsedFlags parsed3)

-- | Parse four flags
parseFlag4 :: Flag a -> Flag b -> Flag c -> Flag d -> ParsedArgs -> Either String ((Maybe a, Maybe b, Maybe c, Maybe d), ParsedArgs)
parseFlag4 flag1 flag2 flag3 flag4 parsed =
  case parseFlag flag1 parsed of
    Left err -> Left err
    Right (maybeA, parsed1) ->
      case parseFlag flag2 parsed1 of
        Left err -> Left err
        Right (maybeB, parsed2) ->
          case parseFlag flag3 parsed2 of
            Left err -> Left err
            Right (maybeC, parsed3) ->
              case parseFlag flag4 parsed3 of
                Left err -> Left err
                Right (maybeD, parsed4) ->
                  if null (parsedFlags parsed4)
                  then Right ((maybeA, maybeB, maybeC, maybeD), parsed4)
                  else Left $ "Unknown flags: " ++ intercalate ", " (map fst $ parsedFlags parsed4)

-- | Run a list of commands with parsed arguments
run :: ([CommandMetadata] -> [String] -> String) -> [Command] -> IO ()
run showHelp commands = do
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
    printHelp cmds parsed = putStr $ showHelp (collectMetadata cmds parsed) (parsedCommands parsed)

-- | Collect metadata from all commands
collectMetadata :: [Command] -> ParsedArgs -> [CommandMetadata]
collectMetadata commands parsed = 
  [meta | NotMe meta <- map (\cmd -> cmd parsed) commands]

-- | Create a help flag
helpFlag :: Flag Bool
helpFlag = flag "help" "Show help information"