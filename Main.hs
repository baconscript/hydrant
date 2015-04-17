import System.Environment
import Data.List
import Data.Maybe

-- Structure for managing command-line options.

data CmdLineOpts = CmdLineOpts {
  configurationFile :: Maybe String,
  commandToRun :: Command
} deriving (Show)

emptyOpts :: CmdLineOpts
emptyOpts = CmdLineOpts {
  configurationFile = Nothing, 
  commandToRun = NoCommand
}

withConfigFile :: CmdLineOpts -> String -> CmdLineOpts
withConfigFile opts fnm = opts { configurationFile = Just fnm }

withCommand opts cmd = opts { commandToRun = cmd }

-- Represent a command that the user wants to perform.

data Command = NoCommand 
             | Help (Maybe String) 
             | CreateDatabase String
  deriving (Show)

-- Run a command.
runCommand :: Command -> IO ()
runCommand NoCommand = runCommand (Help Nothing)

-- TODO: break the printed text out into a file
runCommand (Help Nothing) = putStrLn $ unlines [
    "Usage: hydrant <opts>... <command>",
    "",
    "Options:",
    "  --config, -c      Specify configuration file location",
    "",
    "Commands:",
    "  create <name>     Create a database with a given name",
    "  help [<topic>]    Get help, optionally on a certain topic"
  ]
runCommand (Help (Just topic)) = putStrLn ("Help about " ++ topic)
runCommand (CreateDatabase db) = putStrLn ("Creating database " ++ db)

-- Process a list of command-line arguments.

processArgs :: [String] -> CmdLineOpts -> CmdLineOpts

processArgs [] _ = emptyOpts

processArgs (opt:[]) opts
  | opt == "help"                     = nextOpts `withCommand` (Help Nothing)
  | otherwise                         = nextOpts
  where nextOpts = processArgs [] opts

processArgs (opt:arg:args) opts
  | opt == "--config" || opt == "-c"  = nextOpts `withConfigFile` arg
  | opt == "create" && arg /= ""      = nextOpts `withCommand` (CreateDatabase arg)
  | opt == "help"                     = nextOpts `withCommand` (Help (Just arg))
  | otherwise                         = nextOpts1
  where nextOpts = processArgs args opts
        nextOpts1 = processArgs (arg:args) opts

-- Represent the contents of a configuration file.

defaultPort :: Int
defaultPort = 59795

data Configuration = Configuration {
  databaseFilePath :: Maybe String,
  port :: Int
} deriving Show

defaultConfig = Configuration {
  databaseFilePath = Nothing,
  port = defaultPort
}

-- Handle a single line of a config file.

processConfigLine :: [String] -> Configuration -> Configuration

processConfigLine ("port":val) conf = case (listToMaybe $ ((reads $ unwords val)::[(Int, String)])) of
    Just (port, _) -> conf {port = port}
    Nothing -> conf

processConfigLine ("database-file-path":val) conf = conf {databaseFilePath = Just(unwords val)}

processConfigLine (_:_) conf = conf

processConfigLine _ conf = conf

-- Handle an entire config file passed in as a double list.

processConfigFile :: [[String]] -> Configuration -> Configuration
processConfigFile [] opts = opts
processConfigFile (line:lines) opts = processConfigFile lines (processConfigLine line opts)

-- Read a config file as a configuration.

readConfigFile :: CmdLineOpts -> IO Configuration
readConfigFile opts = do
  let fnm = configurationFile opts
  contents <- case fnm of
    Just fnm -> readFile fnm
    Nothing -> return ""
  return $ processConfigFile (map words (lines contents)) defaultConfig


main = do
  args <- getArgs
  let opts = processArgs args emptyOpts
  cfg <- readConfigFile opts
  putStrLn $ show $ opts
  putStrLn $ show $ cfg
  runCommand $ commandToRun opts
