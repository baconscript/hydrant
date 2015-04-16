import System.Environment
import Data.List

-- Structure for managing command-line options.

data CmdLineOpts = CmdLineOpts {
  configurationFile :: Maybe String,
  commandToRun :: Command
} deriving (Show)

data Command = NoCommand | Help (Maybe String) | CreateDatabase String
  deriving (Show)

emptyOpts :: CmdLineOpts
emptyOpts = CmdLineOpts {
  configurationFile = Nothing, 
  commandToRun = NoCommand
}

withConfigFile :: CmdLineOpts -> String -> CmdLineOpts
withConfigFile opts fnm = opts { configurationFile = Just fnm }

withCommand opts cmd = opts { commandToRun = cmd }

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

data Configuration = Configuration {
  databaseFilePath :: Maybe String
} deriving Show

defaultConfig = Configuration {
  databaseFilePath = Nothing
}

-- Handle a single line of a config file.

processConfigLine :: [String] -> Configuration -> Configuration
processConfigLine [] _ = defaultConfig
processConfigLine (c:[]) _ = defaultConfig
processConfigLine (opt:val) conf
  | opt == "database-file-path"   = conf {databaseFilePath = Just (unwords val)}
  | otherwise                     = conf

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
