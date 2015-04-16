import System.Environment
import Data.Char
import Data.List

data CmdLineOpts = CmdLineOpts {
  configurationFile :: String
} deriving (Show)

emptyOpts :: CmdLineOpts
emptyOpts = CmdLineOpts{configurationFile=""}

withConfigFile :: CmdLineOpts -> String -> CmdLineOpts
withConfigFile opts fnm = CmdLineOpts {
  configurationFile = fnm
}

processArgsRecurse :: [String] -> CmdLineOpts -> CmdLineOpts
processArgsRecurse [] _ = emptyOpts
processArgsRecurse (opt:arg:args) lastOpts
  | opt == "--config" = (processArgsRecurse args lastOpts) `withConfigFile` arg
  | otherwise         = processArgsRecurse (arg:args) lastOpts

processArgs :: [String] -> CmdLineOpts
processArgs args = processArgsRecurse args emptyOpts

main = do
  args <- getArgs
  putStrLn $ show $ processArgs args
