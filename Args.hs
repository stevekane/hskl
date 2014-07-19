import System.Environment
import Data.List

-- read the list of command line arguments supplied at runtime
main = do
  args     <- getArgs
  progName <- getProgName 
  putStrLn "The arguments are: "
  mapM putStrLn args
  putStrLn "The program is called: "
  putStrLn progName
