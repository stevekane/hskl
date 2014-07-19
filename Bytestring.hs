import System.Environment
import System.IO
import System.IO.Error
import System.Directory
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB

-- main = do 
--   print $ show $ LB.pack [99, 82, 12]
--   print $ show $ B.pack [99, 82, 12]

copyFile' :: FilePath -> FilePath -> IO ()
copyFile' orig dest = do
  contents <- B.readFile orig
  B.writeFile dest contents

tryCopy :: IO ()
tryCopy = do
  (original:_) <- getArgs
  contents     <- B.readFile original
  putStrLn $ "The file has " ++  (show . length . lines) contents ++ " lines"

handler :: IOError -> IO () 
handler e = putStrLn "We had issues Jim.  Real.  Human.  Issues"

-- main = do
--   (original:copy:_) <- getArgs
--   origExists        <- doesFileExist original
--   if origExists 
--     then do putStrLn "the file exists"
--             copyFile' original copy 
--     else do putStrLn "the file did not exist"

-- main = do
--   exists <- doesFileExist "test.hs"
--   print exists

main = tryCopy `catch` handler
