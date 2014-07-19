import Data.Char
import Data.List (intersperse)
import Control.Concurrent
import Control.Monad (when, forever)
import System.IO

-- main = putStrLn "Boop"

-- main = do
--   putStrLn "What is your name?"
--   name <- getLine
--   putStrLn ("Whatup " ++ name ++ ".  You are awesome")

-- main = do
--   putStrLn "First name?"
--   firstName <- getLine
--   putStrLn "Last name?"
--   lastName <- getLine
--   let bigFullName = map toUpper (firstName ++ " " ++ lastName)
--   putStrLn bigFullName

-- main = do
--   threadDelay 5000000
--   putStrLn "done"

-- getText :: IO String
-- getText = do
--   putStrLn "Enter a word or sentence for translation: "
--   getLine
-- 
makeSwedish = intersperse 'f'
-- 
-- main = do
--   text <- getText
--   if null text
--     then putStrLn "You must be bored of this."
--     else do
--       putStrLn ("Swedish: " ++ makeSwedish text)
--       main

-- print' :: (Show a) => a -> IO ()
-- print' = putStrLn . show
-- 
-- main = do
--   let bool = True
--       ar   = ["Cats", "Dogs"]
--       num  = 5 
--   print' bool
--   print' ar
--   print' num

-- main = do
--   input <- getLine
--   when (input /= "q") $ do
--     putStrLn $ "You entered: " ++ input
--     main 

-- printAll :: (Show a) => [a] -> IO ()
-- printAll []     = return ()
-- printAll [x]    = print x
-- printAll (x:xs) = do
--   print x 
--   printAll xs
-- 
-- main = do
--   inputs <- sequence [getLine, getLine, getLine]  
--   printAll $ map makeSwedish inputs

-- sequence is typically used to transform a list of IO actions 
-- into an actual IO action
-- map print [1,2,3,4] won't actually DO the actions
-- sequence $ map print [1,2,3,4] will 

-- perform sequence of io actions and throw away result
-- main = do
--   mapM_ print [1,2,3]

-- main = forever $ do
--   putStrLn "Lets chat"
--   l      <- getLine
--   putStr $ (map toUpper l) ++ "\n"

-- getShortLines :: String -> String
-- getShortLines s = unlines [ x | x <- (lines s), length x < 10 ]
-- getShortLines s = unlines $ filter ((<10) . length) (lines s)

-- main = do
--   contents <- getContents
--   putStr   $ getShortLines contents

-- this is sugar for getContents, do transform, print result
-- main = interact getShortLines

linewiseTransform :: (String -> String) -> String -> String
linewiseTransform f = unlines . (map f) . lines

areLinesPalindromes :: String -> String
areLinesPalindromes =
  let isPal w     = w == reverse w
      toTxt True  = "palindrome"
      toTxt False = "not a palindrome"
      lineChecker = toTxt . isPal
  in linewiseTransform lineChecker

-- main = interact areLinesPalindromes

-- main = do
--   handle   <- openFile "girlfriend.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

-- sugar for dealing w/ files
-- main = do
--   let fileName = "girlfriend.txt"
--       reader h = do
--         contents  <- hGetContents h
--         putStr contents
--   withFile fileName ReadMode reader

-- more sugar for reading a file
-- main = do
--   contents <- readFile "girlfriend.txt"
--   putStr contents 

main = do
  contents   <- readFile "girlfriend.txt"
  let allCaps = map toUpper contents
  writeFile "girlfriendcaps.txt" allCaps
