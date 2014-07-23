import Data.List
import System.Environment
import System.IO

data Section = Section { getA :: Int, getB :: Int, getC:: Int } deriving (Show)
data Label = A | B | C deriving (Show)
type RoadSystem = [Section]
type Path = [(Label, Int)]

groupsOf :: Int -> [x] -> [[x]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

buildRoadSystem :: [Int] -> RoadSystem
buildRoadSystem xs = 
  let groups                = groupsOf 3 xs
      makeSection ([a,b,c]) = Section a b c
  in map makeSection groups

costOfPath :: Path -> Int
costOfPath = sum . (map snd)

computePaths :: (Path, Path) -> Section -> (Path, Path)
computePaths (pa, pb) (Section a b c) =
  let priceA     = costOfPath pa
      priceB     = costOfPath pb
      aToA       = priceA + a
      aToB       = priceA + b + c
      bToB       = priceB + b
      bToA       = priceB + a + c
      newPathToA = if bToA <= aToA then (A,a):pa else (C,c):(B,b):pb
      newPathToB = if aToB <= bToB then (B,b):pb else (C,c):(A,a):pa
  in (newPathToA, newPathToB)

showRoute :: Path -> String
showRoute = foldl (\acc (l,_) -> acc ++ " -> " ++ show l) ""

minPath :: RoadSystem -> Path
minPath rs = 
  let (pA, pB) = foldl computePaths ([], []) rs
      costA    = costOfPath pA
      costB    = costOfPath pB
  in if costA > costB then reverse pB else reverse pA

rs = [(Section 10 2 4), (Section 4 50 60), (Section 10 12 14)]

main = do
  nodes    <- getArgs
  let nums  = map read nodes
      rs    = buildRoadSystem nums
      path  = minPath rs
      route = showRoute path
      cost  = costOfPath path
  print path
  print route
  print cost
