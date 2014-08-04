import Control.Monad
import Data.Monoid

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole  = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft bs (l,r) = 
  let balanced = abs (l+bs-r) <= 3
  in if balanced then Just (l+bs, r) else Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight bs (l,r) = 
  let balanced = abs (r+bs-l) <= 3
  in if balanced then Just (l, r+bs) else Nothing

-- convenient infix operator
x -: f = f x

noDo   = Just 5 >>= (\x -> Just (x+5))
withDo = do
  x <- Just 5
  return (x+5)

noDoChain :: Maybe Int
noDoChain   = Just 5 >>= (\x -> return (x+5)) >>= (\y -> return (y-10))
withDoChain = do
  x <- Just 5
  y <- return (x+5)
  return (y-10)

-- here we create a closure over the value x and the value y such that
-- we can use both to compute our final monadic value.  It's easier to
-- accomplish this using the do notation as it sort of flattens things out
noDoChainAr :: [Int]
noDoChainAr = [1,2,3,4] >>= (\x -> (return (x+4) >>= (\y -> return (x+y))))
doChainAr :: [Int]
doChainAr = do
  x <- [1,2,3,4]
  y <- return (x+4)
  return (x+y)

{-
   the following three functions are all functionally identical
   this shows that list comprehensions are taking advantage of the 
   fact that lists are Monoidal and Monadic and thus are part of
   the typeclass MonadPlus
-}
listComp    = [ x | x <- [1..100], x `mod` 5 == 0 ]
noDoGuarded = [1..100] >>= (\x -> guard (x `mod` 5 == 0) >> return x)
doGaurded   = do
  x <- [1..100]
  guard (x `mod` 5 == 0)
  return x

-- sample chess solver types and functions
type Position = (Int, Int)

newtype Board = Board { getBoard :: [[Position]] }

instance Show Board where
  show (Board positions) =
    let printCell cs c = cs ++ " " ++ (show c)
        printRow r     = foldl printCell "" r
        joinRow rs r   = rs ++ "\n" ++ printRow r
    in foldl joinRow "" positions

makeRow :: Int -> Int -> [Position]
makeRow count y =
  let cells    = [1..count]
      makeCell = flip (,) y
  in fmap (makeCell) cells

makeBoard :: Int -> Int -> Board
makeBoard xSize ySize = Board $ fmap (makeRow xSize) [1..ySize]

validKnightMoves :: Board -> Position -> [Position]
validKnightMoves (Board positions) (x,y) = 
  let validHor (x', y') = abs (x-x') == 2 && abs (y-y') == 1
      validVer (x', y') = abs (x-x') == 1 && abs (y-y') == 2
      isValid  (x', y') = validHor (x', y') || validVer (x', y')
  in filter isValid (concat positions)

validKnightMoves' :: Board -> Position -> [Position]
validKnightMoves' (Board positions) (x,y) = do
  let validHor (x', y') = abs (x-x') == 2 && abs (y-y') == 1
      validVer (x', y') = abs (x-x') == 1 && abs (y-y') == 2
      isValid  (x', y') = validHor (x', y') || validVer (x', y')
  (x', y') <- concat positions
  guard (isValid (x', y'))
  return (x', y')

validKnightMoves'' :: Board -> Position -> [Position]
validKnightMoves'' (Board positions) (x,y) =
  let validHor (x', y') = abs (x-x') == 2 && abs (y-y') == 1
      validVer (x', y') = abs (x-x') == 1 && abs (y-y') == 2
      isValid  (x', y') = validHor (x', y') || validVer (x', y')
  in concat positions >>= (\(x', y') -> guard (isValid (x',y')) >> return (x',y'))

-- this way is kind of clever and takes fewer computations
-- it does however require that the board's dimensions be hard-coded
validKnightMoves''' :: Board -> Position -> [Position]
validKnightMoves''' (Board positions) (x,y) = do
  (x',y') <- [(x+2, y+1), (x+1, y+2), (x-2, y-1), (x-1, y-2),
              (x+2, y-1), (x+1, y-2), (x-2, y+1), (x-1, y+2)]
  guard (x' `elem` [1..8] && y' `elem` [1..8])
  return (x',y')

b = makeBoard 8 8
possibleMoves  = validKnightMoves b (0,0)
possibleMoves' = validKnightMoves b (4,4)

in3 :: Board -> Position -> [Position]
in3 b p = do
  firstMove  <- validKnightMoves b p
  secondMove <- validKnightMoves b firstMove
  validKnightMoves b secondMove

in3' :: Board -> Position -> [Position]
in3' b p = return p >>= (validKnightMoves b) 
                    >>= (validKnightMoves b) 
                    >>= (validKnightMoves b)

canReachIn3 :: Board -> Position -> Position -> Bool
canReachIn3 b pSource pDest = pDest `elem` (in3' b pSource)
