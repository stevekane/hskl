import System.Random

data Coin = Heads | 
            Tails 
            deriving (Show, Read, Eq, Ord)

-- this works but it seems really jankity.  think more about it?
-- there are also several more typeclass methods that should be defined
instance Random Coin where
  random g = 
    let (b, g') = random g
    in case b of True  -> (Heads, g')
                 False -> (Tails, g')

tossThree :: StdGen -> (Coin, Coin, Coin)
tossThree gen =
  let (c1, g1) = random gen
      (c2, g2) = random g1
      (c3, g3) = random g2
  in (c1, c2, c3)

main = do
  g <- getStdGen
  print (tossThree g)
