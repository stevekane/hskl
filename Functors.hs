import Control.Applicative

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

-- lets find all the combinations of triangles as before
type Triangle = (Int, Int, Int)

findSolutions :: Int -> Int -> [Triangle]
findSolutions size perimeter =
  let domain     = liftA3 (,,) [1..size] [1..size] [1..size]
      fn (a,b,c) = (a^2 + b^2 == c^2) && (a + b + c == perimeter)
  in filter fn domain

allTrue :: (Eq a, Ord a) => [(a -> Bool)] -> a -> Bool
allTrue fs x = all (==True) $ sequenceA fs x
