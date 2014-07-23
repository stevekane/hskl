import Control.Applicative

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

-- lets find all the combinations of triangles as before
type Triangle = (Int, Int, Int)

-- What does this do?
-- domain applies the (,,) constructor to the applicative
-- combination of three arrays producing an array of all possible
-- combinations of the three arrays organized into Triangles
-- we can be even cuter and use liftA3
-- domain = liftA3 (,,) as bs cs
findSolutions :: Int -> Int -> [Triangle]
findSolutions size perimeter =
  let as         = [1..size]
      bs         = [1..size]
      cs         = [1..size]
      -- domain     = (,,) <$> as <*> bs <*> cs
      domain     = liftA3 (,,) as bs cs
      fn (a,b,c) = (a^2 + b^2 == c^2) && (a + b + c == perimeter)
  in filter fn domain
