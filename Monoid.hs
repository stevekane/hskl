import Data.Monoid
import qualified Foldable as F

newtype Any' = Any' { getAny' :: Bool }
  deriving (Show, Read, Bounded, Eq, Ord)

newtype All' = All' { getAll' :: Bool }
  deriving (Show, Read, Bounded, Eq, Ord)

-- this type of Boolean monoid is based on || being the mappend
-- and False being the mempty
instance Monoid Any' where
  mempty                    = Any' False
  mappend (Any' a) (Any' b) = Any' (a || b)

-- this type of Boolean monoid is based on && being the mappend
-- and True being the mempty
instance Monoid All' where
  mempty                    = All' True
  mappend (All' a) (All' b) = All' (a && b)

-- example useage of a monoid for string length comparison that 
-- terminates as soon as possible
-- first example is a "classic way to do this"
lengthCompare :: String -> String -> Ordering
lengthCompare s1 s2 = compare (length s1) (length s2)

-- now lets say that if the length of the strings is the same,
-- we want to alphabetically compare them before returning an Ord
lengthCompare' :: String -> String -> Ordering
lengthCompare' s1 s2 = 
  let l1   = length s1
      l2   = length s2
      same = l1 == l2
  in if same then compare s1 s2 else compare l1 l2

-- now w/ more monoidness
lengthCompare'' :: String -> String -> Ordering
lengthCompare'' x y = (length x) `compare` (length y) `mappend` (x `compare` y)


