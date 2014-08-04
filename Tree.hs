import qualified Data.List     as List
import qualified Data.Map      as Map
import qualified Data.Foldable as F
import qualified Data.Monoid   as M

data Tree a = Empty |
              Node a (Tree a) (Tree a)
              deriving (Show, Eq, Ord)

instance Functor Tree where
  fmap f Empty          = Empty
  fmap f (Node a tl tr) = Node (f a) (fmap f tl) (fmap f tr)

instance F.Foldable Tree where
  foldMap f Empty          = M.mempty 
  foldMap f (Node a tl tr) = F.foldMap f tl `M.mappend` 
                             f a `M.mappend` 
                             F.foldMap f tr

tsingleton :: a -> Tree a
tsingleton x = Node x Empty Empty

tinsert :: (Ord a) => a -> Tree a -> Tree a
tinsert x Empty          = tsingleton x
tinsert x (Node n lt rt)
  | x == n = Node n lt rt
  | x <  n = Node n (tinsert x lt) rt
  | x >  n = Node n lt (tinsert x rt)

tinlist :: (Ord a) => a -> Tree a -> Bool
tinlist x Empty          = False
tinlist x (Node v tl tr)
  | x == v = True
  | x <  v = tinlist x tl
  | x >  v = tinlist x tr

tfromlist :: (Ord a) => [a] -> Tree a
tfromlist = foldr tinsert Empty
