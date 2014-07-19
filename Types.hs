import qualified Data.Map as Map

data Point2D = Point2D Float Float 
               deriving Show

data Shape2D = Circle    Point2D Float | 
               Rectangle Point2D Point2D | 
               Square    Point2D Float 
               deriving Show

data Vec3D a = Vec3D a a a 
               deriving (Eq, Show, Read)

instance Functor Vec3D where
  fmap f (Vec3D a b c) = Vec3D (f a) (f b) (f c)

-- example from book
data LockerState = Taken | 
                   Free 
                   deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

infixr 5 :-:
infixr 5 .++

data List' a = Empty | 
               a :-: (List' a) 
               deriving (Show, Read, Eq, Ord)

(.++) :: List' a -> List' a -> List' a
Empty      .++ l2    = l2
l1         .++ Empty = l1
(x :-: xs) .++ ys    = x :-: (xs .++ ys)

surface :: Shape2D -> Float
surface (Circle _ r)                                = pi * (r^2)
surface (Rectangle (Point2D x0 y0) (Point2D x1 y1)) = (abs $ x1-x0) * (abs $ y1-y0)
surface (Square _ s)                                = s^2

vAdd :: (Num a) => Vec3D a -> Vec3D a -> Vec3D a
vAdd (Vec3D a b c) (Vec3D d e f) = Vec3D (a+d) (b+e) (c+f)

vMult :: (Num a) => a -> Vec3D a -> Vec3D a
vMult s (Vec3D a b c) = Vec3D (a*s) (b*s) (c*s)

vScalarMult :: (Num a) => Vec3D a -> Vec3D a -> a
vScalarMult (Vec3D a b c) (Vec3D d e f) = (a*d) + (b*e) + (c*f)

-- look up a locker to determine if it exists and is available
lookupLocker :: Int -> LockerMap -> Either String Code
lookupLocker lockerNum map = 
  case Map.lookup lockerNum map of
    Nothing           -> Left "There was no locker found"
    Just (Taken, _)   -> Left "That locker is taken"
    Just (Free, code) -> Right code

-- for convenience in the repl, we define some lockers
lockers :: LockerMap
lockers = Map.fromList [
  (1, (Taken, "123456")),
  (2, (Taken, "abcdef")),
  (3, (Free,  "omg4srs")),
  (4, (Free,  "booberries"))
  ]

class Toju t where
  toju :: j a -> t a j

-- we know that j a is considered a concrete type 
-- if a is a concrete type then j a has kind * -> *
-- therefore t -> * -> (* -> *) -> *
-- t takes a (* -> *) and a * and produces a *
-- t :: * -> (* -> *) -> *

data Goof a b = Goof { gfield :: b a } deriving (Show)

instance Toju Goof where
  toju x = Goof x

data Barry t k p = Barry { yabba :: p, dabba :: t k } deriving (Show)

instance Functor (Barry a b) where
  fmap f (Barry { yabba = x, dabba = y }) = Barry { yabba = f x, dabba = y }
