import Data.Monoid
import Data.List (intersperse)

-- this is a simple clock monoid for the second hand

type Second    = Int
type Minute    = Int
type Hour      = Int

newtype SimpleClock = SimpleClock { getSeconds :: Second }
  deriving (Bounded, Eq, Ord)

data Clock = Clock { getH :: Hour,
                     getM :: Minute,
                     getS :: Second }
                     deriving (Bounded, Eq, Ord)


instance Monoid SimpleClock where
  mempty                                  = SimpleClock 0   
  mappend (SimpleClock x) (SimpleClock y) = SimpleClock $ mod (x + y) 60

instance Show SimpleClock where
  show (SimpleClock x) = show x ++ "s"

instance Monoid Clock where
  mempty                                 = Clock 0 0 0
  mappend (Clock h m s) (Clock h' m' s') =
    let mFromS = div (s + s') 60
        newS   = mod (s + s') 60
        hFromM = div (m + m' + mFromS) 60
        newM   = mod (m + m' + mFromS) 60
        newH   = mod (h + h' + hFromM) 12 
    in Clock newH newM newS

instance Show Clock where
  show (Clock h m s) = show h ++ ":" ++ show m ++ ":" ++ show s
  -- show (Clock h m s) = concat . (intersperse ":") . (fmap show) $ [h,m,s]
