import Control.Monad
import Data.Monoid

over :: Int -> Int -> (Bool, String)
over num x = (x>num, "compared number to " ++ show num)

over10 = over 10
over3  = over 3

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (a, s1) f =
  let (b', s2) = f a
  in (b', s1 `mappend` s2)


