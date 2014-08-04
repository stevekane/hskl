import Data.Monoid
import Control.Applicative ((<$>))

type Error       = String
type Result      = Maybe Error
type Condition a = ((a -> Bool), Error)

checkCondition :: (Ord a, Eq a) => Condition a -> a -> Result
checkCondition (f, m) x = if f x then Nothing else Just m

checkConditions :: (Ord a, Eq a) => [Condition a] -> a -> [Result]
checkConditions cs x = sequence (checkCondition <$> cs) x

getValues :: [Maybe a] -> [a]
getValues = map getValue . filter onlyJust
  where onlyJust Nothing  = False
        onlyJust (Just _) = True
        getValue (Just x) = x

getResults :: [Result] -> Maybe [Error]
getResults rs = if hasErrors then Just errors else Nothing
  where errors    = getValues rs
        hasErrors = length errors == 0

-- this version uses the monoidal nature of strings and maybes
getResults' :: [Result] -> Maybe Error
getResults' = foldr1 mappend
