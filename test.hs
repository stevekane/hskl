import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmall :: Int -> Int
doubleSmall x = if   x > 100 
                then x 
                else doubleMe x

letters = ['a'..'z'] ++ ['A'..'Z']

vowels = ['a', 'e', 'i', 'o', 'u'] ++ ['A', 'E', 'I', 'O', 'U']

consenants = [ltr | ltr <- letters, not (ltr `elem` vowels)]

devowel :: String -> String
devowel name = [ltr | ltr <- name, ltr `elem` consenants]

findSolutions :: Int -> Int -> [(Int, Int, Int)]
findSolutions size perimeter = [ (a,b,c) |
                                 a <- [1..size],
                                 b <- [1..size],
                                 c <- [1..size],
                                 a < b,
                                 a^2 + b^2 == c^2,
                                 a + b + c == perimeter ]

vecSum :: (Num a) => (a, a) -> (a, a) -> (a, a)
vecSum (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

perimeter :: (Num a) => (a, a, a) -> a
perimeter (x, y, z) = x + y + z

head' :: [a] -> a
head' []    = error "Attemped to get head of an empty list"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

bmi :: (RealFloat a) => a -> a -> a
bmi height weight = weight / (height^2)

reportBmi :: (RealFloat a) => a -> a -> String
reportBmi width height
  | val <= skinny = "You are so fabu"
  | val <= normal = "You are somewhat thin"
  | val <= fat    = "You need to lose the fatty, fatty"
  | otherwise     = "You gon die whale"
  where val       = bmi height width
        skinny    = 18.5
        normal    = 25.0
        fat       = 30.0

squareAll :: Num a => [a] -> [a]
squareAll []    = error "No this is not acceptable"
squareAll vals  = 
  let square x = x * x
  in [ square x | x <- vals ]

max' :: (Ord a) => a -> a -> a
max' a b = if a > b then a else b

maximum' :: (Ord a) => [a] -> a
maximum' []      = error "tried to find max of empty list"
maximum' [x]     = x
maximum' (x:xs)  = max' x (maximum' xs)

minimum' :: (Ord a) => [a] -> a
minimum' xs = case xs of []              -> error "tried to find min of empty list"
                         (x:[])          -> x
                         (x:xs)
                           | x < minTail -> x
                           | otherwise   -> minTail 
                           where minTail =  minimum' xs

-- book recommends this type which allows more Num types.  
-- this seems fairly pointless since we want a positive integer
-- to indicate the discrete number of replications....
-- replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' :: Int -> a -> [a]
replicate' count element
  | count <= 0   = []
  | otherwise    = element:replicate' (count-1) element

-- take' :: (Num i, Ord i) => i -> [a] -> [a]
take' :: Int -> [a] -> [a]
take' n _
  | n <= 0              = []
take' _ []              = []
take' count (head:tail) = head:take' (count-1) tail

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = (reverse' xs) ++ [x]

-- repeat' creates an infinite list of the provided element
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- zip' creates a list of tuples out of the provided lists
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _           = []
zip' _ []           = []
zip' (x:xs) (y:ys)  = (x, y):zip' xs ys

-- determine if element is present in list
elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' y (x:xs) = if y == x then True else elem' y xs

quicksort :: Ord a => [a] -> [a]
quicksort []      = []
quicksort (x:xs)  =
  let smaller = [y | y <- xs, y <= x]
      bigger  = [y | y <- xs, y > x]
  in quicksort smaller ++ [x] ++ quicksort bigger

applyTwice :: (a -> a) -> a -> a
applyTwice fn x = fn . fn $ x

applyAmount :: (Num b, Eq b, Ord b) => b -> (a -> a) -> a -> a
applyAmount c fn x
  | c <= 0    = error "cannot apply function zero times"
  | c == 1    = fn x
  | otherwise = fn (applyAmount (c-1) fn x)

zipWith' :: (a -> b ->c) -> [a] -> [b] -> [c]
zipWith' _ [] _           = []
zipWith' _ _ []           = []
zipWith' fn (x:xs) (y:ys) = fn x y : zipWith' fn xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ []      = []
map' fn (x:xs) = fn x : map' fn xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []  = []
filter' fn (x:xs)
  | fn x      = x : filter' fn xs
  | otherwise = filter' fn xs

-- find largest number less than bound that evenly divides by second param
findLargestDivisor :: Integral a => a -> a -> a
findLargestDivisor bound num = 
  let range     = [bound, bound-1]
      divides x = (mod x num) == 0
  in head (filter divides range)

sumOfOddSquares :: Integral a => a -> a
sumOfOddSquares bound =
  let squares    = map (^2) [1..]
      oddSquares = filter odd squares
  in sum (takeWhile (<bound) oddSquares)

chain :: Int -> [Int]
chain 1 = [1]
chain x
  | odd x   = x : chain (x * 3 + 1)
  | even x  = x : chain (x `div` 2)

sum'' :: Num a => [a] -> a
sum'' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys   = foldl check False ys
  where check = \prev z -> prev || (z == y)

elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' _ [] = Nothing
elemIndex' x xs = 
  let check (Just y) _ = Just y
      check Nothing [] = Nothing
      check Nothing t  = if h == x then Just i else Nothing
        where h = head t
              i = (length xs) - (length t)
  in foldl check Nothing (tails xs)

map'' :: (a -> b) -> [a] -> [b]
map'' f xs    = foldr build [] xs
  where build = \x acc -> f x : acc

-- slower as ++ operator is more expensive than : operator
map''' :: (a -> b) -> [a] -> [b]
map''' f xs   = foldl build [] xs
  where build = \acc x -> acc ++ [f x]

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldr1 (\x acc -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

product'' :: (Num a) => [a] -> a
product'' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x acc -> if f x then x : acc else acc) []

-- kind of a stupid way to define this, pattern matching is better
head'' :: [a] -> a
head'' = foldl1 (\x _ -> x)

last'' :: [a] -> a
last'' = foldr1 (\_ x -> x)

sqrtSums :: Int
sqrtSums = 
  let rootNaturals = map sqrt [1..]
      sumCount     = takeWhile (<1000) $ scanl1 (+) rootNaturals 
  in (length sumCount) + 1

and' :: [Bool] -> Bool
and' = foldl (&&) True

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldl (\acc x -> acc || f x) False

or' :: [Bool] -> Bool
or' = foldl (||) False

join' :: [a] -> [a] -> [[a]]
join' first second = [first, second]

tails' :: [a] -> [[a]]
tails' [] = []
tails' li = li : tails' (tail li)

search' :: (Eq a) => [a] -> [a] -> Bool
search' sub li =
  let sublen   = length sub
      allTails = tails' li
      check    = \acc x -> if (take sublen x == sub) then True else acc
  in foldl check False allTails

encode :: Int -> String -> String
encode shift msg = 
  let ords       = map ord msg
      shifted    = map (+ shift) ords
  in  map chr shifted

decode :: Int -> String -> String
decode shift msg  = encode backshift msg
  where backshift = negate shift

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey _   []   = Nothing
findKey key dict = 
  let maybeMatch = find (\(k,v) -> k == key) dict
  in case maybeMatch of (Just a)  -> Just $ snd a
                        (Nothing) -> Nothing

-- this is the version in the book, shorter and uses foldr
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key dict = foldr matchKey Nothing dict
  where matchKey = \(k,v) acc -> if k == key then Just v else acc

inSet' :: String -> Char -> Bool
inSet' str a = Set.member a (Set.fromList str)

-- a few examples.  Set conversion probably not needed for these
isNum'       = inSet' "0123456789"
isAlpha'     = inSet' letters
isConsenant' = inSet' consenants
isVowel'     = inSet' vowels
