-- exercise: understanding folds

-- 1.
-- foldr (*) 1 [1..5]
-- same as c) foldl (*) 1 [1..5]

-- 2.

-- foldl (flip (*)) 1 [1..3]
-- foldl (flip (*)) 1 (1 : 2 : 3 : [])
-- foldl (flip (*)) ((flip (*)) 1 1) (2 : 3 : [])
-- foldl (flip (*)) ((flip (*)) 1 2) (3 : [])
-- foldl (flip (*)) ((flip (*)) 2 3) ([])
-- 6

-- 3. One difference between foldr and foldl is:
-- c) foldr, but not foldl, associates to the right

-- 4. Folds are catamorphisms, which means they are generally used to
-- a) reduce structure

-- 5.
-- a) foldr (++) "" ["woot", "WOOT", "woot"]
-- b) foldr max "" ["fear", "is", "the", "little", "death"]
-- c) foldr (&&) True [False, False]
-- d) foldr (||) False [False, False]
    -- ??
-- e) foldr ((++) . show) "" [1..5]
    -- alternatively (does flip results): foldl (flip((++) . show)) "" [1..5]
-- f) foldl const 0 "tacos"
    -- or: foldr (flip const) 0 "tacos"
-- g) foldl const 0 "tacos"
    -- or: foldr (flip const) 0 "tacos"
-- h) foldr (flip const) 0 "burritos"
    -- or: foldl const 0 "burritos"
-- i) foldl (flip const) 'z' [1..5]
    -- or: foldl const 'z' [1..5]


-- exercise: database processing

import Data.Time

data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
        (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123)) , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
    ]

-- 1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (DbDate x:xs) = x : filterDbDate xs
filterDbDate (_:xs) = filterDbDate xs

-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (DbNumber x:xs) = x : filterDbNumber xs
filterDbNumber (_:xs) = filterDbNumber xs

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral . sumDb) db / (fromIntegral . length . filterDbNumber) db


-- exercise: scans

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- 1.
fibs20 = fibs !! 20

-- 2.
fibslt100 = [x | x <- fibs, x < 100]

-- 3.
fact = scanl (*) 1 [1..]
factN x = fact !! x


-- chapter exercises

-- exercise: warm-up and review

-- 1.
stops = "pbtdkg"
vowels = "aeiou"

-- a)
word = [(a, b, c) | a <- stops, b <- vowels, c <- stops]

-- b)
wordP = [(a, b, c) | a <- stops, b <- vowels, c <- stops, a == 'p']

-- c)
nouns = ["bird", "tree", "lamp"]
verbs = ["flies", "grows", "shines"]

sentences = [(a, b, c) | a <- nouns, b <- verbs, c <- nouns]

-- 2.
seekritFunc x =
    div (sum (map length (words x)))
            (length (words x))
-- separates string by spaces to get words, and calculates their average length

-- 3.
seekritFunc' x =
    fromIntegral (sum (map length (words x))) / fromIntegral(length (words x))


-- exercise: rewriting functions using folds

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (x ==)) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if f x then x : xs else xs) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy f (x:xs) = foldl (\y z -> if f y z == GT then y else z) x (x:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy f (x:xs) = foldl (\y z -> if f y z == LT then y else z) x (x:xs)