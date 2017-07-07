import Data.List (intersperse)

-- exercise: intermission
applyTimes :: (Eq a, Num a) =>
                a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n-1) f $ b
-- (+1) . applyTimes (5-1) (+1) $ 5
-- (+1) . (+1) . applyTimes (4-1) (+1) $ 5
-- (+1) . (+1) . (+1) . applyTimes (3-1) (+1) $ 5
-- (+1) . (+1) . (+1) . (+1) . applyTimes (2-1) (+1) $ 5
-- (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes (1-1) (+1) $ 5
-- (+1) . (+1) . (+1) . (+1) . (+1) $ 5
-- 10

-- exercises: review of types
-- 1. a)
-- 2. b)
-- 3. d)
-- 4. b)

-- exercises: reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. "woops mrow woohoo!"
-- 2. "1 mrow haha"
-- 3. "woops mrow 2 mrow haha"
-- 4. "woops mrow blue mrow haha"
-- 5. "pink mrow haha mrow green mrow woops mrow blue"
-- 6. are mrow Pugs mrow awesome"

-- exercises: recursion
-- 1.
-- dividedBy 15 2
-- go 15 2 0
-- go (15 - 2) 2 (0 + 1)
-- go (15 - 2 - 2) 2 (0 + 1 + 1)
-- go (15 - 2 - 2 - 2) 2 (0 + 1 + 1 + 1)
-- go (15 - 2 - 2 - 2 - 2) 2 (0 + 1 + 1 + 1 + 1)
-- go (15 - 2 - 2 - 2 - 2 - 2) 2 (0 + 1 + 1 + 1 + 1 + 1)
-- go (15 - 2 - 2 - 2 - 2 - 2 - 2) 2 (0 + 1 + 1 + 1 + 1 + 1 + 1)
-- go (15 - 2 - 2 - 2 - 2 - 2 - 2 - 2) 2 (0 + 1 + 1 + 1 + 1 + 1 + 1 + 1)
-- go 1 2 7
-- (7,1)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where
        go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

-- 2.
summer :: (Eq a, Num a) => a -> a
summer = summer' 0
    where
        summer' n 0 = n
        summer' n i = summer' (n + i) (i - 1)

-- 3.
multer :: (Integral a) => a -> a -> a
multer a 0 = 0
multer 0 b = 0
multer a 1 = a
multer 1 b = b
multer a b = a + multer a (b - 1)

-- exercise: fixing dividedBy
data DividedResult a =
     Result (a, a)
    | DividedByZero
    deriving (Show)

dividedBy' :: Integral a => a -> a -> DividedResult a
dividedBy' 0 _ = Result (0, 0)
dividedBy' _ 0 = DividedByZero
dividedBy' num denom =
    go (abs num) (abs denom) 0
    where
        posneg num denom
            | num < 1 && denom < 1 = 1
            | num >= 1 && denom >= 1 = 1
            | otherwise = -1
        go n d count
            | n < d = Result (count * posneg num denom, n)
            | otherwise = go (n - d) d (count + 1)

-- exercise: mccarthy 91

mc91 n
    | n > 100  = n-10
    | n <= 100 = mc91 (mc91 (n+11))


-- exercise: numbers into words


digitToWord :: Int -> String
digitToWord n = case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"

digits :: Int -> [Int]
digits n = calc [] 1
    where
        calc ds count
            | n < count = ds
            | otherwise =  calc (n `div` count `mod` 10 : ds) (count * 10)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord (digits n)

-- implement factorial using Y Combinator
-- i.e., without using explicit recursion (function using its own name in its body)
-- http://mvanier.livejournal.com/2897.html

factorial 0 = 1
factorial n = n * factorial (n - 1)

fact :: (Eq a, Num a) => a -> a
fact = recurse fact'

fact' _ 0 = 1
fact' f n = n * f (n - 1)

recurse :: (a -> a) -> a
recurse f  = f (recurse f)

