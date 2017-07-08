import Data.Char

-- exercise: enumfromto

eftBool :: Bool -> Bool -> [Bool]
eftBool True False  = []
eftBool False True  = [False, True]
eftBool True True   = [True]
eftBool False False = [False]

eftGen :: (Enum a, Ord a) => a -> a -> [a]
eftGen a b
    | a > b = []
    | otherwise = a : eftGen (succ a) b

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftGen

eftInt :: Int -> Int -> [Int]
eftInt = eftGen

eftChar :: Char -> Char -> [Char]
eftChar = eftGen


-- exercise: thy fearful symmetry

-- 1.

myWords :: String -> [String]
myWords "" = []
myWords xs = takeWhile (/= ' ') xs : myWords (drop 1 $ dropWhile (/= ' ') xs)

-- 2.

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines ('\n' : xs) = myLines xs
myLines s = takeWhile (/= '\n') s : myLines (dropWhile (/= '\n') s)

-- What we want 'myLines sentences' to equal
shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?" ]

main :: IO ()
main =
    print $ "Are they equal? "
        ++ show (myLines sentences == shouldEqual)

-- 3.

split :: Char -> String -> [String]
split _ "" = []
split c (x:xs)
    | c == x = split c xs
    | otherwise = takeWhile (/= c) (x:xs) : split c (dropWhile (/= c) (x:xs))


-- exercise: comprehend thy lists

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

-- 1.
-- [x | x <- mySqr, rem x 2 == 0]
-- [4, 16]

-- 2.
-- [(x,y)|x<-mySqr,y<-mySqr,x<50,y>50]
-- []
-- mySqr doesn't have any element larger than 50

-- 3.
-- take 5 [(x,y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- []

-- exercise: square cube

-- 1.
-- this is not like `zip mySqr myCube`, but instead cartesian product
tpls = [(x, y) | x <- mySqr, y <- myCube]

-- 2.
tpls' = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3.
numTpls = length tpls'


-- exercise: will it blow up?

-- 1.
-- [x^y | x <- [1..5], y <- [2, undefined]]
-- error

-- 2.
-- take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
-- [1]

-- 3.
-- sum [1, undefined, 3]
-- error

-- 4.
-- length [1, 2, undefined]
-- 3

-- 5.
-- length $ [1, 2, 3] ++ undefined
-- error

-- 6.
-- take 1 $ filter even [1, 2, 3, undefined]
-- value [2]

-- 7.
-- take 1 $ filter even [1, 3, undefined]
-- error

-- 8.
-- take 1 $ filter odd [1, 3, undefined]
-- value: [1]

-- 9.
-- take 2 $ filter odd [1, 3, undefined]
-- value: [1,3]

-- 10.
-- take 3 $ filter odd [1, 3, undefined]
-- error


-- exercise: is it normal form?

-- 1.
-- [1,2,3,4,5]
-- NF

-- 2.
-- 1 : 2 : 3 : 4 : _
-- WHNF

-- 3.
-- enumFromTo 1 10
-- neither

-- 4.
-- length [1, 2, 3, 4, 5]
-- neither

-- 5.
-- sum (enumFromTo 1 10)
-- neither

-- 6.
-- ['a'..'m'] ++ ['n'..'z']
-- neither

-- 7.
-- (_, 'b')
-- WHNF


-- exercise: more bottoms

-- 1.
-- take 1 $ map (+1) [undefined, 2, 3]
-- error

-- 2.
-- take 1 $ map (+1) [1, undefined, 3]
-- [2]

-- 5.
-- take 2 $ map (+1) [1, undefined, 3]
-- error

-- 4.
-- itIsMystery xs = map (\x -> elem x "aeiou") xs
-- returns list of bools, where true if character is a vowel

-- 5.
-- a) map (^2) [1..10]
-- [1,4,9,16,25,36,49,64,81,100]

-- b) map minimum [[1..10], [10..20], [20..30]]
-- [1,10,20]

-- c) map sum [[1..5], [1..5], [1..5]]
-- [15,15,15]

-- 6.
-- map (\x -> bool x (-x) (x == 3)) [1..10]


-- exercise: filtering

-- 1.
-- filter (\x -> x `mod` 3 == 0) [1..30]

-- 2.
-- length $ filter (\x -> x `mod` 3 == 0) [1..30]

-- 3.
myFilter :: String -> [String]
myFilter s = filter (\x -> x `notElem` ["the", "a", "an"]) (split ' ' s)


-- exercise: zipping

-- 1.
zip' :: [a] -> [b] -> [(a, b)]
zip' [] [] = []
zip' [] _  = []
zip' _ []  = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs

-- 2.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] [] = []
zipWith' f [] _  = []
zipWith' f _ []  = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

-- 3.
zip'' = zipWith' (,)


-- exercise: data.char

-- 1.
-- :t isUpper
-- isUpper :: Char -> Bool
-- :t toUpper
-- toUpper :: Char -> Char

-- 2.
makeUpper = map toUpper

-- 3.
capitalise "" = ""
capitalise (c:ss) = toUpper c : map toLower ss

-- 4.
recursiveCapitalise "" = ""
recursiveCapitalise (c:ss) = toUpper c : recursiveCapitalise ss

-- 5, 6.
capitaliseHead = toUpper . head


-- exercise: ciphers
-- see cipher.hs


-- exercise: writing your own standard functions

-- 1.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs
-- myOr (x:xs) = x || myOr xs

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
    | f x = True
    | otherwise = myAny f xs

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
    | x == a = True
    | otherwise = myElem a xs

-- 4.
myReverse :: [a] -> [a]
myReverse = rev []
    where
        rev acc [] = acc
        rev acc (x:xs) = rev (x:acc) xs

-- 5.
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- 6.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy f (x:xs) = mb f x xs
    where
        mb :: (a -> a -> Ordering) -> a -> [a] -> a
        mb _ m [] = m
        mb f m (x:xs)
            | f x m == GT = mb f x xs
            | otherwise = mb f m xs

-- 9.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy f (x:xs) = mb f x xs
    where
        mb :: (a -> a -> Ordering) -> a -> [a] -> a
        mb _ m [] = m
        mb f m (x:xs)
            | f x m == LT = mb f x xs
            | otherwise = mb f m xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare


