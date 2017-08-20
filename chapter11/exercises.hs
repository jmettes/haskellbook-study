{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Int
import Data.Char

-- exercise: dog types

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

-- 1. type constructor
-- 2. * -> *
-- 3. String -> *
-- 4. Num a => Doggies a
-- 5. Doggies Integer
-- 6. Doggies [Char]
-- 7. both
-- 8. doge -> DogueDeBordeaux doge
-- 9. DogueDeBordeaux [Char]


-- exercise: vehicles

data Price =
    Price Integer
    deriving (Eq, Show)

data Size =
    Size Integer
    deriving (Eq, Show)


data Manufacturer =
    Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1. vehicle
-- 2.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3.
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = error "no manufacturer"

-- 4.
-- it will throw an error


-- exercise: cardinality

-- 1. cardinality: 1
-- 2. cardinality: 3
-- 3. cardinality: 127 + 128 + 1
-- 4.
    -- λ> maxBound::Int
    -- 9223372036854775807
    -- λ> minBound::Int
    -- -9223372036854775808
    -- 9223372036854775807 + 9223372036854775808 + 1 == 2^64
    -- so i'd say it's a 64 bit number, with a cardinality of 2^64

    -- Integer has instance of minBound/maxBound
    -- makes sense for infinitely long numbers.
    -- λ> minBound::Integer
    -- <interactive>:114:1: error:
    --     • No instance for (Bounded Integer)

-- 5. the 8 in Int8 refers to the number of binary bits of memory used for the number
-- i.e., 8.0 == logBase 2 (fromInteger (abs (toInteger (minBound::Int8)) + toInteger (maxBound::Int8) + 1))
-- i.e., 2^8 == 256


-- exercise: for example

data Example = MakeExample Int deriving Show

-- 1.
-- :t MakeExample
-- MakeExample :: Example
-- :t Example
-- error: Data constructor not in scope: Example

-- 2.
-- :i Example
-- data Example = MakeExample
-- instance [safe] Show Example

-- 3.
-- Int -> Example


-- exercise: logic goats

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- instance TooMany Goats where
--     tooMany (Goats n) = tooMany n

-- 1.
instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n

-- 2.
instance TooMany (Int, Int) where
    tooMany (a, b) = tooMany (a + b)

-- 3.
instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (a, b) = tooMany (a + b)


-- exercise: pity the bool

-- 1.
data BigSmall =
    Big Bool
    | Small Bool
    deriving (Eq, Show)

-- cardinality: 2 + 2 = 4

-- 2.
data NumberOrBool =
    Numba Int8
    | BoolyBool Bool
    deriving (Eq, Show)

-- myNumba = Numba (-128)
-- cardinality: (127 + 1 + 128) + 2 = 258

-- if given Int8 larger than 127, it will overflow bitrange and think its negative signed
-- 128 = 010000000 (in 9 bits)
-- however in 8 bits, 10000000 = -128

-- if given Int8 smaller than -128, it will overflow and wrap around in positive range
-- 129 = 101111111 (in 9 bits)
-- however in 8 bits?? 01111111 = 127


-- exercise: how does your garden grow?

-- 1.

data Garden = Gardenia Gardener
                | Daisy Gardener
                | Rose Gardener
                | Lilac Gardener
                deriving Show
-- sum of products
type Gardener = String



-- exercise: programmers

data OperatingSystem =
       GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill | Mac
    | Windows
    deriving (Eq, Show)

data ProgrammingLanguage =
       Haskell
        | Agda
        | Idris
        | PureScript
        deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
                , lang :: ProgrammingLanguage }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer o l | l <- allLanguages,
                                   o <- allOperatingSystems]


-- exponentiation in what order?

-- yes, 3 -> 2 = 2^3 = 8
-- proof:

data Quantum = 
      Yes
    | No
    | Both
    deriving (Eq, Show)

convert :: Quantum -> Bool
convert Yes = False
convert No = False
convert Both = False

convert2 :: Quantum -> Bool
convert2 Yes = False
convert2 No = False
convert2 Both = True

convert3 :: Quantum -> Bool
convert3 Yes = False
convert3 No = True
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes = False
convert4 No = True
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes = True
convert5 No = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes = True
convert6 No = False
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes = True
convert7 No = True
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes = True
convert8 No = True
convert8 Both = True


-- exercise: the quad

data Quad =
      One
    | Two
    | Three
    | Four
    deriving (Eq, Show)

-- 1. 4 * 4 = 16
-- 2. 4 * 4 = 16
-- 3. 4 ^ 4 = 256
-- 4. 2 * 2 * 2 = 8
-- 5. 4 ^ 4 ^ 2 = 4 ^ (4*2)
-- 6. 2 -> 4 -> 4 = 2 ^ (4 * 4) = 65536
    -- this is because a -> b -> c == (a,b) -> c


-- write map for binarytree

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected then print "yup okay!"
    else error "test failed!"


-- convert binary trees to lists: see treetolist.hs


-- write foldr for binarytree

foldTree :: (b -> a -> b) -> b -> BinaryTree a -> b
foldTree f b Leaf = b
foldTree f b (Node left a right) = foldTree f (f (foldTree f b left) a) right
-- foldTree (++) "_" (Node (Node Leaf "1" Leaf) "2" (Node Leaf "3" Leaf))
-- in order



data BT a =
      L
    | N a (BT a) (BT a)
    deriving (Eq, Ord, Show)

mkLeftTree :: [a] -> BT a
-- mkLeftTree [] tree = tree
-- mkLeftTree (a:as) tree = mkLeftTree as (Node tree a Leaf)
mkLeftTree = foldr (\a tree -> N a tree L) L

foldT :: (b -> a -> b) -> b -> BT a -> b
foldT f b L = b
foldT f b (N a left right) = foldT f (f (foldT f b left) a) right


-- chapter exercises

-- multiple choice
-- 1.  a)
-- 2. c)
-- 3. b)
-- 4. c)

-- ciphers

encode :: String -> String -> String
encode key = vigenere (map ord key)

decode :: String -> String -> String
decode key = vigenere (map (negate . ord) key)

vigenere :: [Int] -> String -> String
vigenere shifts msg = map shift (zipFixingSpaces (concat $ repeat shifts) msg)

shift :: (Int, Char) -> Char
shift (_, ' ') = ' '
shift (k,m) = chr ((ord m - ord 'A' + (k - ord 'A')) `mod` 26 + ord 'A')

zipFixingSpaces :: [Int] -> String -> [(Int, Char)]
zipFixingSpaces (k:key) (m:msg)
    | m == ' ' = (k, ' ') : zipFixingSpaces (k:key) msg
    | otherwise = (k, m) : zipFixingSpaces key msg
zipFixingSpaces _ [] = []
zipFixingSpaces [] _ = []

-- as patterns

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xs@(a:as) ys@(b:bs)
    | a == b = isSubsequenceOf as bs
    | otherwise = isSubsequenceOf xs bs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\xs@(x:xs') -> (xs, toUpper x : xs')) . words

-- language exercises

-- 1.
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

-- 2.
capitalizeParagraph :: String -> String
capitalizeParagraph = capitalizeParagraph' . capitalizeWord
    where
        capitalizeParagraph' [] = []
        capitalizeParagraph' ('.':' ':x:xs) = '.':' ':toUpper x: capitalizeParagraph' xs
        capitalizeParagraph' ('.':x:xs) = '.':toUpper x: capitalizeParagraph' xs
        capitalizeParagraph' (x:xs) = x : capitalizeParagraph' xs

-- phone exercise

-- 1.
data DaPhone = DaPhone [(Digit, String)] deriving (Eq, Show)

daphone = DaPhone [('1', "1"),
                   ('2', "ABC"),
                   ('3', "DEF"),
                   ('4', "GHI"),
                   ('5', "JKL"),
                   ('6', "MNO"),
                   ('7', "PQRS"),
                   ('8', "TUV"),
                   ('9', "WXYZ"),
                   ('*', "*^"),
                   ('0', "+_"),
                   ('#', "#.,")]

-- 2.
convo :: [String]
convo =
    ["Wanna play 20 questions",
     "Ya",
     "Lol ok. Have u ever tasted alcohol lol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char
type Presses = Int

presses :: Char -> (Digit, String) -> (Digit, Presses)
presses c (c2, s) = (c2, (+1) $ length $ takeWhile (/= c) s)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone phone) d
    | isUpper d = ('*', 1) : f d
    | otherwise = f (toUpper d)
    where
        f x = map (presses x) $ filter (elem x . snd) phone
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concat . map (reverseTaps phone)

-- 3.
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

-- 4.
-- this is probably pretty inefficient, but i'm feeling lazy
-- but i'd like to do it without importing anything, and using only what's taught so far
mostPopularLetter :: String -> Char
mostPopularLetter s = snd $ maximum [(count x s, x) | x <- ['A'..'z']]
count x = length . filter (==x)

letterCost :: DaPhone -> String -> Char -> Presses
letterCost phone msg c = keyCost * count c msg
    where keyCost = sum $ map snd $ reverseTaps phone c

-- 5.
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

-- again, probably inefficient
coolestWord :: [String] -> String
coolestWord s = snd $ maximum [(count x (allWords s), x) | x <- allWords s]
    where
        allWords = concat . map (words . stripSymbols)
        stripSymbols = filter (\x -> isAlpha x || x == ' ')

-- hutton's razor
-- 1.

data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Add e1 e2) = eval e1 + eval e2
eval (Lit i) = i

-- 2.
printExpr :: Expr -> String
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2
printExpr (Lit i) = show i