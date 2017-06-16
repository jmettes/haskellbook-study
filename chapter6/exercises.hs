
import Data.List

data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn a') = a == a'

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt =
    TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt a') = a == a'
    (==) (TisAString a) (TisAString a') = a == a'
    (==) _ _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a a') (Pair b b') = (a == b) && (a' == b')

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = (a == a) && (b == b')

data Which a =
      ThisOne a
    | ThatOne a

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

data EitherOr a b =
      Hello a
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) _ _ = False

-- Exercises: Tuple Experiment

-- λ> :t quotRem
-- quotRem :: Integral a => a -> a -> (a, a)
-- λ> :t divMod
-- divMod :: Integral a => a -> a -> (a, a)
-- both return tuple of (division, remainder), however divMod will always return (_,y) s.t. y>=0


-- Exercise: why doesn't the below require (Num a, Fractional a)?

-- divideThenAdd :: Fractional a => a -> a -> a divideThenAddxy=(x/y)+1
-- Answer: because Fractional inherits Num
--       VVVVV
-- class Num a => Fractional a where
--   (/) :: a -> a -> a
--   ...


-- Exercise: will they work?

-- max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
    -- yes: 5
-- compare(3*4)(3*5)
    -- yes: LT
-- compare "Julie" True
    -- no: type error
-- (5+3)>(3+6)
    -- yes: False


-- exercise: will it typecheck?
-- 1.

-- data Person = Person Bool
-- printPerson :: Person -> IO ()
-- printPerson person = putStrLn (show person)

    -- no because of Person does not derive from Show

-- 2.
-- data Mood = Blah
--                | Woot deriving Show

-- settleDown x = if x == Woot
--     then Blah
--     else x

    -- no, needs to derive Eq

-- Exercise: do these typecheck, if not why?
type Subject = String
type Verb = String
type Object = String
data Sentence =
    Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool" -- this typechecks, just don't print it
s2 = Sentence "Julie" "loves" "dogs" -- this typechecks & can be printed

-- Exercise: will these type check? if not, why?

data Rocks =
    Rocks String
    deriving (Eq, Show)

data Yeah =
    Yeah Bool
    deriving (Eq, Show)

data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)

-- 1.
-- this doesn't typecheck because it expects a 'Rocks String' and 'Yeah Bool'
-- instead it is given a String and Bool directly
-- phew = Papu "chases" True

-- 2.
-- this typechecks
truth = Papu (Rocks "chomskydoz")
    (Yeah True)
-- 3.
-- this should typecheck, because Papu derives Eq
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4.
-- this doesn't typecheck because Papu doesn't derive Ord
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'


-- Exercise: match the types

-- 1.
-- this does not work because 1 is a concrete value and needs to be constrained
-- f :: a
-- f = 1.0

-- 2.
-- this doesn't typecheck because 1.0 is more specific than Num (it's Fractional)
-- f :: Num a => a
-- f = 1.0

-- 3.
-- this typechecks
f :: Fractional a => a
f = 1.0

-- 4.
-- this typechecks because it is a subclass of Fractional
f' :: RealFrac a => a
f' = 1.0

-- 5.
-- this typechecks because Ord is pretty general
freud :: Ord a => a -> a
freud x = x

-- 6.
-- this typechecks as long as it's not passed something other than a Int
freud' :: Int -> Int
freud' x = x

-- 7.
-- same as question 1., it's given a concrete type, so must be constrained
myX = 1 :: Int
-- sigmund :: a -> a
-- sigmund x = myX

-- 8.
-- this doesn't typecheck because Int is more specific than Num
-- sigmund' :: Num a => a -> a
-- sigmund' x = myX

-- 9.
-- i couldn't get 'sort' working, ghc couldn't find it for some reason, so i replaced it with minimum
-- this typechecks, because String/Char is a subclass of Ord
jung :: String -> Char
jung xs = head (sort xs)

-- 10.
-- this typechecks
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- 11.
-- this typechecks
mySort :: String -> String
mySort = sort

signifier :: Ord a => [a] -> a
signifier xs = head (sort xs)


-- Exercise: type-kwon-do

-- 1.
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

-- 2.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a + fromInteger i
