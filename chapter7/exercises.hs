-- exercise anonymous functions

addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \x -> x + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

-- exercise Variety Pack

k (x, y) = x
-- k :: (a, b) -> a
k1 = k ((4 - 1), 10)
k2 = k ("three", (1 + 2))
-- k2 :: [Char]. not same type as k1,k3
k3 = k (3, True)
-- k3 returns 3

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- exercise: case practice

-- 1.
functionC x y = case x > y of
    True  -> x
    False -> y

-- 2.
ifEvenAdd2 n = case even n of
    True  -> n + 2
    False -> n

-- 3.
nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        EQ -> 0

-- exercise artful dodgy
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

-- 1. answer: 1
-- 2. answer: 11
-- 3. answer: 22
-- 4. answer: 21
-- 5. answer: 12
-- 6. answer: 11
-- 7. answer: 21
-- 8. answer: 21
-- 9. answer: 22
-- 10. answer: 31
-- 11. answer: 23

-- exercise: guard duty

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
    | y >= 0.7 =  'C'
    | y >= 0.9 =  'A'
    | y >= 0.8 =  'B'
    | y >= 0.59 = 'D'
    | y <  0.59 = 'F'
    where y = x / 100

-- 1. answer: always returns 'F'
-- 2. answer: if y >= 0.7 at top, it will give 'C', short-circuiting 'A' & 'B'
-- 3.
pal xs
    | xs == reverse xs = True
    | otherwise = False
    -- b) is correct 
-- 4. answer: [a]
-- 5. answer: pal :: Eq a => [a] -> Bool
-- 6.
numbers x
    | x < 0  = -1
    | x == 0 = 0
    | x > 0  = 1
    -- c) is correct
-- 7. (Eq a, Num a) => a
-- 8. numbers :: (Ord a, Num a, Num b) => a -> Num b

-- exercise: let's write code
-- 1.

-- a) 
tensDigit :: Integral a => a -> a
tensDigit = fst . flip divMod 10
    -- fst . divMod 10
    -- d where xLast = x `div` 10
    --         d     = xLast `mod` 10

-- b) yes
-- c)
genD :: Integral a => a -> a -> a
genD y = fst . flip divMod y

hunsD :: Integral a => a -> a
hunsD = genD 100

-- 2.
foldBool :: a -> a -> Bool -> a
foldBool x y z = case z of
    True  -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y z
    | z = x
    | otherwise = y

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- for 4, 5, 6: arith4.hs