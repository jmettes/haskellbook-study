-- 2.
f :: a -> a -> a
f a b = a

f' :: a -> a -> a
f' a b = b

-- f'' :: (Num a) => a -> a -> a
-- f'' a b = a + b:t 

-- 3.
g :: a -> b -> b
g _ b = b
-- the only implementation (flip const), different parameters types doesn't matter

myConcat x = x ++ " yo"

myMult x = (x / 3) * 5

myCom x = x > length [1..10]

-- f :: zed -> Zed -> Blah
--      [0]    [1]    [2]
-- [0] fully polymorphic, [1] concrete, [2] concrete

-- f :: Enum b => a -> b -> C
--               [0]   [1]  [2]
-- [0] fully polymorphic, [1] constrained polymorphic (Enum), [2] concrete

-- f :: f -> g -> C
--     [0]  [1]  [2]
--     [0] fully polymophic, [1] fully polymorphic, [2] concrete


functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = x > y

functionS :: (a, a) -> a
functionS (x, y) = y

myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, yToZ (xToY x))

i :: a -> a
i = id

c :: a -> b -> a
c = const

c'' :: b -> a -> b
c'' = c

c' :: a -> b -> b
c' = flip const

r :: [a] -> [a]
r (_:xs) = xs
r [] = []

co :: (b -> c) -> (a -> b) -> a -> c
co f g = f . g

a :: (a -> c) -> a -> a
a _ a = a

a' :: (a -> b) -> a -> b
a' f = f

-- 1

f''' :: Int -> String
f''' = undefined

g''' :: String -> Char
g''' = undefined

h :: Int -> Char
h = g''' . f'''

-- 2.

data A

data B

data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- 3.

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4.

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge fxy fywz = fst . fywz . fxy
