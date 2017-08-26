-- chapter exercises

-- determine the kinds
-- 1.
-- a :: *  ?

-- 2.
-- a :: *, f :: * -> * ?


-- string processing
-- 1.

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe xs = unwords $ fmap (f . notThe) (words xs)
    where
        f (Just a) = a
        f Nothing = "a"

-- replaceThe (a:b:c:xs)
--     | [a, b, c] == "the" = 'a' : replaceThe xs
--     | otherwise = a : replaceThe (b : c : xs)
-- replaceThe (x:xs) = x : replaceThe xs
-- replaceThe [] = []


-- 2.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = toInteger $ length $ filter theThenVowel (zip w (drop 1 w))
    where
        w = words xs
        theThenVowel (x,y) = x == "the" && vowel (head y)

vowel :: Char -> Bool
vowel a = case a of
        'a' -> True
        'e' -> True
        'i' -> True
        'o' -> True
        'u' -> True
        _ -> False

-- 3.
countVowels :: String -> Integer
countVowels = toInteger . length . filter vowel


-- validate the word
newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s
    | numVowels > length s - numVowels = Nothing
    | otherwise = Just (Word' s)
        where
            numVowels = length $ filter (`elem` vowels) s

-- it's only natural
data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger (Succ n) = 1 + natToInteger n
natToInteger Zero = 0

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0 = Nothing
    | otherwise = Just (recr n)
    where
        recr n = if n > 0 then Succ (recr (n-1)) else Zero

-- small library for maybe

-- 1.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing = True

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just x) = f x
mayybee y f Nothing = y

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe y Nothing = y

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe [] = Nothing

maybeToList (Just x) = [x]
maybeToList Nothing = []

-- 5.
catsMaybe :: [Maybe a] -> [a]
catsMaybe (Just x:xs) = x : catsMaybe xs
catsMaybe (Nothing:xs) = catsMaybe xs
catsMaybe [] = []

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = flipMaybe' xs []
    where
        flipMaybe' (Just x:xs) acc = flipMaybe' xs (x:acc)
        flipMaybe' (Nothing:xs) _ = Nothing
        flipMaybe' [] acc = Just acc


-- small library for either

-- 1.
left :: Either a b -> [a]
left (Left x) = [x]
left _ = []

right :: Either a b -> [b]
right (Right x) = [x]
right _ = []

-- 1.
lefts' :: [Either a b] -> [a]
lefts' = foldr ((++) . left) []

-- 2.
rights' :: [Either a b] -> [b]
rights' = foldr ((++) . right) []

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _ _ = Nothing

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

-- 6.
eithermaybe'' :: (b -> c) -> Either a b -> Maybe c
eithermaybe'' f (Right x) = Just (either' f f (Right x))
eithermaybe'' _ _ = Nothing


-- unfolds

-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- 2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f acc = case f acc of
    Nothing -> []
    Just (x, y) -> x : myUnfoldr f y

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))


-- finally something other than a list

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- 1.
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f acc = case f acc of
    Nothing -> Leaf
    Just (x, y, z) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f n
    where
        f 0 = Nothing
        f n' = Just (n'-1, n-n', n'-1)