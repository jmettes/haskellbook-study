data Node a = DeadEnd a
            | Passage a (Node a)
            | Fork    a (Node a) (Node a)
            deriving (Show)

-- get :: Node a -> a
-- get (Passage x _) = x
-- get (DeadEnd x) = x
-- get (Fork x _ _) = x

-- put :: a -> Node a -> Node a
-- put a (DeadEnd _) = DeadEnd a
-- put a (Passage _ n) = Passage a n
-- put a (Fork _ x y) = Fork a x y

maze = Fork (0,0)
        (Fork (1,0)
            (DeadEnd (2,0))
            (DeadEnd (2,1)))
        (Passage (0,1)
            (Fork (0,2)
                (Passage (1,2)
                    (DeadEnd (2,2)))
                (DeadEnd (0,3))))

-- turnRight :: Node a -> Maybe (Node a)
-- turnRight (Fork _ l r) = Just r
-- turnRight _            = Nothing

-- data Branch = KeepStraightOn
--             | TurnLeft
--             | TurnRight
-- type Thread = [Branch]

-- turnRight :: Thread -> Thread
-- turnRight t = t ++ [TurnRight]

-- retrieve :: Thread -> Node a -> a
-- retrieve []                  n             = get n
-- retrieve (KeepStraightOn:bs) (Passage _ n) = retrieve bs n
-- retrieve (TurnLeft      :bs) (Fork _ l r)  = retrieve bs l
-- retrieve (TurnRight     :bs) (Fork _ l r)  = retrieve bs r

-- update :: (a -> a) -> Thread -> Node a -> a
-- update f t n = f (retrieve t n)


--------------------


data Branch a  = KeepStraightOn a
               | TurnLeft  a (Node a)
               | TurnRight a (Node a)
type Thread a  = [Branch a]

type Zipper a = (Thread a, Node a)

turnRight :: Zipper a -> Maybe (Zipper a)
turnRight (t, Fork x l r) = Just (TurnRight x l : t, r)
turnRight _               = Nothing

turnLeft :: Zipper a -> Maybe (Zipper a)
turnLeft (t, Fork x l r) = Just (TurnLeft x r : t, l)
turnLeft _               = Nothing

keepStraightOn :: Zipper a -> Maybe (Zipper a)
keepStraightOn (t, Passage x n) = Just (KeepStraightOn x : t, n)
keepStraightOn _                = Nothing

back :: Zipper a -> Maybe (Zipper a)
back ([]                   , _) = Nothing
back (KeepStraightOn x : t , n) = Just (t, Passage x n)
back (TurnLeft  x r    : t , l) = Just (t, Fork x l r)
back (TurnRight x l    : t , r) = Just (t, Fork x l r)

get :: Zipper a -> a
get (_, Passage x _) = x
get (_, DeadEnd x) = x
get (_, Fork x _ _) = x

put :: a -> Zipper a -> Zipper a
put a (t, DeadEnd _) = (t, DeadEnd a)
put a (t, Passage _ n) = (t, Passage a n)
put a (t, Fork _ x y) = (t, Fork a x y)

retrieve :: Zipper a -> a
retrieve ([], n)                             = get ([], n)
retrieve (KeepStraightOn _:bs, Passage _ n)  = retrieve (bs, n)
retrieve (TurnLeft      _ _:bs, Fork _ l r)  = retrieve (bs, l)
retrieve (TurnRight     _ _:bs, Fork _ l r)  = retrieve (bs, r)

update :: (a -> a) -> Zipper a -> a
update f = f . retrieve

-------

data Tree a = Leaf a | Bin (Tree a) (Tree a)
data TreeBranch a = LeftBranch a (Tree a) | RightBranch a (Tree a)
type TreeThread a = [TreeBranch a]
type TreeZipper a = (TreeThread a, Tree a)

data List a = Empty | Cons a (List a)
type ListZipper a = (List a, List a)

