safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeUncons :: [a] -> Maybe (a,[a])
safeUncons [] = Nothing
safeUncons (x:xs) = Just (x,xs)

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix [] l = Just l
stripPrefix _ [] = Nothing
stripPrefix (x:xs) (y:ys)
    | x==y = stripPrefix xs ys
    | otherwise = Nothing

findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex el l = iter l 0
    where
        iter [] _ = Nothing
        iter (x:xs) i
            | x == el = Just i
            | otherwise = iter xs (i+1)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f [] = []
mapMaybe f (x:xs) = consider (f x)
    where
        consider Nothing = mapMaybe f xs
        consider (Just val) = val : mapMaybe f xs

data BinTree a
    = EmptyBT
    | BT a (BinTree a) (BinTree a)

maxSumPath :: BinTree Int -> Int
maxSumPath EmptyBT = 0
maxSumPath (BT val left right) = val + max (maxSumPath left) (maxSumPath right)

prune :: BinTree a -> BinTree a
prune EmptyBT = EmptyBT
prune (BT a EmptyBT EmptyBT) = EmptyBT
prune (BT a left right) = BT a (prune left) (prune right)

bloom :: BinTree a -> BinTree a
bloom EmptyBT = EmptyBT
bloom (BT a EmptyBT EmptyBT) = BT a (BT a EmptyBT EmptyBT) (BT a EmptyBT EmptyBT)
bloom (BT a left right) = BT a (bloom left) (bloom right)

rotateRight :: BinTree a -> BinTree a
rotateRight (BT q (BT p a b) c) = BT p a (BT q b c)
rotateRight t = t

rotateLeft :: BinTree a -> BinTree a
rotateLeft (BT p a (BT q b c)) = BT q (BT p a b) c
rotateLeft t = t

instance Functor BinTree where
    fmap f EmptyBT = EmptyBT
    fmap f (BT val left right) = BT (f val) (fmap f left) (fmap f right)

type BST = BinTree

bstInsert :: Ord a => a -> BST a -> BST a
bstInsert el EmptyBT = BT el EmptyBT EmptyBT
bstInsert el (BT root left right)
    | el <= root = BT root (bstInsert el left) right
    | otherwise = BT root left (bstInsert el right)

bstSearch :: Ord a => a -> BST a -> Bool
bstSearch _ EmptyBT = False
bstSearch el (BT root left right)
    | root == el = True
    | root>el = bstSearch el left
    | otherwise = bstSearch el right

bstValues :: BST a -> [a]
bstValues EmptyBT = []
bstValues (BT root left right) = bstValues left ++ [root] ++ bstValues right

bstSize :: BST a -> Int
bstSize EmptyBT = 0
bstSize (BT root left right) = 1 + bstSize left + bstSize right

fromList :: Ord a => [a] -> BST a
fromList = foldr bstInsert EmptyBT

bstSort :: Ord a => [a] -> [a]
bstSort l = bstValues $ fromList l

data Pair a b = Pair a [b]
instance Eq a => Eq (Pair a b) where
    (==) (Pair f1 s1) (Pair f2 s2) = f1==f2
instance Ord a => Ord (Pair a b) where
    (<=) (Pair f1 s1) (Pair f2 s2) = f1<=f2

type Map a b = BinTree (Pair a b)

-- mapInsert :: (Ord a) => k -> v -> Map a b -> Map a b
-- mapInsert EmptyBT = 