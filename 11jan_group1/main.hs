-- task 1 and 2
safeHead :: [a] -> Maybe a
safeHead list = if null list then Nothing else Just (head list)

safeTail :: [a] -> Maybe [a]
safeTail list = if length list < 2 then Nothing else Just (tail list)

safeUncons :: [a] -> Maybe(a,[a])
safeUncons list@(x : xs)
             | null list || length list ==1  = Nothing
             | otherwise  = Just(x,xs)

stripPrefix :: Eq a =>[a] -> [a] -> Maybe [a]
stripPrefix [] l2 = Just l2
stripPrefix l1 [] = Nothing
stripPrefix (x : xs) (y : ys) = if x == y then stripPrefix xs ys else Nothing

findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex _ [] = Nothing
findIndex y (x : xs)
            | x == y = Just 0
            |otherwise = case findIndex y xs of Nothing -> Nothing
                                                Just i  -> Just (i + 1)


maybeToList :: [a] -> Maybe [a]
maybeToList list = if null list then Nothing else Just list

--task 3
data BinaryTree a = EmptyTree
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving Show

maxSumPath :: Num a => Ord a=> BinaryTree a -> a
maxSumPath EmptyTree = 0
maxSumPath (Node value left right)  = value + max (maxSumPath left)  (maxSumPath right)

-- task 4
prune :: BinaryTree a-> BinaryTree a
prune EmptyTree = EmptyTree
prune (Node _ EmptyTree EmptyTree) = EmptyTree
prune (Node value left right) = Node value (prune left) (prune right)

-- task 5
bloom :: BinaryTree a-> BinaryTree a
bloom EmptyTree = EmptyTree
bloom (Node value EmptyTree EmptyTree) = Node value (Node value EmptyTree EmptyTree) (Node value EmptyTree EmptyTree)
bloom (Node value left right) = Node value (bloom left) (bloom right)

-- task 6
rotateLeft :: BinaryTree a-> BinaryTree a
rotateLeft (Node val left@(Node v l r) right@(Node v1 l1 r1)) = Node v1 (Node val left l1) r1
rotateLeft t = t

rotateRight :: BinaryTree a ->BinaryTree a
rotateRight (Node val left@(Node v l r) right@(Node v1 l1 r1)) = Node v l (Node val r right)
rotateRight t = t

-- task 7
instance Functor BinaryTree where
     fmap _ EmptyTree = EmptyTree
     fmap func (Node val left right) = Node (func val) (fmap func left) (fmap func right)


-- task 9
data BST a = BSTEmpty
           | BSTNode a (BST a) (BST a)

insert ::Ord a => a -> BST a -> BST a
insert x BSTEmpty = BSTNode x BSTEmpty BSTEmpty
insert x (BSTNode val left right)
   | x < val = BSTNode val (insert x left) right
   | otherwise = BSTNode val left (insert x right)

search :: Ord a => a -> BST a -> Bool
search x BSTEmpty = False
search x (BSTNode val left right)
       | x == val = True
       | x < val = search x left
       | otherwise = search x right

values :: BST a -> [a]
values BSTEmpty = []
values (BSTNode val left right) = values left ++ [val] ++ values right

size :: BST a -> Int
size tree = length (values tree)
toList :: BST a -> [a]
toList BSTEmpty = []
toList (BSTNode val left right) = toList left ++ [val] ++ toList right

fromList :: Ord a => [a] ->BST a
fromList = foldr insert BSTEmpty

sort :: Ord a => [a] -> [a]
sort = toList . fromList

-- task 10
data Map k v= EmptyMap
         | NodeMap k v (Map k v) (Map k v)

mapInsert :: Ord k => k -> v -> Map k v -> Map k v
mapInsert key value EmptyMap = NodeMap key value EmptyMap EmptyMap
mapInsert key value (NodeMap currKey currValue left right)
       | key == currKey = NodeMap key value left right
       | key > currKey  = NodeMap currKey currValue left (mapInsert key value right)
       | otherwise      = NodeMap currKey currValue (mapInsert key value left) right

mapSearch :: Ord k => k -> Map k v -> Maybe v
mapSearch key EmptyMap = Nothing
mapSearch key (NodeMap currKey currValue left right)
     | key == currKey  = Just currValue
     | key > currKey   = mapSearch key right
     | otherwise       = mapSearch key left

fromPairs :: Ord k => [(k,v)] -> Map k v
fromPairs = foldr (\(k,v) res -> mapInsert k v res) EmptyMap
-- task 11
instance Functor (Map k) where
     fmap _ EmptyMap = EmptyMap
     fmap func (NodeMap key val left right) = NodeMap key (func val) (fmap func left) (fmap func right)
--task 12



testTree = Node 10 (Node 3 EmptyTree EmptyTree) (Node 14 (Node 11 EmptyTree EmptyTree) (Node 15 EmptyTree EmptyTree))

binaryTestTree = BSTNode 10 (BSTNode 3 BSTEmpty BSTEmpty) (BSTNode 14 (BSTNode 11 BSTEmpty BSTEmpty) (BSTNode 15 BSTEmpty BSTEmpty))

--facts = 1 : zipWith (*) facts [2..]

data NonEmptyTree a = NNode a [NonEmptyTree a]

data NTree a = EmptyNTree

data Direction = L | R


mapPath :: Ord a => a -> Map a b -> Maybe [Direction]
mapPath _ EmptyMap = Nothing
mapPath to (NodeMap key _ left right)
    | key == to = Just []
    | key < to = (:) L <$> mapPath key left
    | otherwise = (:) R <$> mapPath key right