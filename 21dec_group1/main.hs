-- data Student = Student String [Int]

-- findMaxGrade :: Student -> Int
-- findMaxGrade (Student _ grades) = maximum grades

data Student a = Student String [a]

findMaxGrade :: (Ord a) => Student a -> a
findMaxGrade (Student _ grades) = maximum grades

data Player = Monster String Int Int
            | Wizard Int Float
            | Princess Int [String]
    deriving Show

f:: Player->Int
f (Monster name strength height) = height + 1
f (Wizard h beard) = h + 2
f (Princess height lovers) = height + 3

data List a = Empty
            | Cons a (List a)

data Tree a = EmptyTree
           | Node a (Tree a) (Tree a)

height:: Tree a -> Int
height EmptyTree = 0
height (Node _ l r) = 1 + max (height l) (height r)

mapTree :: Tree (Integer -> Integer) -> Integer -> [Integer]
mapTree EmptyTree _ = []
mapTree (Node f EmptyTree EmptyTree) x = [f x]
mapTree (Node f l r) x = mapTree l (f x) ++ mapTree r (f x)

testTree = Node (+1)
                (Node (^2)
                      (Node (*2) EmptyTree EmptyTree)
                      (Node (subtract 3) EmptyTree EmptyTree)
                )
                (Node (3^) EmptyTree EmptyTree)

data BST a = BSTEmpty
            | BSTNode a (BST a) (BST a)

insert :: Ord a => a->BST a->BST a
insert x BSTEmpty = BSTNode x BSTEmpty BSTEmpty
insert x (BSTNode val l r)
    | x<val = BSTNode val (insert x l) r
    | otherwise = BSTNode val l (insert x r)

toList :: BST a -> [a]
toList BSTEmpty = []
toList (BSTNode val l r) = toList l ++ [val] ++ toList r

fromList :: Ord a => [a] -> BST a
fromList = foldr insert BSTEmpty

sort :: Ord a => [a] -> [a]
sort = toList . fromList