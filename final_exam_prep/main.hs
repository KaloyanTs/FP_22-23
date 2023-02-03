-- generatePowers 2 3 → [1, 2, 3, 4, 6, 8, 9, ... ]

generatePowers k t = filter ok [1 ..]
  where
    ok x = (repeatedDiv t . repeatedDiv k) x == 1
      where
        repeatedDiv a x
          | x `mod` a == 0 = repeatedDiv a (x `div` a)
          | otherwise = x

data BinTree a
  = EmptyBT
  | Node a (BinTree a) (BinTree a)

countCodes :: BinTree Int -> Int
countCodes t = iter t 1
  where
    iter :: BinTree Int -> Int -> Int
    iter EmptyBT _ = 0
    iter (Node a left right) code = (if a == code then 1 else 0) + iter left (2 * code) + iter right (2 * code + 1)

racaman = 0 : map determine [1 ..]
  where
    determine n
      | (prev > n) && (prev - n) `notElem` past = prev - n
      | otherwise = prev + n
      where
        prev = last past
        past = take n racaman

checkId :: (Num a, Eq a) => [a -> a] -> [a] -> Bool
checkId fs xs = any (\(f, g) -> all (\x -> f (g (x)) == x) xs) [(f, g) | f <- fs, g <- fs]

type Team = String

type Goals = Int

type Result = (Team, Team, Goals, Goals)

allTeams :: [Result] -> [Team]
allTeams games = uniques $ map getHost games ++ map getGuest games
  where
    uniques [] = []
    uniques (x : xs) = x : filter (/= x) xs
    getHost (h, _, _, _) = h
    getGuest (_, g, _, _) = g

--  points | goals scored | goals conceded
teamScore :: [Result] -> Team -> (Int, Int, Int)
teamScore games t = (points, scored, conceded)
  where
    points = sum $ map getPoints games
    getPoints (h, g, hs, gs)
      | (h == t && hs > gs) || (g == t && gs > hs) = 3
      | (h == t || g == t) && gs == hs = 1
      | otherwise = 0
    scored = sum $ map getScored games
    getScored (h, g, hs, gs)
      | h == t = hs
      | g == t = gs
      | otherwise = 0
    conceded = sum $ map getConceded games
    getConceded (h, g, hs, gs)
      | h == t = gs
      | g == t = hs
      | otherwise = 0

-- team name |  points | goals scored | goals conceded
scoreBoard :: [Result] -> [(Team, Int, Int, Int)]
scoreBoard games = qsort less $ map (\team -> push team (teamScore games team)) $ allTeams games
  where
    push a (b, c, d) = (a, b, c, d)
    qsort _ [] = []
    qsort l (x : xs) = qsort l (filter (l x) xs) ++ [x] ++ qsort l (filter (not . l x) xs)
    less (t1, p1, sc1, c1) (t2, p2, sc2, c2)
      | p1 > p2 = True
      | p1 < p2 = False
      | (sc1 - c1) > (sc2 - c2) = True
      | (sc1 - c1) < (sc2 - c2) = False
      | sc1 > sc2 = True
      | sc1 < sc2 = False
      | otherwise = True

compress :: (Eq a) => [a] -> [(a, Int)]
compress [] = []
compress (x : xs) = (x, 1 + length (takeWhile (== x) xs)) : compress (dropWhile (== x) xs)

type Matrix = [[Int]]

sumMax :: Matrix -> Int
sumMax m = maximum $ map sum $ transpose m
  where
    transpose t
      | all null t = []
      | otherwise = map head t : transpose (map tail t)

findMiddle :: Ord a => [a] -> a
findMiddle l = if null res then head l else snd $ head res
  where
    res = filter fst $ map (\x -> (length (filter (< x) l) == length (filter (> x) l), x)) l

type Food = (String, Int)

-- discountFood :: [Food] -> String
discountFood l = fst $ head $ filter (\(n, d) -> d == res) l
  where
    res = minimum $ filter (> 0) $ map snd l

data Tree a
  = Empty
  | Tnode {val :: a, left :: Tree a, right :: Tree a}
  deriving (Show)

twins :: (Eq a) => Tree a -> [a]
twins Empty = []
twins (Tnode _ l@(Tnode a _ _) r@(Tnode b _ _))
  | a == b = a : (twins l ++ twins r)
  | otherwise = twins l ++ twins r
twins (Tnode _ Empty r) = twins r
twins (Tnode _ l Empty) = twins l

byLevels :: Tree a -> [[a]]
byLevels t = map (level t) [0 .. (height t)]

height :: Tree a -> Int
height Empty = -1
height t = 1 + max (height $ left t) (height $ right t)

level :: Tree a -> Int -> [a]
level Empty _ = []
level t 0 = [val t]
level t n = level (left t) (n - 1) ++ level (right t) (n - 1)

maxDistance l = maximum [sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2) | (x1, y1) <- l, (x2, y2) <- l]

selectionSort [] = []
selectionSort l = replicate (length (filter (== m) l)) m ++ selectionSort (filter (/= m) l)
  where
    m = minimum l

type Graph = [(Int, [Int])]

children g v = if null res then [] else res
  where
    res = snd $ head $ filter ((== v) . fst) g

isReachable :: Graph -> Int -> Int -> Bool
isReachable g v1 v2 = run g v1 v2 0
  where
    run g u v sum
      | u == v = True
      | otherwise = any (\w -> sum + w >= 0 && run g w v (sum + w)) (children g u)

matchedBy :: String -> [String]
matchedBy s = map (\t -> before ++ t ++ after) res
  where
    smbl = last $ takeWhile (/= '*') s
    before = init $ takeWhile (/= '*') s
    after = tail $ dropWhile (/= '*') s
    res = "" : map (smbl :) res

combine :: (t1 -> a) -> (a -> b -> t2) -> (t1 -> b) -> t1 -> t2
combine f h g x = h (f x) (g x)

equalFunctions :: (Foldable t1, Eq a) => t1 t2 -> (t2 -> a) -> (t2 -> a) -> Bool
equalFunctions args f g = all (\arg -> f arg == g arg) args

check :: (Eq b, Enum t2) => t2 -> t2 -> [t2 -> b] -> [b -> b -> b] -> Bool
check a b uns bins = any (\f -> any (equalFunctions [a .. b] f) uns) [combine f h g | f <- uns, g <- uns, h <- bins]

type Plant = (String, Int, Int)

name :: Plant -> String
name (n, _, _) = n

minT :: Plant -> Int
minT (_, m, _) = m

maxT (_, _, m) = m

garden :: [Plant] -> ((Int, Int), [String])
garden plants = head $ qsort less $ [((mi, ma), map name $ filter (\p -> appr p (mi, ma)) plants) | mi <- [overallMin .. overallMax], ma <- [mi .. overallMax]]
  where
    appr p (mi, ma) = minT p <= mi && maxT p >= ma
    overallMin = minimum $ map minT plants
    overallMax = maximum $ map maxT plants
    less x y
      | length (snd x) > length (snd y) = True
      | length (snd x) < length (snd y) = False
      | otherwise = snd (fst x) - fst (fst x) > snd (fst y) - fst (fst y)

qsort :: (a -> a -> Bool) -> [a] -> [a]
qsort l [] = []
qsort l (x : xs) =
  qsort l (filter (`l` x) xs)
    ++ [x]
    ++ qsort
      l
      (filter (not . (`l` x)) xs)

maxPath :: Graph -> Int -> [Int]
maxPath g u = iter u [u]
  where
    iter :: Int -> [Int] -> [Int]
    iter v path = longest $ map (\u -> iter u (path ++ [u])) $ filter (`notElem` path) $ children g v
      where
        longest [] = path
        longest l = foldr1 (\x y -> if length x > length y then x else y) l

largestInterval f g a b = foldr1 (\x y -> if snd x - fst x > snd y - fst y then x else y) [(x, y) | x <- [a .. b], y <- [x .. b], all (\x -> f x == g x) [x .. y]]

intervalTree :: (Ord a) => Tree a -> Tree (a, a)
intervalTree Empty = Empty
intervalTree t@(Tnode r _ _) = e1 $ iter t r
  where
    iter :: (Ord a) => Tree a -> a -> (Tree (a, a), a, a)
    iter Empty prev = (Empty, prev, prev)
    iter (Tnode el l r) prev = (Tnode (mi, ma) (e1 resl) (e1 resr), mi, ma)
      where
        resl = iter l el
        resr = iter r el
        mi = minimum [el, e2 resl, e2 resr]
        ma = maximum [el, e3 resl, e3 resr]
    e1 (x, _, _) = x
    e2 (_, x, _) = x
    e3 (_, _, x) = x

sumOfSquares = filter (\x -> (not . null) [(a, b) | a <- [1 .. x], b <- [1 .. a], a ^ 2 + b ^ 2 == x]) [1 ..]

type Video = (String, Int)

videoName :: Video -> String
videoName (n, _) = n

videoLength :: Video -> Int
videoLength (_, l) = l

-- averageVideo :: [Video] -> String
averageVideo l = videoName $ foldr1 (\v1 v2 -> if videoLength v1 > videoLength v2 then v1 else v2) $ filter ((<= avg) . videoLength) l
  where
    lengths = map videoLength l
    avg = sum lengths `div` length lengths