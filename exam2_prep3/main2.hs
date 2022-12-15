import GHC.Float
type Graph = [(Int, [Int])]

vertices :: Graph -> [Int]
vertices = map fst

children :: Int -> Graph -> [Int]
children u g = snd $ head $ filter (\x -> fst x == u) g

parents :: Int -> Graph -> [Int]
parents u g = filter (\x-> u `elem` children x g) (vertices g)

edge :: Int -> Int -> Graph -> Bool
edge u v g = v `elem` children u g

allPathsWithLimit :: Graph -> Int -> Int -> Int -> [[Int]]
allPathsWithLimit g u v k = concatMap res [1..]--concatMap allPaths [1..]
    where
        allStrings 0 = []
        allStrings 1 = map (: []) vert
        allStrings n = concatMap (\x -> map (: x) vert) $ allStrings (n-1)
        vert = vertices g
        isPath (x:y:ys) = edge x y g && isPath (y:ys)
        isPath p= True
        notPassesVmoreThanK p= length (filter (== v) p) <= k
        startsFromU p = head p == u
        res n =filter startsFromU $
                    filter notPassesVmoreThanK $
                            filter isPath $ allStrings n

type Row = [Int]
type Matrix = [Row]

leastColors :: Matrix -> (Int->Int) -> (Int->Int)->Int
leastColors m f g =minimum $ map colorsInColoring allColorings
    where
        transpose x
            | all null x = []
            | otherwise = map head x : transpose (map tail x)
        trM = transpose m
        allStrings 0 = []
        allStrings 1 = [[True],[False]]
        allStrings n = concatMap (\x -> map (: x) [True,False]) $ allStrings (n-1)
        color coloring = zipWith (\x y -> if x then map f y else map g y)
                            coloring trM
        allColorings = map color (allStrings (length trM))
        colorsInColoring c = length $ uniques $ concat c
        uniques [] = []
        uniques (x:xs) = x: uniques (filter (/= x) xs)

type Glass = (Int,Int)

maxSameVolume :: [Glass] -> [Glass]
maxSameVolume [] = []
maxSameVolume l@(x:xs) = res
    where
        volume :: Glass -> Int
        volume (r,h) = r^2*h
        partition [] = []
        partition [x] = [[x]]
        partition (x:xs) = filter (\y -> volume y == volume x) l
                                : partition (filter (\y -> volume y /= volume x) xs)
        partitioned = partition l
        res = foldr (\x y -> if length x>length y then x else y) (head partitioned) (tail partitioned)

bestPair :: [Glass] -> Float -> (Glass,Glass)
bestPair l quantity
    |length res1 <2 = (head l,head (tail l))
    |otherwise = (head res1, head (tail res1))
    where
        pour = quantity / 2
        pi = 4*atan 1
        modify = map (\(r,h)->((r,h),int2Float h - pour / ((int2Float r ^2)*pi))) l
        notExceeding :: [(Glass,Float)]
        notExceeding =  filter (\((r,h),h1) -> h1>1) modify
        quicksort [] = []
        quicksort (((r,h),h1):xs) = quicksort (filter (\((a,b),c)->
                                                        if c==h1 then b<h else c<h1)
                                                            xs)
                                                            ++ [((r,h),h1)]
                                                            ++ quicksort (filter (\((a,b),c)->
                                                                if c==h1 then b>=h else c>=h1)
                                                                 xs)
        res = quicksort notExceeding
        res1 = map fst res
