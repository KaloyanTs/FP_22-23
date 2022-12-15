{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
forestFire = 0 : map (\x -> nextTerm (take x forestFire)) [1..]
    where
        nextTerm l = head $ dropWhile breaks [1..]
            where
                n = length l
                breaks x = not $ null [ k| k<-[1..(n `div` 2)], 2*l!!(n-k)== l!!(n-2*k) + x ]

type Purchase = (String,String, Double)

purchaseQuery l = res
    where
        partitionByCategory [] = []
        partitionByCategory l@(x:xs) = filter (\y -> getCategory y == getCategory x) l : partitionByCategory (filter (\y-> getCategory y /= getCategory x) xs)
        partitioned = partitionByCategory l
        getCategory :: Purchase -> String
        getCategory (_,c,_) = c
        getPrice :: Purchase -> Double
        getPrice (_,_,c) = c
        getShop :: Purchase -> String
        getShop (c,_,_) = c
        analyzeCat c = (getCategory $ head c,sum (map getPrice c))
        analyzed = map (\x-> (analyzeCat x,x)) partitioned

        partitionByShop [] = []
        partitionByShop l@(x:xs) = filter (\y -> getShop y == getShop x) l : partitionByCategory (filter (\y-> getCategory y /= getCategory x) xs)
        analyzeShop c = (getShop $ head c,sum (map getPrice c))
        shopsWithSum c = map analyzeShop $ partitionByShop c

        biggestSpShop l = fst $ foldr (\x y-> if snd x> snd y then x else y) (head l) l
        res = map (\x -> (fst $ fst x,snd $ fst x, biggestSpShop $ shopsWithSum $ snd x)) analyzed

generateExponents k l = removeSuccDupl $ 1 : map nextTerm [1..]
    where
        powerK = map (^k) [1..]
        powerL = map (^l) [1..]
        mixed  = [powerK!!(x-y)*powerL!!y|x<-[0..],y<-[0..x]]
        nextTerm x = needed !! x
            where
                needed = quicksort $ take ((x+1)*(x+2) `div` 2) mixed
        quicksort [] = []
        quicksort (x:xs) = quicksort (filter (< x) xs) ++
                             [x] ++
                           quicksort (filter (>= x) xs)
        removeSuccDupl l@(x:y:ys) = if x==y then removeSuccDupl (y:ys) else x:removeSuccDupl (y:ys)
        removeSuccDupl x = x

type Graph = [(Int, [Int])]

vertices :: Graph -> [Int]
vertices = map fst

children :: Int -> Graph -> [Int]
children u g = snd $ head $ filter (\p -> fst p == u) g

none :: (a->Bool) -> [a] -> Bool
none p= not . any p

parents :: Int ->Graph -> [Int]
parents u g = [v | v<- vertices g, u `elem` children v g]

isFamily :: [Int]->Graph -> Bool
isFamily f g= all (\u ->  (all (`elem` f) (children u g)
                            &&
                             none (`elem` f) (parents u g) )
                             ||
                              (all (`elem` f) (parents u g)
                              &&
                               none (`elem` f) (children u g)))  f

minIncluding :: Int -> Graph -> [Int]
minIncluding u g
    | isFamily attempt1 g= attempt1
    | isFamily attempt2 g= attempt2
    | otherwise = []
    where
        attempt1 = buildFamily u g False
        attempt2 = buildFamily u g True

buildFamily :: Int -> Graph -> Bool -> [Int]
buildFamily u g flag = helper [u] [u] flag
    where
        helper curr all flag
            |null next = all
            |otherwise = helper next (all ++ next) (not flag)
            where
                tmp = makeSet $ concatMap (\u -> (if flag then children else parents) u g) curr
                next = [v | v<- tmp, v `notElem` all]
                makeSet [] =[]
                makeSet (x:xs) = x : makeSet (filter (/= x) xs)


allEqual lNumbers fs = if null findImage then [] else map find calculated
    where
        calculated = zipWith construct lNumbers fs
        construct l f = map (\x -> (x,f x)) l
        h = head calculated
        hasTheValue value l =  any (\y-> snd y== snd value) l
        hasEqual value l= all (hasTheValue value) l
        run [] _ = []
        run from others = if hasEqual (head from) others then [snd $ head from] else run (tail from) others
        findImage = run (head calculated) (tail calculated)
        find [] = error "should not happen"
        find (x:xs) = if snd x == head findImage then fst x else find xs

type Ingredient = (String,Int)
type Cure = (String, [Ingredient])

isSubstitute a b = simplify ( sortIngredients (snd a) ) == simplify (sortIngredients (snd b))
    where
        sortIngredients [] = []
        sortIngredients (x:xs) = sortIngredients (filter (\y -> snd y < snd x) xs) ++ [x] ++ sortIngredients (filter (\y -> snd y >= snd x) xs)
        simplify ingList = map (\x -> (fst x, snd x `div` g)) ingList
            where
                g = foldr (gcd . snd) (snd (head ingList)) ingList

--bestSubstitutes :: Cure -> [Cure] -> Cure
bestSubstitute c cs = foldr (\x y -> if snd (head (snd x))>snd (head (snd y)) then x else y) (head cs) notStrongerSuitables
    where
        suitable = filter (isSubstitute c) cs
        notStrongerSuitables = filter (isStronger c) suitable
        isStronger :: Cure -> Cure -> Bool
        isStronger a b = snd (head ingA) > snd (head ingB)
            where
                ingA = snd a
                ingB = snd b

groupSubstitutes :: [Cure] -> [[Cure]]
groupSubstitutes [] = []
groupSubstitutes l@(x:xs) = filter (isSubstitute x) l: groupSubstitutes (filter (not.isSubstitute x) l)

stern = concatMap removeLast res
    where
        nextTerm [x] = [x]
        nextTerm (x:y:ys) = x : (x+y) : nextTerm (y:ys)
        nextTerm l = error "something went wrong..."
        removeLast [x] = []
        removeLast (x:xs) = x : removeLast xs
        removeLast [] = error "something went wrong..."
        res = [1,1] : map (\x->nextTerm (res!!x)) [0..]