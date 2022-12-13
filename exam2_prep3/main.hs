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