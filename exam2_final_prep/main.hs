mostFrequent l =if null res then 0 else fst $ head res
    where
        histogram [] = []
        histogram (x:xs) = (x,length (filter (== x) (x:xs))):histogram (filter (/= x) xs)
        compressed = map histogram l
        collectMostFreq l = foldr (\x y -> if snd x == snd (head y)
                                    then x:y else
                                        if snd x > snd (head y)
                                    then [x] else y)
                                    [head l] (tail l)
        mostFreqList = map collectMostFreq compressed
        toSearchFrom = head mostFreqList
        toSearchTo = tail mostFreqList
        res = filter (\y-> all (any (\p -> fst p == fst y)) toSearchTo) toSearchFrom

type TVShow = (String, (Int,Int), Int)

lastShow :: [TVShow]->String
lastShow [] = error "No shows"
lastShow l = fst $ foldr ((\x y-> if after x y then x else y) . findEnd) ("Something went wrong",(0,0)) l
    where
        after (n1,(h1,m1)) (n2,(h2,m2)) = h1>h2 || (h1==h2 && m1>m2)
        findEnd (n,(a,b),d) = if snd sum > 60 then (n,(fst sum +1,snd sum - 60)) else (n,sum)
            where
                sum = (a + (d `div` 60),b + (b `mod` 60))

--longestProgram :: [TVShow] -> [TVShow]
longestProgram [] = []
longestProgram l =map removeEnd $ foldr (\x y -> if sumDuration x > sumDuration y then x else y) [] (filter isProgram (map (map findEnd) ( concatMap allStrings [1..(length l)])))
    where
        allStrings 0 = [[]]
        allStrings 1 = [[x]|x<-l]
        allStrings n = concatMap (\x -> map (: x) (filter (`notElem` x) l)) $ allStrings (n-1)
        findEnd (n,(a,b),d) = if snd sum >= 60 then (n,(a,b),(fst sum +1,snd sum - 60),d) else (n,(a,b),sum,d)
            where
                sum = (a + (d `div` 60),b + (d `mod` 60))
        getStart (_,s,_,_) = s
        getEnd (_,_,e,_) = e
        getDuration (_,_,_,d) = d
        after (h1,m1) (h2,m2) = h1>h2 || (h1==h2 && m1>=m2)
        isProgram (x:y:ys) = (getStart y `after` getEnd x) && isProgram (y:ys)
        isProgram l = True
        sumDuration [] = 0
        sumDuration l = sum $ map getDuration l
        removeEnd (a,b,c,d) = (a,b,d)