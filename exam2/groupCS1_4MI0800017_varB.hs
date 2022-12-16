type Match = (String,String,Int,Int)

maxPointsMinGoal :: [Match] -> String
maxPointsMinGoal l =getName $ head sorted
    where
        getHost (h,_,_,_)=h
        getGuest (_,g,_,_)=g
        partition [] = []
        teams = uniques $ concatMap (\x-> [getHost x,getGuest x]) l
        uniques [] = []
        uniques (x:xs) = x: uniques (filter (/= x) xs)
        partitionByTeams = map (\x-> (x,filter (\y-> (getHost y == x) || (getGuest y==x)) l))  teams
        findPoints team (host,guest,hG,gG)
            |  (host==team )&& hG>gG = 3
            |  (guest==team )&& hG<gG = 3
            |  hG==gG = 1
            | otherwise = 0
        calcPoints (team,matches) = sum (map (\x->findPoints team x) matches)
        findDiff team (host, guest,hG,gG)
            | host==team = hG-gG
            | team == guest = gG - hG
            | otherwise = error "something went wrong..."
        calcDiff (team,matches) = sum (map (\x->findDiff team x) matches)
        getName (n,_,_) = n
        getPoints (_,p,_) = p
        getDiff (_,_,d) = d
        quicksort [] = []
        quicksort (x:xs) = quicksort (filter (\y-> getPoints y >getPoints x || 
                                                    ((getPoints y == getPoints x) && getDiff y < getDiff x ))
                                         xs) ++ [x] ++ quicksort 
                                                    (filter (\y-> getPoints y <getPoints x || 
                                                    ((getPoints y == getPoints x) && getDiff y > getDiff x ))
                                                        xs)
        sorted =quicksort $ map (\x-> (fst x,calcPoints x,calcDiff x)) partitionByTeams

exceedSelf :: [Match] -> [String]
exceedSelf l = filter (\x -> x `exceeds` x) teams
    where
        getHost (h,_,_,_)=h
        getGuest (_,g,_,_)=g
        partition [] = []
        teams = uniques $ concatMap (\x-> [getHost x,getGuest x]) l
        uniques [] = []
        uniques (x:xs) = x: uniques (filter (/= x) xs)

        exceeds x y = any (\m-> win x y m) l ||
                      any (\z-> (x `dominates` z) && z `exceeds` y) 
                          (filter (\z -> z/=x && z/=y) teams)
        win x y (host,guest,hG,gG)
            | host==x && guest == y && hG>gG = True
            | guest==x && host == y && gG>hG = True
            | otherwise = False
        dominates x y  = all (\m-> win x y m || notPlayingTogether x y m) l
        notPlayingTogether x y (host,guest,hG,gG) = not ((host==x && guest == y) || (host==x && guest == y))

combStreams [(x:xs),(y:ys),(z:zs)] = helper [xs,ys,zs] [x,y,z]
    where
        helper s@[(x:xs),(y:ys),(z:zs)] prev@[lastA,lastB,lastC] = [lastA:(step!!0),lastB:(step!!1), lastC:(step!!2)] 
            where
                next = nextTriple prev [x,y,z]
                step = helper [xs,ys,zs] next
        nextTriple prev@[a1n,b1n,c1n] inStream@[an,bn,cn] = res
            where
                all = uniques $ allPermutations inStream
                allPermutations [a,b,c] = uniques [[a,b,c],[a,c,b],[b,c,a],[b,a,c],[c,a,b],[c,b,a]]
                allPermutations p = error "bad input"
                uniques [] = []
                uniques (x:xs) = x: uniques (filter (/= x) xs)
                res = foldr (\x y -> if f x < f y then x else y) (head all) (tail all)
                    where
                        f [x1,x2,x3] = abs(x1 - a1n) + abs(x2 - b1n) + abs(x3 - c1n)
                        f p = error "shouln't happen"
combStreams p = error "expected 3 streams"