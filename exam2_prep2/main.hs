subsets [] = [[]]
subsets (x:xs) = merge without ( map (x:) without)
    where
        without = subsets xs
        merge [] l = l
        merge l [] = l
        merge l1@(x:xs) l2@(y:ys)
            | length x < length y = x : merge xs l2
            | otherwise = y : merge l1 ys

permutations [] = [[]]
permutations l = concatMap (\x-> map (x :) (permutations (filter (/= x) l))) l

nextPerm l = if length (incPrefixList (reverse l)) == length l then [] else  left++[rightPivot]++reverse (middle++[pivot]++rightWithoutPivot)
    where
        incPrefixList [] = []
        incPrefixList [x] = [x]
        incPrefixList (x:y:xs) = x: if y>=x then incPrefixList (y:xs) else []
        incPrefix = incPrefixList l
        decSuffix = reverse (incPrefixList (reverse l))
        last [] = error "empty list has no last element"
        last [x] = x
        last (x:xs) = last xs
        removeLast [] = error "empty list has no last element"
        removeLast [x] = []
        removeLast (x:xs) = x: removeLast xs
        leftWithPivot = take (length l - length decSuffix) l
        pivot = last leftWithPivot
        left = removeLast leftWithPivot
        firstGreater _ [] = error "bad"
        firstGreater el (x:xs) = if x > el then [x] else x:firstGreater el xs
        rightWithPivot = reverse (firstGreater pivot (reverse decSuffix) )
        rightPivot = head rightWithPivot
        rightWithoutPivot = tail rightWithPivot
        middle = take (length decSuffix - length rightWithPivot) decSuffix

repeatCompose 0 _ = id
repeatCompose n f = f . repeatCompose (n - 1) f

iteratingPerms n = helper [1..n]
    where
        helper l = if null next then [l] else l:helper next
            where
                next = nextPerm l

longestIncr [] = []
longestIncr l@(x:xs) = helper l x
    where
        helper [] _ = []
        helper [x] _ = [x]
        helper (x:y:xs) minn
            | x<minn = helper xs minn
            | otherwise = (\x1 y1 -> if length x1 > length y1 then x1 else y1) (x:helper (y:xs) x) (helper (y:xs) y)

ones=1:ones
nats=0:map (+ 1) nats

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

triangles = 0: zipWith (+) (tail nats) triangles

incPrefixList [] = []
incPrefixList [x] = [x]
incPrefixList (x:y:xs) = x: if y>=x then incPrefixList (y:xs) else []