import Prelude hiding (cycle,iterate)

cycle [] = []
cycle l = l ++ cycle l

iterate f x = x : iterate f (f x)

pythagoreanTriples = [(y,x,z)| x<-[1..], y<-[1..x], z<-[x..(x+y)], x^2+y^2==z^2, gcd x y == 1]

sumLast k n = k : helper (fillZeroes ++ [k])
    where
        fillZeroes = take (n-1) [0,0..]
        helper l = s : helper (tail l ++ [s])
            where
                s= sum l

areIntersecting x y = or (zipWith (&&) (bits x) (bits y))
    where
        bits 0 = [False]
        bits 1 = [True]
        bits n = if odd n then True: bits ((n-1) `div` 2) else False : bits (n `div` 2)

sigert = 0 : map (\x -> nextTerm (take x sigert)) [1..]
    where 
        nextTerm l = if null work then length partitioned else minimum (map fst work)
            where
                partitioned= partition indexed
                n = length l
                indexed=zip [0..(length l - 1)] l
                partition [] = []
                partition l@(x:xs) = (snd x,filter (\y -> snd y == snd x) l) : partition (filter (\y -> snd y /= snd x) l )
                work =filter (noIntersectN . (map fst . snd)) partitioned
                noIntersectN :: [Int] -> Bool
                noIntersectN lst= not ( any (`areIntersecting` n) lst)

