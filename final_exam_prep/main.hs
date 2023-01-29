-- generatePowers 2 3 → [1, 2, 3, 4, 6, 8, 9, ... ]

generatePowers k t = filter ok [1..]
    where
        ok x = (repeatedDiv t .repeatedDiv k) x == 1
            where
                repeatedDiv a x
                    | x `mod` a == 0 = repeatedDiv a (x `div` a)
                    | otherwise = x

data BinTree a
    = EmptyBT
    | Node a (BinTree a) (BinTree a)

countCodes::BinTree Int -> Int
countCodes t = iter t 1
    where
        iter :: BinTree Int -> Int -> Int
        iter EmptyBT _ = 0
        iter (Node a left right) code = (if a == code then 1 else 0) + iter left (2*code) + iter right (2*code +1)

racaman = 0 : map determine [1..]
    where
        determine n
            | (prev > n ) && (prev-n) `notElem` past = prev-n 
            | otherwise =  prev+n
                where
                    prev = last past
                    past = take n racaman

checkId :: (Num a, Eq a) => [a -> a] -> [a] -> Bool
checkId fs xs = any (\(f,g)->all (\x->f(g(x))==x) xs) [(f,g)|f<-fs,g<-fs]

type Team = String
type Goals = Int
type Result = (Team, Team, Goals, Goals)

allTeams :: [Result] -> [Team]
allTeams games = uniques $ map getHost games ++ map getGuest games
    where
        uniques [] = []
        uniques (x:xs) = x : filter (/= x) xs
        getHost (h,_,_,_) = h
        getGuest (_,g,_,_) = g
                    --  points | goals scored | goals conceded
teamScore :: [Result] -> Team -> (Int, Int, Int)
teamScore games t = (points, scored, conceded)
    where
        points = sum $ map getPoints games
        getPoints (h,g,hs,gs)
            | (h == t && hs>gs) || (g==t && gs>hs) = 3 
            | (h == t || g==t) && gs==hs = 1 
            | otherwise = 0
        scored = sum $ map getScored games
        getScored (h,g,hs,gs)
            | h == t = hs 
            | g == t = gs 
            | otherwise = 0
        conceded = sum $ map getConceded games
        getConceded (h,g,hs,gs)
            | h == t = gs 
            | g == t = hs 
            | otherwise = 0
        -- team name |  points | goals scored | goals conceded
scoreBoard :: [Result] -> [(Team, Int, Int, Int)]
scoreBoard games = qsort less $ map (\team -> push team (teamScore games team)) $ allTeams games
    where
        push a (b,c,d) = (a,b,c,d)
        qsort _ [] = []
        qsort l (x:xs) = qsort l (filter (l x) xs) ++ [x] ++ qsort l (filter (not.l x) xs)
        less (t1,p1,sc1,c1) (t2,p2,sc2,c2)
            | p1 > p2 = True
            | p1 < p2 = False
            | (sc1 - c1) > (sc2 - c2) = True
            | (sc1 - c1) < (sc2 - c2) = False
            | sc1 > sc2 = True
            | sc1 < sc2 = False
            | otherwise = True