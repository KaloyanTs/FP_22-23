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
allTeams games = undefined
                    --  points | goals scored | goals conceded
teamScore :: [Result] -> Team -> (Int, Int, Int)
teamScore games t = undefined
        -- team name |  points | goals scored | goals conceded
scoreBoard :: [Result] -> [(Team, Int, Int, Int)]
scoreBoard games = undefined