import Foreign.C (eROFS)
import System.Win32 (COORD (x))
import Prelude hiding (abs, all, length, maximum, minimum, replicate, reverse, zip, zipWith)

minimum [] = error "no elements"
minimum l@(x : xs) = foldr (\x y -> min x y) x l

maximum [] = error "no elements"
maximum [x] = x
maximum l@(x : xs) = foldr (\x y -> max x y) x l

length [] = 0
length l@(x : xs) = foldr (\x y -> 1 + y) 0 l

all _ [] = True
all p l@(x : xs) = foldr (&&) True (map p l)

any _ [] = False
any p l@(x : xs) = foldr (||) False (map p l)

append l1 l2 = foldr (:) l2 l1

replicate :: [b] -> [b]
replicate = foldr (\x y -> x : x : y) []

countDivisors n = length [x | x <- [1 .. (n -1)], n `mod` x == 0]

isPrime n = n > 1 && countDivisors n == 1

descartes l1 l2 = [(x, y) | x <- l1, y <- l2]

naturals = [0 ..]

primes = filter isPrime [0 ..]

sieve [] = []
sieve (x : xs) = x : filter (\y -> y `mod` x /= 0) (sieve xs)

eratosten = sieve [2 ..]

nxn = [(power2 n, oddPart n) | n <- [1 ..]]
  where
    power2 n
      | odd n = 0
      | otherwise = 1 + power2 (n `div` 2)
    oddPart n
      | odd n = (n - 1) `div` 2
      | otherwise = oddPart (n `div` 2)

nxn2 = [(i, n - i) | n <- [0 ..], i <- [0 .. n]]

compress [] = []
compress (x : xs) = (x, count x) : compress (dropWhile (== x) xs)
  where
    count x = 1 + length (takeWhile (== x) xs)

maxRepeated [] = 0
maxRepeated l@(x : xs) = max (length (takeWhile (== x) l)) (maxRepeated (dropWhile (== x) xs))

makeSet [] = []
makeSet (x : xs)
  | left == xs = x : makeSet xs
  | otherwise = makeSet xs
  where
    left = filter (/= x) xs

histogram [] = []
histogram (x : xs) = (x, count x) : histogram (filter (/= x) xs)
  where
    count x = 1 + length (filter (== x) xs)

maxDistance l = maximum (map distance (descartes l l))
  where
    distance ((x1, y1), (x2, y2)) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

quicksort _ [] = []
quicksort _ [x] = [x]
quicksort less l@(x : xs) = quicksort less (filter (`less` x) xs) ++ [x] ++ quicksort less (filter (\y -> not (less y x)) xs)

specialSort :: (Ord a) => [[a]] -> [[a]]
specialSort = quicksort less
  where
    less l1 l2 = mostCommon l1 < mostCommon l2

mostCommon [] = error "no elements"
mostCommon l@(x : xs) = fst (foldl cmp (count x) (map count xs))
  where
    count n = (n, length (filter (== n) l))
    cmp (x1, x2) (y1, y2)
      | x2 < y2 = (y1, y2)
      | x2 == y2 = if x1 > y1 then (x1, x2) else (y1, y2)
      | otherwise = (x1, x2)