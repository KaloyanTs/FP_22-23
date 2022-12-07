{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-
>>> d
Variable not in scope: d

-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- no incomplete patterns in lambdas!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

import Prelude hiding (abs, filter, foldl, map, reverse, takeWhile, zip, zipWith)

map2 :: (t -> a) -> [t] -> [a]
map2 _ [] = []
map2 f (x : xs) = f x : map f xs

abs x
  | x >= 0 = x
  | otherwise = - x

filter2 _ [] = []
filter2 p (x : xs)
  | p x = x : filter2 p xs
  | otherwise = filter2 p xs

foldl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl _ nv [] = nv
foldl op nv (x : xs) = foldl op (op nv x) xs

reverse [] = []
reverse l = foldl (flip (:)) [] l

compose f g x = f (g x)

prefix l1 l2 = take (length l1) l2 == l1

--prefix [] _ = True
--prefix _ [] = False
--prefix (x : xs) (y : ys) = (x == y) && prefix xs ys

suffix l1 l2 = drop (length l2 - length l1) l2 == l1

--suffix l1 l2 = prefix (reverse l1) (reverse l2)

filter p = foldr (\x y -> if p x then x : y else y) []

map f = foldr (\x y -> f x : y) []

uninterleave :: (Foldable t, Integral a) => t a -> ([a], [a])
uninterleave l = (filter odd l, filter even l)

zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith op (x : xs) (y : ys) = op x y : zipWith op xs ys

takeWhile _ [] = []
takeWhile p (x : xs) = if p x then x : takeWhile p xs else []

nub [] = []
nub (x : xs) = x : filter (x /=) (nub xs)

prime x
  | x < 2 = False
  | otherwise = not (null (filter (\y -> x `mod` y == 0) [1 .. (sqrt x)]))

quicksort [] = []
quicksort [x] = [x]
quicksort (x : xs) = quicksort (filter (< x) xs) ++ [x] ++ quicksort (filter (> x) xs)

factorize 1 = [1]
factorize x
  | x < 1 = error "cannot be factorized"
  | otherwise = helper 2 x
  where
    helper _ 1 = []
    helper from y
      | y `mod` from == 0 = from : helper from (y `div` from)
      | otherwise = helper (from + 1) y
