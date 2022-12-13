{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use second" #-}

import GHC.Float ( int2Double ) -- todo avoid in future

isNPerm n f = inRange values && uniques values
  where
    values = map f [0 .. (n -1)]
    inRange l = not (any (\x -> x < 0 || x >= n) l)
    onlyOne el l = length (filter (== el) l) == 1
    uniques l = all (`onlyOne` l) l

maxCycle n f
  | not (isNPerm n f) = []
  | otherwise = foldr ((\x y -> if length x >= length y then x else y) . startFrom) [] [0 .. (n - 1)]
  where
    startFrom i = reverse (iter i [i])
      where
        iter _ [] = error "unacceptable error"
        iter index l@(x : xs)
          | f index == i = l
          | otherwise = iter (f index) (f index : l)

movingAverage [] _ = error "working only with infinite streams"
movingAverage l@(x : xs) n = firstEl : movingAverage xs n
  where
    firstEl = (/) (sum (take n l)) (int2Double n)

allAverages l = begin 2 l
  where
    begin i l = movingAverage l i : begin (i + 1) l

type Box = (String, [String])

allObjects :: [Box] -> [String]
allObjects inv = filter noLabel inBox
  where
    labels = map fst inv
    inBox = concatMap snd inv
    noLabel x = x `notElem` labels

cleanUp :: [Box] -> [Box]
cleanUp inv = removeBoxInBox (removeEmpty (leaveEmpty inv))
  where
    leaveEmpty inv = solve (length inv) inv
      where
        solve i inv
          | i == 0 = inv
          | otherwise = work (solve (i - 1) inv)
          where
            work :: [Box] -> [Box]
            work inv = map check inv
              where
                allEmpty = map fst (filter empty inv)
                check box = (fst box, filter (`notElem` allEmpty) (snd box))
        removeEmpty l = filter (not . empty) l
        empty x = null (snd x)
    removeEmpty = filter notEmpty
      where
        notEmpty box = not (null (snd box))
    removeBoxInBox inv = filter (\x -> fst x `notElem` toBeRemovedNames) (map work inv)
      where
        boxesNames = map fst inv
        hasOnlyBox box = length (snd box) == 1 && elem (head (snd box)) boxesNames
        toBeModified = filter hasOnlyBox inv
        toBeRemovedNames = map (head . snd) toBeModified
        toBeRemoved = filter (\x -> fst x `elem` toBeRemovedNames) inv
        work box
          | hasOnlyBox box = (fst box, snd (head (filter (\x -> fst x == head (snd box)) inv)))
          | otherwise = box
