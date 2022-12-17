import System.IO
import Data.Char

isIdentifier :: String -> Bool
isIdentifier [] = False
isIdentifier (x:xs) = isAsciiLower x &&
                all (\y-> isLetter y || isDigit y) xs

isVariable :: String -> Bool
isVariable [] = False
isVariable (x:xs) = isAsciiUpper x &&
                all (\y-> isLetter y || isDigit y) xs

isConstant :: String -> Bool
isConstant = isIdentifier

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy s l = first :
                splitBy s (if null remain then remain
                            else tail remain) 
    where
        first = firstValid l 0
        firstValid [] _ = []
        firstValid (x:xs) c
            | x==s && c==0 = []
            | x=='(' = x : firstValid xs (c+1)
            | x==')' = x : firstValid xs (c-1)
            | otherwise = x : firstValid xs c
        opening = length (filter (== '(') first)
        closing = length (filter (== ')') first)
        remain = drop (length first) l

isAtom :: String -> Bool
isAtom l = isValidPar && isIdentifier beforePar && all isTerm (splitBy ',' insidePar)
    where
       beforePar = takeWhile (/= '(') l
       parPart = dropWhile (/= '(') l
       isValidPar = (not . null) parPart && (head parPart == '(' && last parPart == ')')
       insidePar = init $ tail parPart

isTerm :: String -> Bool
isTerm [] = False
isTerm l@(x:xs) = isConstant l ||
                  isVariable l ||
                  isAtom l

isFact :: String -> Bool
isFact =isAtom

isRule :: String -> Bool
isRule l = hasSpecial &&
           isAtom beforeSpecial && 
           all isAtom 
           (splitBy ',' afterSpecial)
    where
        hasSpecial = " :- " `isSubstring` l
        isSubstring [] _ = True
        isSubstring _ [] = False
        isSubstring (x:xs) (y:ys) = (x==y && isSubstring xs ys) || isSubstring (x:xs) ys
        beforeSpecial = takeWhile (/= ' ') l
        afterSpecial = drop 4 (dropWhile (/= ' ') l)


main = do
        let list = []
        handle <- openFile "prolog/test.pl" ReadMode
        contents <- hGetContents handle
        hClose handle
        let singlewords = words contents
            list = f singlewords
        print list
        --todo find out what this does

f :: [String] -> [String]
f = map read