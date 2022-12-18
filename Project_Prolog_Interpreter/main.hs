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
                            else dropWhile (==' ') (tail remain))
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
isFact l = (not.null) l &&
           last l == '.' &&
           (isAtom.init) l

isRule :: String -> Bool
isRule l = (not.null) l &&
           last l == '.' &&
           hasSpecial &&
           isAtom beforeSpecial &&
           all isAtom
           (splitBy ',' afterSpecial)
    where
        noDot = init l
        hasSpecial = " :- " `isSubstring` noDot
        isSubstring [] _ = True
        isSubstring _ [] = False
        isSubstring (x:xs) (y:ys) = (x==y && isSubstring xs ys) || isSubstring (x:xs) ys
        beforeSpecial = takeWhile (/= ' ') noDot
        afterSpecial = drop 4 (dropWhile (/= ' ') noDot)

removeEmpty :: [String] -> [String]
removeEmpty = filter (not.null)

isComment :: String -> Bool
isComment l = (not.null) l && head (dropWhile (==' ') l) == '%'

extractData :: String -> [String]
extractData l = removeEmpty (splitBy '\n' l)

consult :: String -> (Bool , [String])
consult contents = (truth,if truth then [] else filter (\x-> not (isFact x || isRule x || isComment x)) (extractData contents))
    where
        truth = all (\x-> isFact x || isRule x || isComment x) (extractData contents)

--todo should whitespaces be allowed after ','
main = do
        putStr "Which file to consult from the directory \"prolog/\"?\n> "
        file <- getLine
        contents <- readFile ("prolog/" ++ file)
        print $ consult contents
        --todo use where ?
        -- print $ if fst truth then fst truth else fst truth ++ '\n' ++ snd truth
        --     where
        --         truth = consult contents
        main