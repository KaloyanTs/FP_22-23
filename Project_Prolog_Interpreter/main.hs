{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isLetter)

data LetterNumberSequence
  = EmptyLNS
  | Cons Char LetterNumberSequence

data Identifier = MakeId Char LetterNumberSequence

data Variable = MakeVar Char LetterNumberSequence

data Constant = MakeC Identifier

data TermSequence
  = EmptyTS
  | MakeTS Term TermSequence

data Term
  = MakeTermC Constant
  | MakeTermV Variable
  | MakeTermId Atom

data Atom = MakeAtom Identifier Term TermSequence

data Fact = MakeFact Atom

data AtomSequence
  = EmptyAS
  | MakeAS Atom AtomSequence

data Rule = MakeRule Atom Atom

isIdentifier :: String -> Bool
isIdentifier [] = False
isIdentifier (x : xs) =
  isAsciiLower x
    && all (\y -> isLetter y || isDigit y || y == '_') xs

toLNS :: String -> LetterNumberSequence
toLNS = foldr Cons EmptyLNS

toIdentifier :: String -> Identifier
toIdentifier [] = error "empty String cannot be an identifier"
toIdentifier s@(x : xs)
  | not (isIdentifier s) = error $ s ++ " cannot be a valid identifier"
  | otherwise = MakeId x (toLNS xs)

toVariable :: String -> Variable
toVariable [] = error "empty String cannot be a variable"
toVariable s@(x : xs)
  | not (isVariable s) = error $ s ++ " cannot be a valid variable"
  | otherwise = MakeVar x (toLNS xs)

toConstant :: String -> Constant
toConstant [] = error "empty String cannot be a constant"
toConstant s@(x : xs)
  | not (isConstant s) = error $ s ++ " cannot be a valid constant"
  | otherwise = MakeC (MakeId x (toLNS xs))

--todo should '_' be a valid part of identifier?

isVariable :: String -> Bool
isVariable [] = False
isVariable (x : xs) =
  isAsciiUpper x
    && all (\y -> isLetter y || isDigit y) xs

isConstant :: String -> Bool
isConstant = isIdentifier

splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy s l =
  first :
  splitBy
    s
    ( if null remain
        then remain
        else dropWhile (== ' ') (tail remain)
    )
  where
    first = firstValid l 0
    firstValid [] _ = []
    firstValid (x : xs) c
      | x == s && c == 0 = []
      | x == '(' = x : firstValid xs (c + 1)
      | x == ')' = x : firstValid xs (c -1)
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

--todo toAtom :: String -> Atom

isTerm :: String -> Bool
isTerm [] = False
isTerm l@(x : xs) =
  isConstant l
    || isVariable l
    || isAtom l

--todo toTerm :: String -> Term

isFact :: String -> Bool
isFact l =
  (not . null) l
    && last l == '.'
    && (isAtom . init) l

-- toFact :: String -> Fact
-- toFact s
--       | not (isFact s) = error $ s ++ " cannot be a valid fact"
--       | otherwise = MakeFact (toAtom (init s))
      --todo

isRule :: String -> Bool
isRule l =
  (not . null) l
    && last l == '.'
    && hasSpecial
    && isAtom beforeSpecial
    && all
      isAtom
      (splitBy ',' afterSpecial)
  where
    noDot = init l
    hasSpecial = " :- " `isSubstring` noDot
    isSubstring [] _ = True
    isSubstring _ [] = False
    isSubstring (x : xs) (y : ys) = (x == y && isSubstring xs ys) || isSubstring (x : xs) ys
    beforeSpecial = takeWhile (/= ' ') noDot
    afterSpecial = drop 4 (dropWhile (/= ' ') noDot)

removeEmpty :: [String] -> [String]
removeEmpty = filter (not . null)

isComment :: String -> Bool
isComment l = (not . null) l && head (dropWhile (== ' ') l) == '%'

extractData :: String -> [String]
extractData l = removeEmpty (splitBy '\n' l)

consult :: String -> (Bool, [String])
consult contents = (truth, if truth then [] else filter (\x -> not (isFact x || isRule x || isComment x)) (extractData contents))
  where
    truth = all (\x -> isFact x || isRule x || isComment x) (extractData contents)

removeWhiteSpacesAfterComma :: String -> String
removeWhiteSpacesAfterComma [] = []
removeWhiteSpacesAfterComma (x : xs)
  | x == ',' = x : removeWhiteSpacesAfterComma (dropWhile (== ' ') xs)
  | otherwise = x : removeWhiteSpacesAfterComma xs

check :: String -> [String] -> IO ()
check input database = do
  if input == "quit"
    then return ()
    else
      if not (isFact input) && not (isRule input)
        then do
          print "You are allowed to input only facts and queries!"
          userInteract database
        else do
          print $ if input `elem` database then "true." else "false."
          userInteract database

userInteract :: [String] -> IO ()
userInteract database = do
  factInput <- getLine
  let fact = removeWhiteSpacesAfterComma factInput
  check fact database

workWithFile :: String -> IO ()
workWithFile path = do
  contents <- readFile ("prolog/" ++ path)
  let truth = consult contents
  print $ if fst truth then "true." else "false.\n" ++ unlines (snd truth)
  let realCode = [removeWhiteSpacesAfterComma x | x <- lines contents, (not . isComment) x, (not . null) x]
  userInteract realCode

loop :: IO ()
loop = do
  putStr "Which file to consult from the directory \"prolog/\"?\n> "
  file <- getLine
  workWithFile file
  putStrLn "Consult another file? ( y | [n] )"
  response <- getLine
  if (not . null) response && head response == 'y' then loop else return ()

main :: IO ()
main = do
  loop
  putStrLn "Closing..."
  response <- getLine
  return ()