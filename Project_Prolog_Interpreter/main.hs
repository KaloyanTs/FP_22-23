{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use bimap" #-}

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isLetter)

data LetterNumberSequence
  = EmptyLNS
  | Cons Char LetterNumberSequence

data Identifier = MakeId Char LetterNumberSequence

data Variable = MakeVar Char LetterNumberSequence

type Constant = Identifier

data TermSequence
  = EndTS Term
  | MakeTS Term TermSequence

data Term
  = MakeTermC Constant
  | MakeTermV Variable
  | MakeTermAtom Atom

data Atom = MakeAtom Identifier TermSequence

type Fact = Atom

data AtomSequence
  = EndAS Atom
  | MakeAS Atom AtomSequence

data Rule = MakeRule Atom AtomSequence

toLNS :: String -> LetterNumberSequence
toLNS = foldr Cons EmptyLNS

isIdentifier :: String -> Bool
isIdentifier [] = False
isIdentifier (x : xs) =
  isAsciiLower x
    && all (\y -> isLetter y || isDigit y || y == '_') xs

toIdentifier :: String -> Identifier
toIdentifier [] = error "empty String cannot be an identifier"
toIdentifier s@(x : xs)
  | not (isIdentifier s) = error $ s ++ " cannot be a valid identifier"
  | otherwise = MakeId x (toLNS xs)

--todo should '_' be a valid part of identifier?

isVariable :: String -> Bool
isVariable [] = False
isVariable (x : xs) =
  isAsciiUpper x
    && all (\y -> isLetter y || isDigit y) xs

toVariable :: String -> Variable
toVariable [] = error "empty String cannot be a variable"
toVariable s@(x : xs)
  | not (isVariable s) = error $ s ++ " cannot be a valid variable"
  | otherwise = MakeVar x (toLNS xs)

isConstant :: String -> Bool
isConstant = isIdentifier

toConstant :: String -> Constant
toConstant [] = error "empty String cannot be a constant"
toConstant s@(x : xs)
  | not (isConstant s) = error $ s ++ " cannot be a valid constant"
  | otherwise = MakeId x (toLNS xs)

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
isAtom l = isValidPar && isIdentifier beforePar && not (null insidePar) && all isTerm (splitBy ',' insidePar)
  where
    beforePar = takeWhile (/= '(') l
    parPart = dropWhile (/= '(') l
    isValidPar = (not . null) parPart && (head parPart == '(' && last parPart == ')')
    insidePar = init $ tail parPart

toAtom :: String -> Atom
toAtom l
  | not (isAtom l) = error $ l ++ " cannot be an atom"
  | otherwise = MakeAtom id (toTermSequence terms)
  where
    id = toIdentifier (takeWhile (/= '(') l)
    parPart = dropWhile (/= '(') l
    isValidPar = (not . null) parPart && (head parPart == '(' && last parPart == ')')
    terms = map toTerm (splitBy ',' (init (tail parPart)))

toAtomSequence :: [Atom] -> AtomSequence
toAtomSequence [] = error "atom sequence consists of at least one atom"
toAtomSequence [a] = EndAS a
toAtomSequence (x : xs) = MakeAS x (toAtomSequence xs)

isTerm :: String -> Bool
isTerm [] = False
isTerm l@(x : xs) =
  isConstant l
    || isVariable l
    || isAtom l

toTerm :: String -> Term
toTerm l
  | not (isTerm l) = error $ l ++ " cannot be a term"
  | isConstant l = MakeTermC (toConstant l)
  | isVariable l = MakeTermV (toVariable l)
  | otherwise = MakeTermAtom (toAtom l)

toTermSequence :: [Term] -> TermSequence
toTermSequence [] = error "term sequence has at least one term"
toTermSequence [t] = EndTS t
toTermSequence (x : xs) = MakeTS x (toTermSequence xs)

isFact :: String -> Bool
isFact l =
  (not . null) l
    && last l == '.'
    && (isAtom . init) l

toFact :: String -> Fact
toFact s
  | not (isFact s) = error $ s ++ " cannot be a valid fact"
  | otherwise = toAtom (init s)

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

toRule :: String -> Rule
toRule l
  | not (isRule l) = error $ l ++ "cannot be a rule"
  | otherwise = MakeRule (toAtom beforeSpecial) (toAtomSequence atoms)
  where
    noDot = init l
    beforeSpecial = init (takeWhile (/= ':') noDot)
    afterSpecial = drop 3 (dropWhile (/= ':') noDot)
    atoms = map toAtom (splitBy ',' afterSpecial)

--todo use break function 2 lines above

--todo implement
isEquality :: String -> Bool
isEquality str =
  '=' `elem` str
    && last str == '.'
    && isTerm before
    && isTerm after
  where
    noDot = init str
    before =
      reverse $
        dropWhile (== ' ') $
          reverse $
            takeWhile (/= '=') noDot
    after = dropWhile (== ' ') $ tail $ dropWhile (/= '=') noDot

--todo use break function again

toEquality :: String -> (Term, Term)
toEquality str
  | not (isEquality str) = error $ str ++ " cannot be an equality"
  | otherwise = (toTerm before, toTerm after)
  where
    noDot = init str
    before =
      reverse $
        dropWhile (== ' ') $
          reverse $
            takeWhile (/= '=') noDot
    after = dropWhile (== ' ') $ tail $ dropWhile (/= '=') noDot

areIdenticalLNS :: LetterNumberSequence -> LetterNumberSequence -> Bool
areIdenticalLNS EmptyLNS EmptyLNS = True
areIdenticalLNS (Cons c1 lns1) (Cons c2 lns2) =
  c1 == c2
    && areIdenticalLNS lns1 lns2
areIdenticalLNS _ _ = False

areIdenticalIds :: Identifier -> Identifier -> Bool
areIdenticalIds (MakeId c1 lns1) (MakeId c2 lns2) =
  c1 == c2
    && areIdenticalLNS lns1 lns2

areIdenticalConstants :: Constant -> Constant -> Bool
areIdenticalConstants = areIdenticalIds

areIdenticalVariables :: Variable -> Variable -> Bool
areIdenticalVariables (MakeVar c1 lns1) (MakeVar c2 lns2) =
  c1 == c2
    && areIdenticalLNS lns1 lns2

lengthTS :: TermSequence -> Int
lengthTS (EndTS _) = 1
lengthTS (MakeTS _ ts) = 1 + lengthTS ts

insertInStack :: TermSequence -> TermSequence -> [(Term, Term)] -> [(Term, Term)]
insertInStack (EndTS t1) (EndTS t2) stack = (t1, t2) : stack
insertInStack (MakeTS t1 ts1) (MakeTS t2 ts2) stack = insertInStack ts1 ts2 $ (t1, t2) : stack
insertInStack _ _ _ = error "TermSequnces must be of equal length"

toBeUnified :: (Term, Term) -> [(Variable, Identifier)]
--                      stack  result
toBeUnified pair = iter [pair] []
  where
    iter :: [(Term, Term)] -> [(Variable, Identifier)] -> [(Variable, Identifier)]
    iter [] res = res
    iter ((MakeTermC lhs, MakeTermC rhs) : pairs) res
      | areIdenticalConstants lhs rhs = iter pairs res
      | otherwise = []
    iter ((MakeTermV lhs, MakeTermV rhs) : pairs) res
      | areIdenticalVariables lhs rhs = iter pairs res
      | otherwise = []
    iter ((MakeTermC lhs, MakeTermV rhs) : pairs) res =
      iter (replaceStack rhs lhs pairs) ((rhs, lhs) : res)
    --todo should lhs and rhs be replaced in the res??????
    iter ((MakeTermV lhs, MakeTermC rhs) : pairs) res =
      iter (replaceStack lhs rhs pairs) ((lhs, rhs) : res)
    iter ((MakeTermV _, _) : pairs) _ = []
    iter ((MakeTermC _, _) : pairs) _ = []
    iter ((_, MakeTermV _) : pairs) _ = []
    iter ((_, MakeTermC _) : pairs) _ = []
    iter ((MakeTermAtom lhsAtom, MakeTermAtom rhsAtom) : pairs) res =
      proceedAtoms lhsAtom rhsAtom pairs res
    proceedAtoms (MakeAtom id1 ts1) (MakeAtom id2 ts2) stack res
      | not (areIdenticalIds id1 id2) = []
      | lengthTS ts1 /= lengthTS ts2 = []
      | otherwise = iter (insertInStack ts1 ts2 stack) res

replaceStack :: Variable -> Constant -> [(Term, Term)] -> [(Term, Term)]
replaceStack var c = map (\(l, r) -> (replaceInTerm l, replaceInTerm r))
  where
    replaceInTerm :: Term -> Term
    replaceInTerm p@(MakeTermV v)
      | areIdenticalVariables v var = MakeTermC c
      | otherwise = p
    replaceInTerm p@(MakeTermC _) = p
    replaceInTerm p@(MakeTermAtom a) = MakeTermAtom (replaceInAtom a)
    replaceInAtom :: Atom -> Atom
    replaceInAtom a@(MakeAtom id ts) = MakeAtom id (replaceInTS ts)
    replaceInTS :: TermSequence -> TermSequence
    replaceInTS (EndTS t) = EndTS (replaceInTerm t)
    replaceInTS (MakeTS t ts) = MakeTS (replaceInTerm t) (replaceInTS ts)

showAtom :: Atom -> [Char]
showAtom (MakeAtom id ts) = showIdentifier id ++ "(" ++ showTermSequence ts ++ ")"

showTermSequence :: TermSequence -> [Char]
showTermSequence (EndTS t) = showTerm t
showTermSequence (MakeTS t ts) = showTerm t ++ showTermSequence ts

showTerm :: Term -> [Char]
showTerm (MakeTermC c) = showConstant c
showTerm (MakeTermV v) = showVariable v
showTerm (MakeTermAtom a) = showAtom a

showConstant :: Identifier -> [Char]
showConstant = showIdentifier

showVariable :: Variable -> [Char]
showVariable (MakeVar c lns) = c : showLNS lns

showIdentifier :: Identifier -> [Char]
showIdentifier (MakeId c lns) = c : showLNS lns

showLNS :: LetterNumberSequence -> [Char]
showLNS EmptyLNS = []
showLNS (Cons a lns) = a : showLNS lns

showRule :: Rule -> String
showRule (MakeRule a as) = showAtom a ++ " :- " ++ showAtomSequence as ++ "."

showAtomSequence :: AtomSequence -> [Char]
showAtomSequence (EndAS a) = showAtom a
showAtomSequence (MakeAS a as) = showAtom a ++ "," ++ showAtomSequence as

showFact :: Atom -> [Char]
showFact a = showAtom a ++ "."

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

--todo separate the above in a module

type Database = ([Rule], [Fact])

interpreteCode :: [String] -> Database
interpreteCode c = (rules, facts)
  where
    rules = map toRule (filter isRule c)
    facts = map toFact (filter isFact c)

showFacts :: Database -> [String]
showFacts (r, f) = map showFact f

showRules :: Database -> [String]
showRules (r, f) = map showRule r

getFactIds :: Fact -> [Identifier]
getFactIds (MakeAtom id ts) = id : getTSIds ts

getTSIds :: TermSequence -> [Identifier]
getTSIds (EndTS t) = getTermIds t
getTSIds (MakeTS t ts) = getTermIds t ++ getTSIds ts

getTermIds :: Term -> [Identifier]
getTermIds (MakeTermC c) = [c]
getTermIds (MakeTermV _) = []
getTermIds (MakeTermAtom a) = getAtomIds a

getAtomIds :: Atom -> [Identifier]
getAtomIds (MakeAtom id ts) = id : getTSIds ts

--todo not sure whether those below are necessary
getASIds :: AtomSequence -> [Identifier]
getASIds (EndAS a) = getAtomIds a
getASIds (MakeAS a as) = getAtomIds a ++ getASIds as

getRuleIds :: Rule -> [Identifier]
getRuleIds (MakeRule a as) = getAtomIds a ++ getASIds as

allIdentifiers :: Database -> [Identifier]
allIdentifiers (r, f) = concatMap getRuleIds r ++ concatMap getFactIds f

--todo experiment using datatypes above

check :: String -> [String] -> IO ()
check input database = do
  if input == "quit"
    then return ()
    else
      if not (isFact input) && not (isRule input) && not (isEquality input)
        then do
          print "You are allowed to input only facts, queries and equalities!"
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