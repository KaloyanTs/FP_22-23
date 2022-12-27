module Conversions where

import Checkers
import Datatypes
import Tools

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
  | otherwise = MakeId x (toLNS xs)

toAtom :: String -> Atom
toAtom l
  | not (isAtom l) = error $ l ++ " cannot be an atom"
  | otherwise = MakeAtom id (toTermSequence terms)
  where
    id = toIdentifier (takeWhile (/= '(') l)
    parPart = dropWhile (/= '(') l
    isValidPar = (not . null) parPart && (head parPart == '(' && last parPart == ')')
    terms = map toTerm (splitBy ',' (init (tail parPart)))

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

toFact :: String -> Fact
toFact s
  | not (isFact s) = error $ s ++ " cannot be a valid fact"
  | otherwise = toAtom (init s)

toRule :: String -> Rule
toRule l
  | not (isRule l) = error $ l ++ "cannot be a rule"
  | otherwise = MakeRule (toAtom beforeSpecial) (toAtomSequence atoms)
  where
    noDot = init l
    breaking = break (== ':') noDot
    beforeSpecial = init $ fst breaking
    afterSpecial = drop 3 $ snd breaking
    atoms = map toAtom (splitBy ',' afterSpecial)

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

toAtomSequence :: [Atom] -> AtomSequence
toAtomSequence [] = error "atom sequence consists of at least one atom"
toAtomSequence [a] = EndAS a
toAtomSequence (x : xs) = MakeAS x (toAtomSequence xs)

showAtom :: Atom -> [Char]
showAtom (MakeAtom id ts) = showIdentifier id ++ "(" ++ showTermSequence ts ++ ")"

showTermSequence :: TermSequence -> [Char]
showTermSequence (EndTS t) = showTerm t
showTermSequence (MakeTS t ts) = showTerm t ++ "," ++ showTermSequence ts

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

--todo not sure whether those above are necessary

factToTerm :: Fact -> Term
factToTerm f = MakeTermAtom (toAtom (init (showFact f)))