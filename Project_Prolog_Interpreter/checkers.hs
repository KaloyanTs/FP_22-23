module Checkers where

import Data.Char
import Datatypes
import Tools

isIdentifier :: String -> Bool
isIdentifier [] = False
isIdentifier (x : xs) =
  isAsciiLower x
    && all (\y -> isLetter y || isDigit y || y == '_') xs

--todo should '_' be a valid part of identifier?

isVariable :: String -> Bool
isVariable [] = False
isVariable (x : xs) =
  isAsciiUpper x
    && all (\y -> isLetter y || isDigit y) xs

isConstant :: String -> Bool
isConstant = isIdentifier

isAtom :: String -> Bool
isAtom l =
  isValidPar && isIdentifier beforePar && not (null insidePar)
    && all isTerm (map removeEndSpace (splitBy ',' insidePar))
  where
    beforePar = takeWhile (/= '(') l
    parPart = dropWhile (/= '(') l
    isValidPar = (not . null) parPart && (head parPart == '(' && last parPart == ')')
    insidePar = init $ tail parPart

isTerm :: String -> Bool
isTerm [] = False
isTerm l@(x : xs) =
  isConstant l
    || isVariable l
    || isAtom l

isFact :: String -> Bool
isFact l =
  (not . null) l
    && last l == '.'
    && (isAtom . init) l

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

isEquality :: String -> Bool
isEquality str =
  '=' `elem` str
    && last str == '.'
    && isTerm before
    && isTerm after
  where
    noDot = init str
    breaking = break (== '=') noDot
    before =
      reverse $
        dropWhile (== ' ') $
          reverse $
            fst breaking
    after = dropWhile (== ' ') $ tail $ snd breaking

isComment :: String -> Bool
isComment l = (not . null) l && head (dropWhile (== ' ') l) == '%'

termContainsVariable :: Term -> Bool
termContainsVariable (MakeTermC _) = False
termContainsVariable (MakeTermV _) = True
termContainsVariable (MakeTermAtom a) = atomContainsVariable a

atomContainsVariable :: Atom -> Bool
atomContainsVariable (MakeAtom _ ts) = tsContainsVariable ts

tsContainsVariable :: TermSequence -> Bool
tsContainsVariable (EndTS t) = termContainsVariable t
tsContainsVariable (MakeTS t ts) =
  termContainsVariable t
    || tsContainsVariable ts

anyAS :: (Atom->Bool) -> AtomSequence -> Bool
anyAS p (EndAS a) = p a
anyAS p (MakeAS a as) = p a || any p as

allAS :: (Atom->Bool) -> AtomSequence -> Bool
allAS p (EndAS a) = p a
allAS p (MakeAS a as) = p a && any p as