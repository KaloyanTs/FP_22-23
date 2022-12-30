module Resolutions where

import Checkers
import Datatypes
import Identities
import Unification

--todo understand resolution and implement here
resolve :: Term -> Database -> QueryResult
resolve t (r, f)
  | any (toBeUnified t) f = EndQR True
  | otherwise = iterRules r
  where
    iterRules :: [Rule] -> QueryResult
    iterRules [] = EndQR False
    iterRules (r : rules) = if not (bad outcome) then outcome else iterRules rules
      where
        outcome = try t r
    bad :: QueryResult -> Bool
    bad (EndQR False) = True
    bad qr = False
    good :: QueryResult -> Bool
    good (EndQR True) = True
    good qr = False
    try :: Term -> Rule -> QueryResult
    try (MakeTermAtom ta) (MakeRule a as)
      | bad possibleCond = EndQR False
      | good possibleCond = possibleCond
      | otherwise = iterQR possibleCond as
      where
        possibleCond = toBeUnified ta a
        iterQR :: QueryResult -> AtomSequence -> QueryResult
        iterQR (EndQR _) _ = EndQR 
        iterQR (MakeQR req qr) as = if success then success else iterQR qr as
          where
            success = iterAS req as []
        iterAS :: [(Variable, Identifier)] -> AtomSequence -> [(Variable, Identifier)] -> QueryResult
        iterAS [] _ _ = error "something went wrong..."
        iterAS req (EndAS a) res = res ++ resolve (apply req a) db
        iterAS req (MakeAS a as) res
          | bad searchApplied = EndQR False
          where
            searchApplied = resolve (apply res a) db
    try _ _ = error "it shouldn't occur..."