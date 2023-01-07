module Resolutions where

import Checkers
import Conversions
import Datatypes
import Identities
import Unification

uniqueQRs :: [QueryResult] -> [QueryResult]
uniqueQRs [] = []
uniqueQRs (qr:qrs) = qr : filter (not.areIdenticalQR qr) qrs

--todo understand resolution and implement here
--todo quieries must not finish with false (at least not to be shown)
resolve :: Term -> Database -> [QueryResult]
resolve t db@(r,f)
  | noVars = [EndQR ((not.null) res)]
  | otherwise =  res
  where
    noVars = (not.termContainsVariable) t
    res = uniqueQRs $ factsRes ++ searchSolution rulesRes
    factsRes = filter notBad (map (\fact->toBeUnified (factToTerm fact, t)) f)
    -- rules whose head can be unified with the term
    -- the queryResult is kept for applyASing over the atomsequence
    rulesRes = filter (notBad.snd)
                      (map
                      (\rule@(MakeRule a as)->(as,toBeUnified (t, MakeTermAtom a)))
                      r)
                      --todo not working
    searchSolution results = filter notBad $ map (\res@(as,requirements)->
                                          if solve (applyAS as requirements)
                                            -- todo why it is empty for ivan,penka
                                          then requirements
                                          else EndQR False) results
      where
    --todo solve says if atom sequence has compatible solution
    --todo solve must return all requirements and function must use them to tell which are necessary
        solve :: AtomSequence -> Bool
        solve (EndAS a) = (\x->(not.null) x && notBad (head x)) $ resolve (MakeTermAtom  a) db
        solve (MakeAS a as) = any (solve . applyAS as) (resolve (MakeTermAtom a) db)

    --   todo still not understanding

interpreteInput :: String -> Database -> [QueryResult]
interpreteInput input db@(r, f)
  | isFact input = resolve readyTerm db
  | otherwise = [toBeUnified (toEquality input)]
  where
    readyTerm = MakeTermAtom $ toAtom (init input)

-- interpreteInput :: String -> Database -> [QueryResult]
-- interpreteInput input (r, f)
--   | isFact input = reverse $ search f []
--   | isRule input = error "not done yet"
--   | otherwise = [toBeUnified (toEquality input)]
--   where
--     readyTerm = MakeTermAtom $ toAtom (init input)
--     search [] qrs = EndQR False : qrs
--     search (fact : fs) qrs
--       | good res = [res]
--       | notBad res = search fs (res : qrs)
--       | otherwise = search fs qrs
--       where
--         res = toBeUnified (factToTerm fact, readyTerm)