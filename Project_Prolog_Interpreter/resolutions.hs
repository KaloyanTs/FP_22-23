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
resolve t (r,f) = uniqueQRs $ factsRes ++ searchSolution rulesRes
  where
    factsRes = filter notBad (map (\fact->toBeUnified (factToTerm fact, t)) f)
    -- rules whose head can be unified with the term
    -- the queryResult is kept for applying over the atomseqence
    rulesRes = filter (notBad.snd)
                      (map
                      (\rule@(MakeRule a as)->(as,toBeUnified (MakeTermAtom a, t)))
                      r)
    searchSolution results = map (\res@(as,requirements)->
                                          if solve (apply as requirements)
                                          then requirements
                                          else EndQR False) results
      where
    --todo solve says if atom sequence has compatible solution
        solve :: AtomSequence -> Bool
        solve as = False
        apply :: AtomSequence -> QueryResult -> AtomSequence
        apply x qr = x
    --todo solve today

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