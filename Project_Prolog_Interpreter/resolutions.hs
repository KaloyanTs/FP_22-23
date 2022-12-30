module Resolutions where

import Checkers
import Conversions
import Datatypes
import Identities
import Unification

--todo understand resolution and implement here
resolve :: Term -> Database -> [QueryResult]
resolve t (r,f)
  | not (null factsRes) = factsRes
  | otherwise = [EndQR False] --todo start working with rules, now is bad
  where
    factsRes = filter notBad (map (\fact->toBeUnified (factToTerm fact, t)) f)

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