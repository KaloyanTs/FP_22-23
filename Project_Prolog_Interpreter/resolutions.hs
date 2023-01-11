module Resolutions where

import Checkers
import Conversions
import Datatypes
import Identities
import Unification

uniqueQRs :: [QueryResult] -> [QueryResult]
uniqueQRs [] = []
uniqueQRs (qr : qrs) = qr : filter (not . areIdenticalQR qr) qrs

--todo understand resolution and implement here
--todo quieries must not finish with false (at least not to be shown)
resolve :: Term -> Database -> [QueryResult]
resolve t db@(r, f)
  | noVars = [EndQR ((not . null) res)]
  | otherwise = res
  where
    noVars = (not . termContainsVariable) t
    res = uniqueQRs $ factsRes ++ searchSolution rulesRes
    factsRes = filter notBad (map (\fact -> toBeUnified (factToTerm fact, t)) f)
    -- rules whose head can be unified with the term
    -- the queryResult is kept for applyASing over the atomsequence
    rulesRes =
      filter
        (notBad . snd)
        ( map
            (\rule@(MakeRule a as) -> (as, toBeUnified (MakeTermAtom a, t)))
            r
        )
    --todo not working
    searchSolution results =
      filter notBad $
        map
          ( \res@(as, requirements) ->
              if solve (applyAS as requirements)
                then -- todo why it is empty for ivan,penka
                  requirements
                else EndQR False
          )
          results
      where
        --todo solve says if atom sequence has compatible solution
        --todo solve must return all requirements and function must use them to tell which are necessary
        solve :: AtomSequence -> Bool
        solve (EndSequence a) = (\x -> (not . null) x && notBad (head x)) $ resolve (MakeTermAtom a) db
        solve (MakeSequence a as) = any (solve . applyAS as) (resolve (MakeTermAtom a) db)

--   todo still not understanding

buildRTree :: Database -> [Atom] -> QueryResult -> ResolutionTree
buildRTree _ [] qr = LeafRT qr
buildRTree db@(r, f) arr@(a : as) qr = NodeRT arr children
  where
    children = factChildren ++ ruleChildren
    factChildren = map (\fqr -> buildRTree db (map (`apply` qr) as) (appendQR fqr qr)) (fst u)
    ruleChildren = map (\(al, rqr) -> buildRTree db (map (`apply` rqr) (al ++ as)) (appendQR rqr qr)) (snd u)
    u = unifiers db a

unifiers :: Database -> Atom -> ([QueryResult], [([Atom], QueryResult)])
unifiers db@(r, f) a =
  ( filter notBad (map (\fact -> toBeUnified (factToTerm fact, MakeTermAtom a)) f),
    filter (notBad . snd) (map (\(MakeRule ah as) -> (asToAtomArray as, toBeUnified (MakeTermAtom ah, MakeTermAtom a))) r)
  )

collectSolutions :: ResolutionTree -> [QueryResult]
collectSolutions EmptyRT = []
collectSolutions (LeafRT qr) = [qr]
collectSolutions (NodeRT _ ts) = concatMap collectSolutions ts

needed :: Atom -> [QueryResult] -> [QueryResult]
needed a qrs = filter (notBad) (map (onlyUseful vars) qrs)
  where
    vars = getVariablesAtom a
    onlyUseful :: [Variable] -> QueryResult -> QueryResult
    -- onlyUseful _ qr = qr
    onlyUseful [] _ = EndQR True
    onlyUseful arr (EndQR b) = EndQR b
    onlyUseful arr (MakeQR qr@(var, _) qrs)
      | any (areIdenticalVariables var) arr = MakeQR qr (onlyUseful (filter (not . areIdenticalVariables var) arr) qrs)
      | otherwise = onlyUseful (filter (not . areIdenticalVariables var) arr) qrs

interpreteInput :: String -> Database -> [QueryResult]
interpreteInput input db@(r, f)
  --  | isFact input = resolve readyTerm db
  | isFact input = needed readyAtom $ collectSolutions $ buildRTree db [readyAtom] (EndQR True)
  -- todo check what variables are needed !!!!!!!!!!!
  | otherwise = [toBeUnified (toEquality input)]
  where
    readyAtom = toAtom (init input)