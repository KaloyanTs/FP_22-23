{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module Unification where

import Checkers
import Datatypes
import Identities
import Conversions

lengthTS :: TermSequence -> Int
lengthTS (EndTS _) = 1
lengthTS (MakeTS _ ts) = 1 + lengthTS ts

insertInStack :: TermSequence -> TermSequence -> [(Term, Term)] -> [(Term, Term)]
insertInStack (EndTS t1) (EndTS t2) stack = (t1, t2) : stack
insertInStack (MakeTS t1 ts1) (MakeTS t2 ts2) stack = insertInStack ts1 ts2 $ (t1, t2) : stack
insertInStack _ _ _ = error "TermSequnces must be of equal length"

toBeUnified :: (Term, Term) -> QueryResult
toBeUnified pair@(l, r)
  | termContainsVariable l || termContainsVariable r = iter [pair] (EndQR False)
  | areIdenticalTerms l r = EndQR True
  | otherwise = EndQR False
  where
    iter :: [(Term, Term)] -> QueryResult -> QueryResult
    iter [] res = res
    iter ((MakeTermC lhs, MakeTermC rhs) : pairs) res
      | areIdenticalConstants lhs rhs = iter pairs res
      | otherwise = EndQR False
    iter ((MakeTermV lhs, MakeTermV rhs) : pairs) res 
    -- = iter pairs res
      | areIdenticalVariables lhs rhs = iter pairs res
      | otherwise = EndQR False
      -- todo consider removing this check
    iter ((MakeTermC lhs, MakeTermV rhs) : pairs) res =
      iter (replaceStack rhs lhs pairs) (MakeQR (rhs, lhs) res)
    --todo should lhs and rhs be replaced in the res??????
    iter ((MakeTermV lhs, MakeTermC rhs) : pairs) res =
      iter (replaceStack lhs rhs pairs) (MakeQR (lhs, rhs) res)
    iter ((MakeTermV _, _) : pairs) _ = EndQR False
    iter ((MakeTermC _, _) : pairs) _ = EndQR False
    iter ((_, MakeTermV _) : pairs) _ = EndQR False
    iter ((_, MakeTermC _) : pairs) _ = EndQR False
    iter ((MakeTermAtom lhsAtom, MakeTermAtom rhsAtom) : pairs) res =
      proceedAtoms lhsAtom rhsAtom pairs res
    proceedAtoms (MakeAtom id1 ts1) (MakeAtom id2 ts2) stack res
      | not (areIdenticalIds id1 id2) = EndQR False
      | lengthTS ts1 /= lengthTS ts2 = EndQR False
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

applyAS :: AtomSequence -> QueryResult -> AtomSequence
applyAS (EndAS a) qr = EndAS (apply a qr)
applyAS (MakeAS a as) qr = MakeAS (apply a qr) (applyAS as qr)
apply :: Atom -> QueryResult -> Atom
apply a (MakeQR (var,id) qr@(MakeQR _ _)) = apply (substituteAtom var id a) qr
apply a (MakeQR (var,id) (EndQR _)) = substituteAtom var id a
apply _ _ = error "query must not have been empty..."
substituteAtom :: Variable -> Identifier -> Atom -> Atom
substituteAtom var iD (MakeAtom idPart ts) = MakeAtom idPart (substituteTS var iD ts)
substituteTS :: Variable -> Identifier -> TermSequence -> TermSequence
substituteTS var id (EndTS t) = EndTS (substituteTerm var id t)
substituteTS var id (MakeTS t ts) = MakeTS (substituteTerm var id t) (substituteTS var id ts)
substituteTerm :: Variable -> Identifier -> Term -> Term
substituteTerm var id t@(MakeTermV v)
  | areIdenticalVariables var v = MakeTermC id
  | otherwise = t
substituteTerm var id t@(MakeTermAtom a) = MakeTermAtom (substituteAtom var id a)
substituteTerm _ _ t = t