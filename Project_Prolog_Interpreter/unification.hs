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
      = iter (replaceStackVar lhs rhs pairs) 
              (MakeQR (lhs, ReplaceVar rhs) res)
      --  | areIdenticalVariables lhs rhs = iter 
      --                                   (replaceStackVar lhs rhs pairs) 
      --                                   (MakeQR (lhs, ReplaceVar rhs) res)
      --  | otherwise = EndQR False
      -- todo consider removing this check
    iter ((MakeTermC lhs, MakeTermV rhs) : pairs) res =
      iter (replaceStackC rhs lhs pairs) (MakeQR (rhs, ReplaceId lhs) res)
    --todo should lhs and rhs be replaced in the res??????
    iter ((MakeTermV lhs, MakeTermC rhs) : pairs) res =
      iter (replaceStackC lhs rhs pairs) (MakeQR (lhs, ReplaceId rhs) res)
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

replaceStackC :: Variable -> Constant -> [(Term, Term)] -> [(Term, Term)]
replaceStackC var c = map (\(l, r) -> (replaceInTerm l, replaceInTerm r))
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

replaceStackVar :: Variable -> Variable -> [(Term, Term)] -> [(Term, Term)]
replaceStackVar var r = map (\(l, r) -> (replaceInTerm l, replaceInTerm r))
  where
    replaceInTerm :: Term -> Term
    replaceInTerm p@(MakeTermV v)
      | areIdenticalVariables v var = MakeTermV r
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
apply a (MakeQR (var,r) qr@(MakeQR _ _)) = apply (substituteAtom var r a) qr
apply a (MakeQR (var,r) (EndQR _)) = substituteAtom var r a
apply _ _ = error "query must not have been empty..."
substituteAtom :: Variable -> Replacement -> Atom -> Atom
substituteAtom var r (MakeAtom idPart ts) = MakeAtom idPart (substituteTS var r ts)
substituteTS :: Variable -> Replacement -> TermSequence -> TermSequence
substituteTS var r (EndTS t) = EndTS (substituteTerm var r t)
substituteTS var r (MakeTS t ts) = MakeTS (substituteTerm var r t) (substituteTS var r ts)
substituteTerm :: Variable -> Replacement -> Term -> Term
substituteTerm var (ReplaceId id) t@(MakeTermV v)
  | areIdenticalVariables var v = MakeTermC id
  | otherwise = t
substituteTerm var (ReplaceVar a) t@(MakeTermV v)
  | areIdenticalVariables var v = MakeTermV a
  | otherwise = t
substituteTerm var r t@(MakeTermAtom a) = MakeTermAtom (substituteAtom var r a)
substituteTerm _ _ t = t