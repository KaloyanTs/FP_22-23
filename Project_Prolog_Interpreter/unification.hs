module Unification
where

import Datatypes
import Checkers
import Identities

lengthTS :: TermSequence -> Int
lengthTS (EndTS _) = 1
lengthTS (MakeTS _ ts) = 1 + lengthTS ts

insertInStack :: TermSequence -> TermSequence -> [(Term, Term)] -> [(Term, Term)]
insertInStack (EndTS t1) (EndTS t2) stack = (t1, t2) : stack
insertInStack (MakeTS t1 ts1) (MakeTS t2 ts2) stack = insertInStack ts1 ts2 $ (t1, t2) : stack
insertInStack _ _ _ = error "TermSequnces must be of equal length"

toBeUnified :: (Term, Term) -> [(Variable, Identifier)]
--                      stack  result
toBeUnified pair@(l, r)
  | termContainsVariable l || termContainsVariable r = iter [pair] []
  | areIdenticalTerms l r = [(MakeVar 'D' EmptyLNS, MakeId 'd' EmptyLNS)]
  | otherwise = []
  where
    --todo find more meaningfull return above

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