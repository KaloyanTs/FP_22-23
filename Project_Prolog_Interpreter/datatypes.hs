module Datatypes
where

data LetterNumberSequence
  = EmptyLNS
  | Cons Char LetterNumberSequence

data Identifier = MakeId Char LetterNumberSequence

--todo assert first letter is capital
data Variable = MakeVar Char LetterNumberSequence

type Constant = Identifier

data TermSequence
  = EndTS Term
  | MakeTS Term TermSequence

data Term
  = MakeTermC Constant
  | MakeTermV Variable
  | MakeTermAtom Atom

data Atom = MakeAtom Identifier TermSequence

type Fact = Atom

data AtomSequence
  = EndAS Atom
  | MakeAS Atom AtomSequence

data Rule = MakeRule Atom AtomSequence

type Database = ([Rule], [Fact])