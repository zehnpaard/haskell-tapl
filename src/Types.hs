module Types where

import Text.Printf

data Term = TmVar String
          | TmAbs String Term
          | TmApp Term Term
  deriving Eq

instance Show Term where show = showTerm

showTerm (TmVar s)     = s
showTerm (TmAbs s t)   = printf "λ%s.%s" s $ show t
showTerm (TmApp t1 t2) = printf "(%s %s)" (show t1) (show t2)
