module Types where

import Text.Printf

data PTerm = PTmVar String
           | PTmAbs String PTerm
           | PTmApp PTerm PTerm
  deriving Eq

instance Show PTerm where show = showPTerm
showPTerm (PTmVar s)     = s
showPTerm (PTmAbs s t)   = printf "λ%s.%s" s $ show t
showPTerm (PTmApp t1 t2) = printf "(%s %s)" (show t1) (show t2)

data Term = TmVar Int
          | TmAbs Term
          | TmApp Term Term
  deriving Eq

instance Show Term where show = showTerm
showTerm (TmVar n)     = show n
showTerm (TmAbs t)     = "λ." ++ show t
showTerm (TmApp t1 t2) = printf "(%s %s)" (show t1) (show t2)
