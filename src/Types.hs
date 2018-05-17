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
