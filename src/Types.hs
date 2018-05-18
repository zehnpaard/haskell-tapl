module Types where

import Text.Printf

data PTerm = PTmVar String
           | PTmAbs String PTerm
           | PTmApp PTerm PTerm
           | PTmTrue
           | PTmFalse
           | PTmIf PTerm PTerm PTerm
  deriving Eq

instance Show PTerm where show = showPTerm
showPTerm (PTmVar s)       = s
showPTerm (PTmAbs s t)     = printf "λ%s.%s" s $ show t
showPTerm (PTmApp t1 t2)   = printf "(%s %s)" (show t1) (show t2)
showPTerm PTmTrue          = "true"
showPTerm PTmFalse         = "false"
showPTerm (PTmIf t1 t2 t3) = printf "(if %s then %s else %s)" (show t1) (show t2) (show t3)

data Term = TmVar Int
          | TmAbs Term
          | TmApp Term Term
          | TmTrue
          | TmFalse
  deriving Eq

instance Show Term where show = showTerm
showTerm (TmVar n)     = show n
showTerm (TmAbs t)     = "λ." ++ show t
showTerm (TmApp t1 t2) = printf "(%s %s)" (show t1) (show t2)
showTerm TmTrue        = "true"
showTerm TmFalse       = "false"
