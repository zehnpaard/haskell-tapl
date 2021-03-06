module Types where

import Text.Printf

data PTerm = PTmVar String
           | PTmAbs String TmType PTerm
           | PTmApp PTerm PTerm
           | PTmTrue
           | PTmFalse
           | PTmIf PTerm PTerm PTerm
  deriving Eq

instance Show PTerm where show = showPTerm
showPTerm (PTmVar s)       = s
showPTerm (PTmAbs s tp t)  = printf "λ%s:%s.%s" s (show tp) (show t)
showPTerm (PTmApp t1 t2)   = printf "(%s %s)" (show t1) (show t2)
showPTerm PTmTrue          = "true"
showPTerm PTmFalse         = "false"
showPTerm (PTmIf t1 t2 t3) = printf "(if %s then %s else %s)" (show t1) (show t2) (show t3)

data Term = TmVar Int
          | TmAbs TmType Term
          | TmApp Term Term
          | TmTrue
          | TmFalse
          | TmIf Term Term Term
  deriving Eq

instance Show Term where show = showTerm
showTerm (TmVar n)       = show n
showTerm (TmAbs tp t)    = printf "λ:%s.%s" (show tp) (show t)
showTerm (TmApp t1 t2)   = printf "(%s %s)" (show t1) (show t2)
showTerm TmTrue          = "true"
showTerm TmFalse         = "false"
showTerm (TmIf t1 t2 t3) = printf "(if %s then %s else %s)" (show t1) (show t2) (show t3)

data TmType = TpBool | TpArrow TmType TmType
  deriving Eq

instance Show TmType where show = showType
showType TpBool = "Bool"
showType (TpArrow TpBool tp2) = "Bool->" ++ (show tp2)
showType (TpArrow tp1 tp2) = printf "(%s)->%s" (show tp1) (show tp2)
