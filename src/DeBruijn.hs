module DeBruijn where

import Data.List

import Types

convertTerm :: [String] -> PTerm -> Term
convertTerm context (PTmVar s) = case findIndex (==s) context of
  Nothing -> TmVar (-1)
  Just n  -> TmVar n
convertTerm context (PTmAbs s t) = TmAbs $ convertTerm (s:context) t
convertTerm context (PTmApp t1 t2) = TmApp (convertTerm context t1) (convertTerm context t2)
