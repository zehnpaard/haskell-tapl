module Evaluate where

import Types

walk :: (Int -> Term -> Term) -> Int -> Term -> Term
walk varFn absDepth term
  | (TmVar _) <- term     = varFn absDepth term
  | (TmAbs t) <- term     = TmAbs $ walk varFn (absDepth + 1) t
  | (TmApp t1 t2) <- term = TmApp (walk varFn absDepth t1) (walk varFn absDepth t2)
  | TmTrue <- term        = TmTrue
  | TmFalse <- term       = TmFalse

substitute :: Term -> Term -> Term
substitute innerTerm outerTerm = walk varFn 0 outerTerm
  where varFn absDepth t@(TmVar n) = if (n==absDepth) then innerTerm else t

isValue :: Term -> Bool
isValue (TmAbs _) = True
isValue TmTrue    = True
isValue TmFalse   = True
isValue _         = False

eval1 :: Term -> Term
eval1 (TmApp t1 t2)
  | not $ isValue t1 = TmApp (eval1 t1) t2
  | not $ isValue t2 = TmApp t1 (eval1 t2)
  | TmAbs t <- t1    = substitute t2 t
eval1 t              = error $ "No evaluation applicable to " ++ show t

eval :: Term -> [Term]
eval t
  | isValue t = [t]
  | otherwise = t:(eval $ eval1 t)
