module TypeCheck where

import Control.Monad

import Types

typeOf :: [TmType] -> Term -> Maybe TmType

typeOf context (TmVar n)
  | length context > n = Just $ context !! n
  | otherwise          = Nothing

typeOf context (TmAbs tp t)    = TpArrow tp <$> typeOf (tp:context) t

typeOf context (TmApp t1 t2)   = join $ f <$> tp1 <*> tp2
  where tp1  = typeOf context t1
        tp2  = typeOf context t2
        f (TpArrow tp' tp'') tp2 | tp' == tp2  = Just tp''
        f _ _                                  = Nothing

typeOf _ TmTrue                = Just TpBool
typeOf _ TmFalse               = Just TpBool

typeOf context (TmIf t1 t2 t3) = join $ f <$> tp1 <*> tp2 <*> tp3
  where tp1  = typeOf context t1
        tp2  = typeOf context t2
        tp3  = typeOf context t3
        f TpBool tp2 tp3 | tp2 == tp3  = Just tp2
        f _ _ _                        = Nothing
