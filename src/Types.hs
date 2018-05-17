module Types where

data Term = TmVar String
  deriving Eq

instance Show Term where show = showTerm

showTerm (TmVar s) = s
