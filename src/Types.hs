module Types where

data Term = TmVar String

instance Show Term where show = showTerm

showTerm (TmVar s) = s
