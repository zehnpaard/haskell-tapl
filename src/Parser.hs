module Parser where

import Text.ParserCombinators.Parsec

import Types

readExpr :: String -> Term
readExpr input = case parse parseExpr "lambda" input of
  Left err -> error $ show err
  Right t  -> t

parseExpr :: Parser Term
parseExpr = parseVar

parseVar :: Parser Term
parseVar = fmap TmVar $ (:) <$> letter <*> many alphaNum
