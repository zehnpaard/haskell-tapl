module Parser where

import Text.ParserCombinators.Parsec

import Types

readExpr :: String -> Term
readExpr input = case parse parseExpr "lambda" input of
  Left err -> error $ show err
  Right t  -> t

parseExpr :: Parser Term
parseExpr = parseAbs 
        <|> parseApp
        <|> parseVar

parseVar :: Parser Term
parseVar = TmVar <$> parseVarString

parseAbs :: Parser Term
parseAbs = TmAbs <$ char 'λ' <*> parseVarString <* char '.' <*> parseExpr

parseApp :: Parser Term
parseApp = TmApp <$ char '(' <*> parseExpr <* char ' ' <*> parseExpr <* char ')'

parseVarString :: Parser String
parseVarString = (:) <$> letter <*> many alphaNum