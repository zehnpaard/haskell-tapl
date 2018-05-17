module Parser where

import Text.ParserCombinators.Parsec

import Types

readExpr :: String -> PTerm
readExpr input = case parse parseExpr "lambda" input of
  Left err -> error $ show err
  Right t  -> t

parseExpr :: Parser PTerm
parseExpr = parseAbs 
        <|> parseApp
        <|> parseVar

parseVar :: Parser PTerm
parseVar = PTmVar <$> parseVarString

parseAbs :: Parser PTerm
parseAbs = PTmAbs <$ char 'λ' <*> parseVarString <* char '.' <*> parseExpr

parseApp :: Parser PTerm
parseApp = PTmApp <$ char '(' <*> parseExpr <* char ' ' <*> parseExpr <* char ')'

parseVarString :: Parser String
parseVarString = (:) <$> letter <*> many alphaNum
