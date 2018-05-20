module Parser where

import Text.ParserCombinators.Parsec

import Types

readExpr :: String -> PTerm
readExpr input = case parse parseExpr "lambda" input of
  Left err -> error $ show err
  Right t  -> t

parseExpr :: Parser PTerm
parseExpr = parseAbs 
        <|> try parseIf
        <|> parseApp
        <|> try parseBool
        <|> parseVar


parseAbs :: Parser PTerm
parseAbs = PTmAbs <$ char 'λ' <*> parseVarString 
                  <* char ':' <*> parseType
                  <* char '.' <*> parseExpr

parseVar :: Parser PTerm
parseVar = PTmVar <$> parseVarString

parseVarString :: Parser String
parseVarString = (:) <$> letter <*> many alphaNum


parseApp :: Parser PTerm
parseApp = PTmApp <$ char '(' <*> parseExpr <* char ' ' <*> parseExpr <* char ')'

parseBool :: Parser PTerm
parseBool = parseTrue <|> parseFalse

parseTrue :: Parser PTerm
parseTrue = string "true" *> pure PTmTrue

parseFalse :: Parser PTerm
parseFalse = string "false" *> pure PTmFalse


parseIf :: Parser PTerm
parseIf = PTmIf <$ string "(if " <*> parseExpr 
                <* string " then " <*> parseExpr
                <* string " else " <*> parseExpr
                <* char ')'

parseType :: Parser TmType
parseType = try parseTpArrow <|> parseTypex

parseTpArrow :: Parser TmType
parseTpArrow = TpArrow <$> parseTypex <* string "->" <*> parseType

parseTypex :: Parser TmType
parseTypex = parseTpBool <|> char '(' *> parseType <* char ')'

parseTpBool :: Parser TmType
parseTpBool = string "Bool" *> pure TpBool
