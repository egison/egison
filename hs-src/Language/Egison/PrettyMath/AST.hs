{- |
Module      : Language.Egison.PrettyMath.AST
Licence     : MIT
-}

module Language.Egison.PrettyMath.AST
  ( MathExpr(..)
  , MathIndex(..)
  , isSub
  , parseExpr
  ) where

import           Text.ParserCombinators.Parsec hiding (spaces)

data MathExpr
  = Atom String [MathIndex]
  | NegativeAtom String
  | Plus [MathExpr]
  | Multiply [MathExpr]
  | Power MathExpr MathExpr
  | Func MathExpr [MathExpr]
  | Tensor [MathExpr] [MathIndex]
  | Tuple [MathExpr]
  | Collection [MathExpr]
  | Exp MathExpr
  | Quote MathExpr
  | Partial MathExpr [MathExpr]
  deriving (Eq, Show)

data MathIndex
  = Super MathExpr
  | Sub MathExpr
  deriving (Eq, Show)

isSub :: MathIndex -> Bool
isSub (Sub _) = True
isSub _       = False

--
-- Parser
--

spaces :: Parser ()
spaces = skipMany1 space

spaces0 :: Parser ()
spaces0 = skipMany space

symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=>?@#"

parseAtom :: Parser MathExpr
parseAtom = Atom <$> ((:) <$> (letter <|> symbol <|> digit) <*> many (letter <|> digit <|> symbol)) <*> many parseScript

parseAtom' :: Parser MathExpr
parseAtom' = flip Atom [] <$> ((:) <$> (letter <|> symbol <|> digit) <*> many (letter <|> digit <|> symbol))

parsePartial :: Parser MathExpr
parsePartial = Partial <$> parseAtom <*> many1 (char '|' >> parseAtom)

parseNegativeAtom :: Parser MathExpr
parseNegativeAtom = char '-' >> NegativeAtom <$> ((:) <$> (letter <|> symbol <|> digit) <*> many (letter <|> digit <|> symbol))

parseList :: Parser [MathExpr]
parseList = sepEndBy parseExpr spaces

parseScript :: Parser MathIndex
parseScript = Sub <$> (char '_' >> parseAtom')
              <|> Super <$> (char '~' >> parseAtom')

parsePlus :: Parser MathExpr
parsePlus = string "(+" >> spaces >> Plus <$> parseList <* char ')'

parseMultiply :: Parser MathExpr
parseMultiply = string "(*" >> spaces >> Multiply <$> parseList <* char ')'

parseFunction :: Parser MathExpr
parseFunction = char '(' >> Func <$> parseAtom <* spaces <*> parseList <* char ')'

parseTensor :: Parser MathExpr
parseTensor = string "[|" >> spaces0 >> Tensor <$> parseList <* spaces0 <* string "|]" <*> many parseScript

parseTuple :: Parser MathExpr
parseTuple = char '[' >> Tuple <$> parseList <* char ']'

parseCollection :: Parser MathExpr
parseCollection = char '{' >> Collection <$> parseList <* char '}'

parseExp :: Parser MathExpr
parseExp = string "(exp" >> spaces >> Exp <$> parseExpr <* char ')'

parseQuote :: Parser MathExpr
parseQuote = char '\'' >> Quote <$> parseExpr'

parseExpr' :: Parser MathExpr
parseExpr' = parseNegativeAtom
         <|> try parsePartial
         <|> parseAtom
         <|> parseQuote
         <|> try parseExp
         <|> try parsePlus
         <|> try parseMultiply
         <|> try parseFunction
         <|> try parseTensor
         <|> try parseTuple
         <|> try parseCollection

parseExpr :: Parser MathExpr
parseExpr = do
  x <- parseExpr'
  option x $ Power x <$> try (char '^' >> parseExpr')
