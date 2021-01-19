{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Language.Egison.PrettyMath.AST
Licence     : MIT
-}

module Language.Egison.PrettyMath.AST
  ( MathExpr(..)
  , MathIndex(..)
  , ToMathExpr(..)
  , isSub
  , parseExpr
  ) where

import           Data.Foldable                 (toList)
import           Text.ParserCombinators.Parsec hiding (spaces)

import qualified Language.Egison.Data          as E
import qualified Language.Egison.IExpr         as E
import qualified Language.Egison.Math.Expr     as E

data MathExpr
  = Atom String [MathIndex]
  | NegativeAtom String
  | Plus [MathExpr]
  | Multiply [MathExpr]
  | Div MathExpr MathExpr
  | Power MathExpr MathExpr
  | Func MathExpr [MathExpr]
  | Tensor [MathExpr] [MathIndex]
  | Tuple [MathExpr]
  | Collection [MathExpr]
  | Quote MathExpr
  | FunctionSymbol String
  | Partial MathExpr [MathExpr]
  deriving (Eq, Show)

data MathIndex
  = Super MathExpr
  | Sub MathExpr
  deriving (Eq, Show)

isSub :: MathIndex -> Bool
isSub (Sub _) = True
isSub _       = False


class ToMathExpr a where
  toMathExpr :: a -> MathExpr

instance ToMathExpr E.EgisonValue where
  toMathExpr (E.ScalarData s)  = toMathExpr s
  toMathExpr (E.Tuple es)      = Tuple (map toMathExpr es)
  toMathExpr (E.Collection es) = Collection (map toMathExpr (toList es))
  toMathExpr (E.TensorData t)  = toMathExpr t
  toMathExpr e                 = Atom (show e) []

instance ToMathExpr a => ToMathExpr (E.Tensor a) where
  toMathExpr (E.Scalar _)       = undefined
  toMathExpr (E.Tensor [_] xs js) = Tensor (map toMathExpr (toList xs)) (map toMathIndex js)
  toMathExpr (E.Tensor [_, n] xs js) = Tensor (f (fromIntegral n) (map toMathExpr (toList xs))) (map toMathIndex js)
    where
      f _ [] = []
      f n xs = Tensor (take n xs) [] : f n (drop n xs)
  toMathExpr (E.Tensor _ xs js) = undefined

instance ToMathExpr E.ScalarData where
  toMathExpr (E.Div p (E.Plus [E.Term 1 []])) = toMathExpr p
  toMathExpr (E.Div p1 p2)                    = Div (toMathExpr p1) (toMathExpr p2)

instance ToMathExpr E.PolyExpr where
  toMathExpr (E.Plus [])  = Atom "0" []
  toMathExpr (E.Plus [x]) = toMathExpr x
  toMathExpr (E.Plus xs)  = Plus (map toMathExpr xs)

instance ToMathExpr E.TermExpr where
  toMathExpr (E.Term n [])  = toMathExpr n
  toMathExpr (E.Term 1 [x]) = toMathExpr x
  toMathExpr (E.Term 1 xs)  = Multiply (map toMathExpr xs)
  toMathExpr (E.Term n xs)  = Multiply (toMathExpr n : map toMathExpr xs)

instance ToMathExpr Integer where
  toMathExpr n | n < 0 = NegativeAtom (show (-n))
  toMathExpr n         = Atom (show n) []

instance {-# OVERLAPPING #-} ToMathExpr (E.SymbolExpr, Integer) where
  toMathExpr (x, 1) = toMathExpr x
  toMathExpr (x, n) = Power (toMathExpr x) (toMathExpr n)

instance ToMathExpr E.SymbolExpr where
  toMathExpr (E.Symbol _ (':':':':':':_) []) = Atom "#" []
  toMathExpr (E.Symbol _ s js) = toMathExpr' js (Atom s [])
    where
      toMathExpr' [] acc = acc
      toMathExpr' (E.User x:js) (Partial e ps) =
        toMathExpr' js (Partial e (ps ++ [toMathExpr x]))
      toMathExpr' (E.User x:js) e@Atom{} =
        toMathExpr' js (Partial e [toMathExpr x])
      toMathExpr' (j:js) (Atom e is) =
        toMathExpr' js (Atom e (is ++ [toMathIndex j]))
      toMathExpr' _ _ = undefined -- TODO

  toMathExpr (E.Apply fn mExprs) =
    case (toMathExpr fn, mExprs) of
      (Atom "^" [], [x, y]) -> Power (toMathExpr x) (toMathExpr y)
      _                     -> Func (toMathExpr fn) (map toMathExpr mExprs)
  toMathExpr (E.Quote mExpr) = Quote (toMathExpr mExpr)
  toMathExpr (E.FunctionData name _ _ js) = toMathExpr' js (FunctionSymbol (show name))
    where
      toMathExpr' [] acc = acc
      toMathExpr' (E.User x:js) (Partial e ps) =
        toMathExpr' js (Partial e (ps ++ [toMathExpr x]))
      toMathExpr' (E.User x:js) e@FunctionSymbol{} =
        toMathExpr' js (Partial e [toMathExpr x])
      toMathExpr' _ acc = undefined -- TODO

toMathIndex :: ToMathExpr a => E.Index a -> MathIndex
toMathIndex (E.Sub x) = Sub (toMathExpr x)
toMathIndex (E.Sup x) = Super (toMathExpr x)
toMathIndex _         = undefined -- TODO

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
parsePlus = try (string "(+") >> spaces >> Plus <$> parseList <* char ')'

parseMultiply :: Parser MathExpr
parseMultiply = try (string "(*") >> spaces >> Multiply <$> parseList <* char ')'

parseDiv :: Parser MathExpr
parseDiv = try (string "(/") >> spaces >> Div <$> parseExpr <*> (spaces >> parseExpr) <* char ')'

parseFunction :: Parser MathExpr
parseFunction = char '(' >> Func <$> parseAtom <* spaces <*> parseList <* char ')'

parseTensor :: Parser MathExpr
parseTensor = string "[|" >> spaces0 >> Tensor <$> parseList <* spaces0 <* string "|]" <*> many parseScript

parseTuple :: Parser MathExpr
parseTuple = char '[' >> Tuple <$> parseList <* char ']'

parseCollection :: Parser MathExpr
parseCollection = char '{' >> Collection <$> parseList <* char '}'

parseQuote :: Parser MathExpr
parseQuote = char '\'' >> Quote <$> parseExpr'

parseExpr' :: Parser MathExpr
parseExpr' = parseNegativeAtom
         <|> try parsePartial
         <|> parseAtom
         <|> parseQuote
         <|> parsePlus
         <|> parseMultiply
         <|> parseDiv
         <|> try parseFunction
         <|> try parseTensor
         <|> try parseTuple
         <|> try parseCollection

parseExpr :: Parser MathExpr
parseExpr = do
  x <- parseExpr'
  option x $ Power x <$> try (char '^' >> parseExpr')
