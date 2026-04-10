{-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Language.Egison.PrettyMath.AST
Licence     : MIT
-}

module Language.Egison.PrettyMath.AST
  ( MathValue(..)
  , MathIndex(..)
  , ToMathValue(..)
  , isSub
  , parseExpr
  ) where

import           Data.Foldable                 (toList)
import           Text.ParserCombinators.Parsec hiding (spaces)

import qualified Language.Egison.Data          as E
import qualified Language.Egison.IExpr         as E
import qualified Language.Egison.Math.CAS      as CAS

data MathValue
  = Atom String [MathIndex]
  | NegativeAtom String
  | Plus [MathValue]
  | Multiply [MathValue]
  | Div MathValue MathValue
  | Power MathValue MathValue
  | Func MathValue [MathValue]
  | Tensor [MathValue] [MathIndex]
  | Tuple [MathValue]
  | Collection [MathValue]
  | Quote MathValue
  | Partial MathValue [MathValue]
  deriving (Eq, Show)

data MathIndex
  = Super MathValue
  | Sub MathValue
  deriving (Eq, Show)

isSub :: MathIndex -> Bool
isSub (Sub _) = True
isSub _       = False


class ToMathValue a where
  toMathValue :: a -> MathValue

instance ToMathValue E.EgisonValue where
  toMathValue (E.CASData cv)    = toMathValue cv
  toMathValue (E.Tuple es)      = Tuple (map toMathValue es)
  toMathValue (E.Collection es) = Collection (map toMathValue (toList es))
  toMathValue (E.TensorData t)  = toMathValue t
  toMathValue e                 = Atom (show e) []

instance ToMathValue a => ToMathValue (E.Tensor a) where
  toMathValue (E.Scalar _)       = undefined
  toMathValue (E.Tensor [_] xs js) = Tensor (map toMathValue (toList xs)) (map toMathIndex js)
  toMathValue (E.Tensor [_, n] xs js) = Tensor (f (fromIntegral n) (map toMathValue (toList xs))) (map toMathIndex js)
    where
      f _ [] = []
      f n xs = Tensor (take n xs) [] : f n (drop n xs)
  toMathValue (E.Tensor _ _ _) = undefined

-- CASValue instances
instance ToMathValue CAS.CASValue where
  toMathValue (CAS.CASInteger n) = toMathValue n
  toMathValue (CAS.CASFactor sym) = toMathValue sym
  toMathValue (CAS.CASPoly []) = Atom "0" []
  toMathValue (CAS.CASPoly [t]) = toMathValue t
  toMathValue (CAS.CASPoly ts) = Plus (map toMathValue ts)
  toMathValue (CAS.CASFrac num (CAS.CASInteger 1)) = toMathValue num
  toMathValue (CAS.CASFrac num (CAS.CASPoly [CAS.CASTerm (CAS.CASInteger 1) []])) = toMathValue num
  toMathValue (CAS.CASFrac num denom) = Div (toMathValue num) (toMathValue denom)

instance ToMathValue CAS.CASTerm where
  toMathValue (CAS.CASTerm coeff []) = toMathValue coeff
  toMathValue (CAS.CASTerm (CAS.CASInteger 1) [x]) = toMathValue x
  toMathValue (CAS.CASTerm (CAS.CASInteger 1) xs) = Multiply (map toMathValue xs)
  toMathValue (CAS.CASTerm coeff xs) = Multiply (toMathValue coeff : map toMathValue xs)

instance ToMathValue Integer where
  toMathValue n | n < 0 = NegativeAtom (show (-n))
  toMathValue n         = Atom (show n) []

instance {-# OVERLAPPING #-} ToMathValue (CAS.SymbolExpr, Integer) where
  toMathValue (x, 1) = toMathValue x
  toMathValue (x, n) = Power (toMathValue x) (toMathValue n)

instance ToMathValue CAS.SymbolExpr where
  toMathValue (CAS.Symbol _ (':':':':':':_) []) = Atom "#" []
  toMathValue (CAS.Symbol _ s js) = toMathValue' js (Atom s [])
    where
      toMathValue' [] acc = acc
      toMathValue' (E.User x:js) (Partial e ps) =
        toMathValue' js (Partial e (ps ++ [toMathValue x]))
      toMathValue' (E.User x:js) e@Atom{} =
        toMathValue' js (Partial e [toMathValue x])
      toMathValue' (j:js) (Atom e is) =
        toMathValue' js (Atom e (is ++ [toMathIndex j]))
      toMathValue' _ _ = undefined -- TODO

  toMathValue (CAS.Apply1 fn a1) =
    case toMathValue fn of
      Atom "^" [] -> Power (toMathValue fn) (toMathValue a1)
      _           -> Func (toMathValue fn) [toMathValue a1]
  toMathValue (CAS.Apply2 fn a1 a2) =
    case toMathValue fn of
      Atom "^" [] -> Power (toMathValue a1) (toMathValue a2)
      _           -> Func (toMathValue fn) [toMathValue a1, toMathValue a2]
  toMathValue (CAS.Apply3 fn a1 a2 a3) =
    Func (toMathValue fn) [toMathValue a1, toMathValue a2, toMathValue a3]
  toMathValue (CAS.Apply4 fn a1 a2 a3 a4) =
    Func (toMathValue fn) [toMathValue a1, toMathValue a2, toMathValue a3, toMathValue a4]
  toMathValue (CAS.Quote mExpr) = Quote (toMathValue mExpr)
  toMathValue (CAS.QuoteFunction whnf) =
    case E.prettyFunctionName whnf of
      Just name -> Atom name []
      Nothing   -> Atom "f" []
  toMathValue (CAS.FunctionData (CAS.CASPoly [CAS.CASTerm (CAS.CASInteger 1) [(CAS.Symbol _ s js, 1)]]) _) = toMathValue' js (Atom s [])
    where
      toMathValue' [] acc = acc
      toMathValue' (E.User x:js) (Partial e ps) =
        toMathValue' js (Partial e (ps ++ [toMathValue x]))
      toMathValue' (E.User x:js) e@Atom{} =
        toMathValue' js (Partial e [toMathValue x])
      toMathValue' (j:js) (Atom e is) =
        toMathValue' js (Atom e (is ++ [toMathIndex j]))
      toMathValue' _ _ = undefined -- TODO
  toMathValue (CAS.FunctionData name _) = toMathValue name

toMathIndex :: ToMathValue a => E.Index a -> MathIndex
toMathIndex (E.Sub x) = Sub (toMathValue x)
toMathIndex (E.Sup x) = Super (toMathValue x)
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

parseAtom :: Parser MathValue
parseAtom = Atom <$> ((:) <$> (letter <|> symbol <|> digit) <*> many (letter <|> digit <|> symbol)) <*> many parseScript

parseAtom' :: Parser MathValue
parseAtom' = flip Atom [] <$> ((:) <$> (letter <|> symbol <|> digit) <*> many (letter <|> digit <|> symbol))

parsePartial :: Parser MathValue
parsePartial = Partial <$> parseAtom <*> many1 (char '|' >> parseAtom)

parseNegativeAtom :: Parser MathValue
parseNegativeAtom = char '-' >> NegativeAtom <$> ((:) <$> (letter <|> symbol <|> digit) <*> many (letter <|> digit <|> symbol))

parseList :: Parser [MathValue]
parseList = sepEndBy parseExpr spaces

parseScript :: Parser MathIndex
parseScript = Sub <$> (char '_' >> parseAtom')
              <|> Super <$> (char '~' >> parseAtom')

parsePlus :: Parser MathValue
parsePlus = try (string "(+") >> spaces >> Plus <$> parseList <* char ')'

parseMultiply :: Parser MathValue
parseMultiply = try (string "(*") >> spaces >> Multiply <$> parseList <* char ')'

parseDiv :: Parser MathValue
parseDiv = try (string "(/") >> spaces >> Div <$> parseExpr <*> (spaces >> parseExpr) <* char ')'

parseFunction :: Parser MathValue
parseFunction = char '(' >> Func <$> parseAtom <* spaces <*> parseList <* char ')'

parseTensor :: Parser MathValue
parseTensor = string "[|" >> spaces0 >> Tensor <$> parseList <* spaces0 <* string "|]" <*> many parseScript

parseTuple :: Parser MathValue
parseTuple = char '[' >> Tuple <$> parseList <* char ']'

parseCollection :: Parser MathValue
parseCollection = char '{' >> Collection <$> parseList <* char '}'

parseQuote :: Parser MathValue
parseQuote = char '\'' >> Quote <$> parseExpr'

parseExpr' :: Parser MathValue
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

parseExpr :: Parser MathValue
parseExpr = do
  x <- parseExpr'
  option x $ Power x <$> try (char '^' >> parseExpr')
