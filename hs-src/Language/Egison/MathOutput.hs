{- |
Module      : Language.Egison.MathOutput
Copyright   : Satoshi Egi
Licence     : MIT

This module provides utility functions.
-}

module Language.Egison.MathOutput (mathExprToAsciiMath, mathExprToLatex) where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

mathExprToAsciiMath :: String -> String
mathExprToAsciiMath input = case parse parseExpr "math-expr" input of
    Left err -> "[#f " ++ input ++ "]"
    Right val -> "[#t \"" ++ (showMathExpr val) ++ "\"]"

mathExprToLatex :: String -> String
mathExprToLatex input = case parse parseExpr "math-expr" input of
    Left err -> "[#f " ++ input ++ "]"
    Right val -> "[#t \"" ++ (showMathExpr val) ++ "\"]"

data MathExpr = Atom String
              | NegativeAtom String
              | Plus [MathExpr]
              | Multiply [MathExpr]
              | Power MathExpr MathExpr
              | Func MathExpr [MathExpr]
              | Tensor [MathExpr] [MathExpr] [MathExpr]
              deriving (Show, Eq) 

showMathExpr :: MathExpr -> String
showMathExpr (Atom func) = func
showMathExpr (NegativeAtom func) = "-" ++ func
showMathExpr (Plus []) = ""
showMathExpr (Plus [a]) = showMathExpr a
showMathExpr (Plus lvs) = case (lvs !! 1) of
                           NegativeAtom na -> (showMathExpr (head lvs)) ++ " - " ++ na ++ (showMathExpr (Plus $ tail $ tail lvs))
                           Plus (NegativeAtom na:rest) -> (showMathExpr (head lvs)) ++ " - " ++ na ++ " + " ++ (showMathExpr (Plus $ rest ++ (tail $ tail lvs)))
                           Multiply (NegativeAtom na:rest) -> (showMathExpr (head lvs)) ++ " - " ++ na ++ " " ++ (showMathExpr (Plus $ rest ++ (tail $ tail lvs)))
                           _ -> (showMathExpr (head lvs)) ++ " + " ++ (showMathExpr (Plus $ tail lvs))
showMathExpr (Multiply []) = ""
showMathExpr (Multiply [a]) = showMathExpr a
showMathExpr (Multiply lvs) = (showMathExpr' (head lvs)) ++ " " ++ (showMathExpr (Multiply (tail lvs)))
showMathExpr (Power lv1 lv2) = (showMathExpr lv1) ++ "^" ++ (showMathExpr lv2)
showMathExpr (Func f lvs) = case f of
                             Atom "/" -> if (length lvs) == 2 then "frac{" ++ (showMathExpr (lvs !! 0)) ++ "}{" ++ (showMathExpr (lvs !! 1)) ++ "}"
                                                         else (showMathExpr f) ++ "(" ++ (showMathExprArg lvs) ++ ")"
                             _ -> (showMathExpr f) ++ "(" ++ (showMathExprArg lvs) ++ ")"
showMathExpr (Tensor lvs us ds)
    | us == [] = "(" ++ (showMathExprArg lvs) ++ ")"
    | otherwise = "(" ++ (showMathExprArg lvs) ++ ")_(" ++ (showMathExprIndices us) ++ ")"

showMathExpr' :: MathExpr -> String
showMathExpr' (Plus lvs) = "(" ++ showMathExpr (Plus lvs) ++ ")"
showMathExpr' val = showMathExpr val

showMathExprArg :: [MathExpr] -> String
showMathExprArg [] = ""
showMathExprArg [a] = showMathExpr a
showMathExprArg lvs = (showMathExpr (head lvs)) ++ ", " ++ (showMathExprArg $ tail lvs)

showMathExprIndices :: [MathExpr] -> String
showMathExprIndices [a] = showMathExpr a
showMathExprIndices lvs = (showMathExpr (head lvs)) ++ (showMathExprIndices $ tail lvs)

spaces :: Parser ()
spaces = skipMany1 space

spaces0 :: Parser ()
spaces0 = skipMany space

symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=>?@#"

parseAtom :: Parser MathExpr
parseAtom = do first <- letter <|> symbol <|> digit
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ Atom atom

parseNegativeAtom :: Parser MathExpr
parseNegativeAtom = do
   char '-'
   first <- letter <|> symbol <|> digit
   rest <- many (letter <|> digit <|> symbol)
   let atom = [first] ++ rest
   return $ NegativeAtom atom

parseList :: Parser [MathExpr]
parseList = sepEndBy parseExpr spaces

parseUsList :: Parser [MathExpr]
parseUsList = sepEndBy parseExpr $ char '_'

parsePlus :: Parser MathExpr
parsePlus = do
    string "(+"
    spaces
    xs <- parseList
    char ')'
    return $ Plus xs

parseMultiply :: Parser MathExpr
parseMultiply = do
    string "(*"
    spaces
    xs <- parseList
    char ')'
    return $ Multiply xs

parseFunction :: Parser MathExpr
parseFunction = do
    char '('
    func <- parseAtom
    spaces
    xs <- parseList
    char ')'
    return $ Func func xs

parseTensor :: Parser MathExpr
parseTensor = do
    string "[|"
    spaces0
    xs <- parseList
    spaces0
    string "|]"
    option (Tensor xs [] []) $ try $ char '_' >> parseUsList >>= \us -> return $ Tensor xs us []
    
parseExpr' :: Parser MathExpr
parseExpr' = parseNegativeAtom
         <|> parseAtom
         <|> try parsePlus
         <|> try parseMultiply
         <|> try parseFunction
         <|> try parseTensor

parseExpr :: Parser MathExpr
parseExpr = do
    x <- parseExpr'
    option x  $ Power x <$> (try $ char '^' >> parseExpr')
