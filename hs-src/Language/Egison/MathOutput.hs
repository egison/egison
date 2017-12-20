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
    Right val -> "[#t \"" ++ (showMathExprAsciiMath val) ++ "\"]"

mathExprToLatex :: String -> String
mathExprToLatex input = case parse parseExpr "math-expr" input of
    Left err -> "[#f " ++ input ++ "]"
    Right val -> "[#t \"" ++ (showMathExprLatex val) ++ "\"]"

data MathExpr = Atom String
              | NegativeAtom String
              | Plus [MathExpr]
              | Multiply [MathExpr]
              | Power MathExpr MathExpr
              | Func MathExpr [MathExpr]
              | Tensor [MathExpr] [MathExpr] [MathExpr]
              deriving (Show, Eq) 

--
-- Show (AsciiMath)
--

showMathExprAsciiMath :: MathExpr -> String
showMathExprAsciiMath (Atom func) = func
showMathExprAsciiMath (NegativeAtom func) = "-" ++ func
showMathExprAsciiMath (Plus []) = ""
showMathExprAsciiMath (Plus [a]) = showMathExprAsciiMath a
showMathExprAsciiMath (Plus lvs) = case (lvs !! 1) of
                           NegativeAtom na -> (showMathExprAsciiMath (head lvs)) ++ " - " ++ na ++ (showMathExprAsciiMath (Plus $ tail $ tail lvs))
                           Plus (NegativeAtom na:rest) -> (showMathExprAsciiMath (head lvs)) ++ " - " ++ na ++ " + " ++ (showMathExprAsciiMath (Plus $ rest ++ (tail $ tail lvs)))
                           Multiply (NegativeAtom na:rest) -> (showMathExprAsciiMath (head lvs)) ++ " - " ++ na ++ " " ++ (showMathExprAsciiMath (Plus $ rest ++ (tail $ tail lvs)))
                           _ -> (showMathExprAsciiMath (head lvs)) ++ " + " ++ (showMathExprAsciiMath (Plus $ tail lvs))
showMathExprAsciiMath (Multiply []) = ""
showMathExprAsciiMath (Multiply [a]) = showMathExprAsciiMath a
showMathExprAsciiMath (Multiply lvs) = (showMathExprAsciiMath' (head lvs)) ++ " " ++ (showMathExprAsciiMath (Multiply (tail lvs)))
showMathExprAsciiMath (Power lv1 lv2) = (showMathExprAsciiMath lv1) ++ "^" ++ (showMathExprAsciiMath lv2)
showMathExprAsciiMath (Func f lvs) = case f of
                             Atom "/" -> if (length lvs) == 2 then "frac{" ++ (showMathExprAsciiMath (lvs !! 0)) ++ "}{" ++ (showMathExprAsciiMath (lvs !! 1)) ++ "}"
                                                         else (showMathExprAsciiMath f) ++ "(" ++ (showMathExprAsciiMathArg lvs) ++ ")"
                             _ -> (showMathExprAsciiMath f) ++ "(" ++ (showMathExprAsciiMathArg lvs) ++ ")"
showMathExprAsciiMath (Tensor lvs us ds)
    | us == [] = "(" ++ (showMathExprAsciiMathArg lvs) ++ ")"
    | otherwise = "(" ++ (showMathExprAsciiMathArg lvs) ++ ")_(" ++ (showMathExprAsciiMathIndices us) ++ ")"

showMathExprAsciiMath' :: MathExpr -> String
showMathExprAsciiMath' (Plus lvs) = "(" ++ showMathExprAsciiMath (Plus lvs) ++ ")"
showMathExprAsciiMath' val = showMathExprAsciiMath val

showMathExprAsciiMathArg :: [MathExpr] -> String
showMathExprAsciiMathArg [] = ""
showMathExprAsciiMathArg [a] = showMathExprAsciiMath a
showMathExprAsciiMathArg lvs = (showMathExprAsciiMath (head lvs)) ++ ", " ++ (showMathExprAsciiMathArg $ tail lvs)

showMathExprAsciiMathIndices :: [MathExpr] -> String
showMathExprAsciiMathIndices [a] = showMathExprAsciiMath a
showMathExprAsciiMathIndices lvs = (showMathExprAsciiMath (head lvs)) ++ (showMathExprAsciiMathIndices $ tail lvs)

--
-- Show (Latex)
--

showMathExprLatex :: MathExpr -> String
showMathExprLatex (Atom func) = func
showMathExprLatex (NegativeAtom func) = "-" ++ func
showMathExprLatex (Plus []) = ""
showMathExprLatex (Plus [a]) = showMathExprLatex a
showMathExprLatex (Plus lvs) = case (lvs !! 1) of
                           NegativeAtom na -> (showMathExprLatex (head lvs)) ++ " - " ++ na ++ (showMathExprLatex (Plus $ tail $ tail lvs))
                           Plus (NegativeAtom na:rest) -> (showMathExprLatex (head lvs)) ++ " - " ++ na ++ " + " ++ (showMathExprLatex (Plus $ rest ++ (tail $ tail lvs)))
                           Multiply (NegativeAtom na:rest) -> (showMathExprLatex (head lvs)) ++ " - " ++ na ++ " " ++ (showMathExprLatex (Plus $ rest ++ (tail $ tail lvs)))
                           _ -> (showMathExprLatex (head lvs)) ++ " + " ++ (showMathExprLatex (Plus $ tail lvs))
showMathExprLatex (Multiply []) = ""
showMathExprLatex (Multiply [a]) = showMathExprLatex a
showMathExprLatex (Multiply lvs) = (showMathExprLatex' (head lvs)) ++ " " ++ (showMathExprLatex (Multiply (tail lvs)))
showMathExprLatex (Power lv1 lv2) = (showMathExprLatex lv1) ++ "^" ++ (showMathExprLatex lv2)
showMathExprLatex (Func (Atom "sqrt") lvs) = "\\sqrt{" ++ (showMathExprLatexArg lvs) ++ "}"
showMathExprLatex (Func (Atom "/") lvs) = "\\frac{" ++ (showMathExprLatex (lvs !! 0)) ++ "}{" ++ (showMathExprLatex (lvs !! 1)) ++ "}"
showMathExprLatex (Func f lvs) = (showMathExprLatex f) ++ "(" ++ (showMathExprLatexArg lvs) ++ ")"
showMathExprLatex (Tensor lvs us ds)
    | us == [] = "(" ++ (showMathExprLatexArg lvs) ++ ")"
    | otherwise = "(" ++ (showMathExprLatexArg lvs) ++ ")_(" ++ (showMathExprLatexIndices us) ++ ")"

showMathExprLatex' :: MathExpr -> String
showMathExprLatex' (Plus lvs) = "(" ++ showMathExprLatex (Plus lvs) ++ ")"
showMathExprLatex' val = showMathExprLatex val

showMathExprLatexArg :: [MathExpr] -> String
showMathExprLatexArg [] = ""
showMathExprLatexArg [a] = showMathExprLatex a
showMathExprLatexArg lvs = (showMathExprLatex (head lvs)) ++ ", " ++ (showMathExprLatexArg $ tail lvs)

showMathExprLatexIndices :: [MathExpr] -> String
showMathExprLatexIndices [a] = showMathExprLatex a
showMathExprLatexIndices lvs = (showMathExprLatex (head lvs)) ++ (showMathExprLatexIndices $ tail lvs)

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
