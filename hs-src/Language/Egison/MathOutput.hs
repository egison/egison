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
              | Tensor [MathExpr] [MathIndex]
              | Tuple [MathExpr]
              | Collection [MathExpr]
              | Exp MathExpr
              deriving (Eq) 

data MathIndex = Super MathExpr
              | Sub MathExpr
              deriving (Eq)

--
-- Show (AsciiMath)
--

showMathIndexAsciiMath :: MathIndex -> String
showMathIndexAsciiMath (Super a) = showMathExprAsciiMath a
showMathIndexAsciiMath (Sub a) = showMathExprAsciiMath a

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
showMathExprAsciiMath (Multiply ((NegativeAtom "1"):lvs)) = "-" ++ (showMathExprAsciiMath (Multiply lvs))
showMathExprAsciiMath (Multiply lvs) = (showMathExprAsciiMath' (head lvs)) ++ " " ++ (showMathExprAsciiMath (Multiply (tail lvs)))
showMathExprAsciiMath (Power lv1 lv2) = (showMathExprAsciiMath lv1) ++ "^" ++ (showMathExprAsciiMath lv2)
showMathExprAsciiMath (Func f lvs) = case f of
                                       Atom "/" -> if (length lvs) == 2 then "frac{" ++ (showMathExprAsciiMath (lvs !! 0)) ++ "}{" ++ (showMathExprAsciiMath (lvs !! 1)) ++ "}"
                                                                        else (showMathExprAsciiMath f) ++ "(" ++ (showMathExprAsciiMathArg lvs) ++ ")"
                                       _ -> (showMathExprAsciiMath f) ++ "(" ++ (showMathExprAsciiMathArg lvs) ++ ")"
showMathExprAsciiMath (Tensor lvs mis)
  | mis == [] = "(" ++ (showMathExprAsciiMathArg lvs) ++ ")"
  | filter isSub mis == [] = "(" ++ (showMathExprAsciiMathArg lvs) ++ ")^(" ++ (showMathExprAsciiMathIndices mis) ++ ")"
  | filter (not . isSub) mis == [] = "(" ++ (showMathExprAsciiMathArg lvs) ++ ")_(" ++ (showMathExprAsciiMathIndices mis) ++ ")"
  | otherwise = "(" ++ (showMathExprAsciiMathArg lvs) ++ ")_(" ++ (showMathExprAsciiMathIndices (filter isSub mis)) ++ ")^(" ++ (showMathExprAsciiMathIndices (filter (not . isSub) mis)) ++ ")"
showMathExprAsciiMath (Tuple lvs) = "(" ++ showMathExprAsciiMathArg lvs ++ ")"
showMathExprAsciiMath (Collection lvs) = "{" ++ showMathExprAsciiMathArg lvs ++ "}"
showMathExprAsciiMath (Exp x) = "e^" ++ showMathExprAsciiMath x

isSub :: MathIndex -> Bool
isSub x = case x of
            Sub _ -> True
            _ -> False

showMathExprAsciiMath' :: MathExpr -> String
showMathExprAsciiMath' (Plus lvs) = "(" ++ showMathExprAsciiMath (Plus lvs) ++ ")"
showMathExprAsciiMath' val = showMathExprAsciiMath val

showMathExprAsciiMathArg :: [MathExpr] -> String
showMathExprAsciiMathArg [] = ""
showMathExprAsciiMathArg [a] = showMathExprAsciiMath a
showMathExprAsciiMathArg lvs = (showMathExprAsciiMath (head lvs)) ++ ", " ++ (showMathExprAsciiMathArg $ tail lvs)

showMathExprAsciiMathIndices :: [MathIndex] -> String
showMathExprAsciiMathIndices [a] = showMathIndexAsciiMath a
showMathExprAsciiMathIndices lvs = (showMathIndexAsciiMath (head lvs)) ++ (showMathExprAsciiMathIndices $ tail lvs)

--
-- Show (Latex)
--

showMathIndexLatex :: MathIndex -> String
showMathIndexLatex (Super a) = showMathExprLatex a
showMathIndexLatex (Sub a) = showMathExprLatex a

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
showMathExprLatex (Multiply ((NegativeAtom "1"):lvs)) = "-" ++ (showMathExprLatex (Multiply lvs))
showMathExprLatex (Multiply lvs) = (showMathExprLatex' (head lvs)) ++ " " ++ (showMathExprLatex (Multiply (tail lvs)))
showMathExprLatex (Power lv1 lv2) = (showMathExprLatex lv1) ++ "^" ++ (showMathExprLatex lv2)
showMathExprLatex (Func (Atom "sqrt") [x]) = "\\sqrt{" ++ (showMathExprLatex x) ++ "}"
showMathExprLatex (Func (Atom "rt") [x, y]) = "\\sqrt[" ++ (showMathExprLatex x) ++ "]{" ++ (showMathExprLatex y) ++ "}"
showMathExprLatex (Func (Atom "/") [x, y]) = "\\frac{" ++ (showMathExprLatex x) ++ "}{" ++ (showMathExprLatex y) ++ "}"
showMathExprLatex (Func f lvs) = (showMathExprLatex f) ++ "(" ++ (showMathExprLatexArg lvs ", ") ++ ")"
showMathExprLatex (Tensor lvs mis) = case (head lvs) of
                                       Tensor _ _ -> "\\begin{pmatrix} " ++ showMathExprLatexVectors lvs ++ "\\end{pmatrix}" ++ showMathExprLatexScript mis
                                       _ -> "\\begin{pmatrix} " ++ showMathExprLatexVectors lvs ++ "\\end{pmatrix}" ++ showMathExprLatexScript mis
showMathExprLatex (Tuple lvs) = "(" ++ showMathExprLatexArg lvs ", " ++ ")"
showMathExprLatex (Collection lvs) = "{" ++ showMathExprLatexArg lvs ", " ++ "}"
showMathExprLatex (Exp x) = "e^" ++ showMathExprLatex x

showMathExprLatex' :: MathExpr -> String
showMathExprLatex' (Plus lvs) = "(" ++ showMathExprLatex (Plus lvs) ++ ")"
showMathExprLatex' val = showMathExprLatex val

showMathExprLatexArg :: [MathExpr] -> String -> String
showMathExprLatexArg [] _ = ""
showMathExprLatexArg [a] _ = showMathExprLatex a
showMathExprLatexArg lvs s = (showMathExprLatex (head lvs)) ++ s ++ (showMathExprLatexArg  (tail lvs) s)

showMathExprLatexIndices :: [MathIndex] -> String
showMathExprLatexIndices [a] = showMathIndexLatex a
showMathExprLatexIndices lvs = (showMathIndexLatex (head lvs)) ++ (showMathExprLatexIndices $ tail lvs)

showMathExprLatexScript :: [MathIndex] -> String
showMathExprLatexScript [] = ""
showMathExprLatexScript lvs = if (isSub (head lvs)) then let (a, b) = span isSub lvs in "_{" ++ (showMathExprLatexIndices a) ++ "}" ++ showMathExprLatexScript b
                                                             else let (a, b) = span (not . isSub) lvs in "^{" ++ (showMathExprLatexIndices a) ++ "}" ++ showMathExprLatexScript b

showMathExprLatexVectors :: [MathExpr] -> String
showMathExprLatexVectors [] = ""
showMathExprLatexVectors ((Tensor lvs []):r) = showMathExprLatexArg lvs " & " ++ " \\\\ " ++ showMathExprLatexVectors r
showMathExprLatexVectors lvs = showMathExprLatexArg lvs " \\\\ " ++ "\\\\ "

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
parseAtom = do 
    first <- letter <|> symbol <|> digit
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

parseScript :: Parser MathIndex
parseScript = (char '_' >> parseExpr >>= return . Sub) <|> (char '~' >> parseExpr >>= return . Super)

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
    ys <- many $ parseScript
    return $ Tensor xs ys

parseTuple :: Parser MathExpr
parseTuple = do
    char '['
    xs <- parseList
    char ']'
    return $ Tuple xs

parseCollection :: Parser MathExpr
parseCollection = do
    char '{'
    xs <- parseList
    char '}'
    return $ Collection xs

parseExp :: Parser MathExpr
parseExp = do
    string "(exp"
    spaces
    x <- parseAtom
    char ')'
    return $ Exp x

parseExpr' :: Parser MathExpr
parseExpr' = parseNegativeAtom
         <|> parseAtom
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
    option x  $ Power x <$> (try $ char '^' >> parseExpr')
