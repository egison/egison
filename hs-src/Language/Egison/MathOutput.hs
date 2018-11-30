{- |
Module      : Language.Egison.MathOutput
Copyright   : Satoshi Egi
Licence     : MIT

This module provides utility functions.
-}

module Language.Egison.MathOutput (mathExprToHaskell, mathExprToAsciiMath, mathExprToLatex, mathExprToMathematica) where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

mathExprToHaskell :: String -> String
mathExprToHaskell input = case parse parseExpr "math-expr" input of
                            Left err -> input
                            Right val -> "#haskell|" ++ show val ++ "|#"

mathExprToAsciiMath :: String -> String
mathExprToAsciiMath input = case parse parseExpr "math-expr" input of
                              Left err -> input
                              Right val -> "#asciimath|" ++ showMathExprAsciiMath val ++ "|#"

mathExprToLatex :: String -> String
mathExprToLatex input = case parse parseExpr "math-expr" input of
                          Left err -> input
                          Right val -> "#latex|" ++ showMathExprLatex val ++ "|#"

mathExprToMathematica :: String -> String
mathExprToMathematica input = case parse parseExpr "math-expr" input of
                          Left err -> input
                          Right val -> "#mathematica|" ++ showMathExprMathematica val ++ "|#"

data MathExpr = Atom String [MathIndex]
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

data MathIndex = Super MathExpr
              | Sub MathExpr
              deriving (Eq, Show)

--
-- Show (AsciiMath)
--

showMathExprAsciiMath :: MathExpr -> String
showMathExprAsciiMath (Atom func []) = func
showMathExprAsciiMath (NegativeAtom func) = "-" ++ func
showMathExprAsciiMath (Plus []) = ""
showMathExprAsciiMath (Plus (x:xs)) = showMathExprAsciiMath x ++ showMathExprAsciiMathForPlus xs
 where
  showMathExprAsciiMathForPlus :: [MathExpr] -> String
  showMathExprAsciiMathForPlus [] = ""
  showMathExprAsciiMathForPlus ((NegativeAtom a):xs) = " - " ++ a ++ showMathExprAsciiMathForPlus xs
  showMathExprAsciiMathForPlus ((Multiply (NegativeAtom "1":ys)):xs) = " - " ++ showMathExprAsciiMath (Multiply ys) ++ showMathExprAsciiMathForPlus xs
  showMathExprAsciiMathForPlus ((Multiply (NegativeAtom a:ys)):xs) = " - " ++ showMathExprAsciiMath (Multiply ((Atom a []):ys)) ++ " " ++ showMathExprAsciiMathForPlus xs
  showMathExprAsciiMathForPlus (x:xs) = " + " ++ showMathExprAsciiMath x ++ showMathExprAsciiMathForPlus xs
showMathExprAsciiMath (Multiply []) = ""
showMathExprAsciiMath (Multiply [a]) = showMathExprAsciiMath a
showMathExprAsciiMath (Multiply (NegativeAtom "1":lvs)) = "-" ++ showMathExprAsciiMath (Multiply lvs)
showMathExprAsciiMath (Multiply lvs) = showMathExprAsciiMath' (head lvs) ++ " " ++ showMathExprAsciiMath (Multiply (tail lvs))
showMathExprAsciiMath (Power lv1 lv2) = showMathExprAsciiMath lv1 ++ "^" ++ showMathExprAsciiMath lv2
showMathExprAsciiMath (Func f lvs) = case f of
                                       Atom "/" [] -> if length lvs == 2 then "frac{" ++ showMathExprAsciiMath (head lvs) ++ "}{" ++ showMathExprAsciiMath (lvs !! 1) ++ "}"
                                                                        else showMathExprAsciiMath f ++ "(" ++ showMathExprAsciiMathArg lvs ++ ")"
                                       _ -> showMathExprAsciiMath f ++ "(" ++ showMathExprAsciiMathArg lvs ++ ")"
showMathExprAsciiMath (Tensor lvs mis)
  | null mis = "(" ++ showMathExprAsciiMathArg lvs ++ ")"
  | not (any isSub mis) = "(" ++ showMathExprAsciiMathArg lvs ++ ")^(" ++ showMathExprAsciiMathIndices mis ++ ")"
  | not (any (not . isSub) mis) = "(" ++ showMathExprAsciiMathArg lvs ++ ")_(" ++ showMathExprAsciiMathIndices mis ++ ")"
  | otherwise = "(" ++ showMathExprAsciiMathArg lvs ++ ")_(" ++ showMathExprAsciiMathIndices (filter isSub mis) ++ ")^(" ++ showMathExprAsciiMathIndices (filter (not . isSub) mis) ++ ")"
showMathExprAsciiMath (Tuple lvs) = "(" ++ showMathExprAsciiMathArg lvs ++ ")"
showMathExprAsciiMath (Collection lvs) = "{" ++ showMathExprAsciiMathArg lvs ++ "}"
showMathExprAsciiMath (Exp x) = "e^(" ++ showMathExprAsciiMath x ++ ")"

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
showMathExprAsciiMathArg lvs = showMathExprAsciiMath (head lvs) ++ ", " ++ (showMathExprAsciiMathArg (tail lvs))

showMathExprAsciiMathIndices :: [MathIndex] -> String
showMathExprAsciiMathIndices [a] = showMathIndexAsciiMath a
showMathExprAsciiMathIndices lvs = showMathIndexAsciiMath (head lvs) ++ showMathExprAsciiMathIndices (tail lvs)

showMathIndexAsciiMath :: MathIndex -> String
showMathIndexAsciiMath (Super a) = showMathExprAsciiMath a
showMathIndexAsciiMath (Sub a) = showMathExprAsciiMath a

--
-- Show (Latex)
--

showMathExprLatex :: MathExpr -> String
showMathExprLatex (Atom a []) = a
showMathExprLatex (Atom a xs) = a ++ showMathExprLatexScript xs
showMathExprLatex (Partial f xs) = "\\frac{" ++ convertToPartial (f, length xs) ++ "}{" ++ showPartial xs ++ "}"
                                         where showPartial :: [MathExpr] -> String
                                               showPartial xs = let lx = elemCount xs in convertToPartial2 (head lx) ++ foldr (\x acc -> " " ++ convertToPartial2 x ++ acc) "" (tail lx)

                                               convertToPartial :: (MathExpr, Int) -> String
                                               convertToPartial (x, 1) = "\\partial " ++ showMathExprLatex x
                                               convertToPartial (x, n) = "\\partial^" ++ show n ++ " " ++ showMathExprLatex x

                                               convertToPartial2 :: (MathExpr, Int) -> String
                                               convertToPartial2 (x, 1) = "\\partial " ++ showMathExprLatex x
                                               convertToPartial2 (x, n) = "\\partial " ++ showMathExprLatex x ++ "^"  ++ show n
showMathExprLatex (NegativeAtom a) = "-" ++ a
showMathExprLatex (Plus []) = ""
showMathExprLatex (Plus (x:xs)) = showMathExprLatex x ++ showMathExprLatexForPlus xs
 where
  showMathExprLatexForPlus :: [MathExpr] -> String
  showMathExprLatexForPlus [] = ""
  showMathExprLatexForPlus ((NegativeAtom a):xs) = " - " ++ a ++ showMathExprLatexForPlus xs
  showMathExprLatexForPlus ((Multiply (NegativeAtom "1":ys)):xs) = " - " ++ showMathExprLatex (Multiply ys) ++ showMathExprLatexForPlus xs
  showMathExprLatexForPlus ((Multiply (NegativeAtom a:ys)):xs) = " - " ++ showMathExprLatex (Multiply ((Atom a []):ys)) ++ showMathExprLatexForPlus xs
  showMathExprLatexForPlus (x:xs) = " + " ++  showMathExprLatex x ++ showMathExprLatexForPlus xs
showMathExprLatex (Multiply []) = ""
showMathExprLatex (Multiply [x]) = showMathExprLatex x
showMathExprLatex (Multiply (Atom "1" []:xs)) = showMathExprLatex (Multiply xs)
showMathExprLatex (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathExprLatex (Multiply xs)
showMathExprLatex (Multiply (x:xs)) = showMathExprLatex' x ++ " " ++ showMathExprLatex (Multiply xs)
showMathExprLatex (Power lv1 lv2) = showMathExprLatex lv1 ++ "^" ++ showMathExprLatex lv2
showMathExprLatex (Func (Atom "sqrt" []) [x]) = "\\sqrt{" ++ showMathExprLatex x ++ "}"
showMathExprLatex (Func (Atom "rt" []) [x, y]) = "\\sqrt[" ++ showMathExprLatex x ++ "]{" ++ showMathExprLatex y ++ "}"
showMathExprLatex (Func (Atom "/" []) [x, y]) = "\\frac{" ++ showMathExprLatex x ++ "}{" ++ showMathExprLatex y ++ "}"
showMathExprLatex (Func f xs) = showMathExprLatex f ++ "(" ++ showMathExprLatexArg xs ", " ++ ")"
showMathExprLatex (Tensor xs mis) = case head xs of
                                       Tensor _ _ -> "\\begin{pmatrix} " ++ showMathExprLatexVectors xs ++ "\\end{pmatrix}" ++ showMathExprLatexScript mis
                                       _ -> "\\begin{pmatrix} " ++ showMathExprLatexVectors xs ++ "\\end{pmatrix}" ++ showMathExprLatexScript mis
showMathExprLatex (Tuple xs) = "(" ++ showMathExprLatexArg xs ", " ++ ")"
showMathExprLatex (Collection xs) = "\\{" ++ showMathExprLatexArg xs ", " ++ "\\}"
showMathExprLatex (Exp x) = "e^{" ++ showMathExprLatex x ++ "}"
showMathExprLatex (Quote x) = "(" ++ showMathExprLatex x ++ ")"

showMathExprLatex' :: MathExpr -> String
showMathExprLatex' (Plus xs) = "(" ++ showMathExprLatex (Plus xs) ++ ")"
showMathExprLatex' x = showMathExprLatex x

showMathExprLatexArg :: [MathExpr] -> String -> String
showMathExprLatexArg [] _ = ""
showMathExprLatexArg [a] _ = showMathExprLatex a
showMathExprLatexArg lvs s = showMathExprLatex (head lvs) ++ s ++ showMathExprLatexArg  (tail lvs) s

showMathExprLatexSuper :: MathIndex -> String
showMathExprLatexSuper (Super (Atom "#" [])) = "\\#"
showMathExprLatexSuper (Super x) = showMathExprLatex x
showMathExprLatexSuper (Sub x) = "\\;"

showMathExprLatexSub :: MathIndex -> String
showMathExprLatexSub (Sub (Atom "#" [])) = "\\#"
showMathExprLatexSub (Sub x) = showMathExprLatex x
showMathExprLatexSub (Super x) = "\\;"

showMathExprLatexScript :: [MathIndex] -> String
showMathExprLatexScript [] = ""
showMathExprLatexScript is = "_{" ++ concat (map showMathExprLatexSub is) ++ "}^{" ++ concat (map showMathExprLatexSuper is) ++ "}"

showMathExprLatexVectors :: [MathExpr] -> String
showMathExprLatexVectors [] = ""
showMathExprLatexVectors (Tensor lvs []:r) = showMathExprLatexArg lvs " & " ++ " \\\\ " ++ showMathExprLatexVectors r
showMathExprLatexVectors lvs = showMathExprLatexArg lvs " \\\\ " ++ "\\\\ "

--
-- Show (Mathematica)
--

showMathExprMathematica :: MathExpr -> String
showMathExprMathematica (Atom a []) = a
showMathExprMathematica (Partial f xs) = undefined
showMathExprMathematica (NegativeAtom a) = "-" ++ a
showMathExprMathematica (Plus []) = ""
showMathExprMathematica (Plus (x:xs)) = showMathExprMathematica x ++ showMathExprMathematicaForPlus xs
 where
  showMathExprMathematicaForPlus :: [MathExpr] -> String
  showMathExprMathematicaForPlus [] = ""
  showMathExprMathematicaForPlus ((NegativeAtom a):xs) = " - " ++ a ++ showMathExprMathematicaForPlus xs
  showMathExprMathematicaForPlus ((Multiply (NegativeAtom "1":ys)):xs) = " - " ++ showMathExprMathematica (Multiply ys) ++ showMathExprMathematicaForPlus xs
  showMathExprMathematicaForPlus ((Multiply (NegativeAtom a:ys)):xs) = " - " ++ showMathExprMathematica (Multiply ((Atom a []):ys)) ++ showMathExprMathematicaForPlus xs
  showMathExprMathematicaForPlus (x:xs) = " + " ++  showMathExprMathematica x ++ showMathExprMathematicaForPlus xs
showMathExprMathematica (Multiply []) = ""
showMathExprMathematica (Multiply [x]) = showMathExprMathematica x
showMathExprMathematica (Multiply (Atom "1" []:xs)) = showMathExprMathematica (Multiply xs)
showMathExprMathematica (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathExprMathematica (Multiply xs)
showMathExprMathematica (Multiply (x:xs)) = showMathExprMathematica' x ++ " " ++ showMathExprMathematica (Multiply xs)
showMathExprMathematica (Power lv1 lv2) = showMathExprMathematica lv1 ++ "^" ++ showMathExprMathematica lv2
showMathExprMathematica (Func (Atom "sqrt" []) [x]) = "Sqrt[" ++ showMathExprMathematica x ++ "]"
showMathExprMathematica (Func (Atom "rt" []) [x, y]) = "Surd[" ++ showMathExprMathematica x ++ "," ++ showMathExprMathematica y ++ "]"
showMathExprMathematica (Func (Atom "/" []) [x, y]) = "(" ++ showMathExprMathematica x ++ ")/(" ++ showMathExprMathematica y ++ ")"
showMathExprMathematica (Func f xs) = showMathExprMathematica f ++ "(" ++ showMathExprMathematicaArg xs ++ ")"
showMathExprMathematica (Tensor lvs mis)
  | null mis = "{" ++ showMathExprMathematicaArg lvs ++ "}"
  | not (any isSub mis) = "{" ++ showMathExprMathematicaArg lvs ++ "}^(" ++ showMathExprMathematicaIndices mis ++ ")"
  | not (any (not . isSub) mis) = "{" ++ showMathExprMathematicaArg lvs ++ "}_(" ++ showMathExprMathematicaIndices mis ++ ")"
  | otherwise = "{" ++ showMathExprMathematicaArg lvs ++ "}_(" ++ showMathExprMathematicaIndices (filter isSub mis) ++ ")^(" ++ showMathExprMathematicaIndices (filter (not . isSub) mis) ++ ")"
showMathExprMathematica (Tuple xs) = "(" ++ showMathExprMathematicaArg xs ++ ")"
showMathExprMathematica (Collection xs) = "{" ++ showMathExprMathematicaArg xs ++ "}"
showMathExprMathematica (Exp x) = "e^(" ++ showMathExprMathematica x ++ ")"
showMathExprMathematica (Quote x) = "(" ++ showMathExprMathematica x ++ ")"

showMathExprMathematica' :: MathExpr -> String
showMathExprMathematica' (Plus xs) = "(" ++ showMathExprMathematica (Plus xs) ++ ")"
showMathExprMathematica' x = showMathExprMathematica x

showMathExprMathematicaArg :: [MathExpr] -> String
showMathExprMathematicaArg [] = ""
showMathExprMathematicaArg [a] = showMathExprMathematica a
showMathExprMathematicaArg lvs = showMathExprMathematica (head lvs) ++ ", " ++ (showMathExprMathematicaArg (tail lvs))

showMathExprMathematicaIndices :: [MathIndex] -> String
showMathExprMathematicaIndices [a] = showMathIndexMathematica a
showMathExprMathematicaIndices lvs = showMathIndexMathematica (head lvs) ++ showMathExprMathematicaIndices (tail lvs)

showMathIndexMathematica :: MathIndex -> String
showMathIndexMathematica (Super a) = showMathExprMathematica a
showMathIndexMathematica (Sub a) = showMathExprMathematica a


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
    let atom = first : rest
    ys <- many parseScript
    return $ Atom atom ys

parseAtom' :: Parser MathExpr
parseAtom' = do
    first <- letter <|> symbol <|> digit
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ Atom atom []

parsePartial :: Parser MathExpr
parsePartial = do
    xs <- parseAtom
    is <- many1 (char '|' >> parseAtom)
    return $ Partial xs is

parseNegativeAtom :: Parser MathExpr
parseNegativeAtom = do
    char '-'
    first <- letter <|> symbol <|> digit
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ NegativeAtom atom

parseList :: Parser [MathExpr]
parseList = sepEndBy parseExpr spaces

parseScript :: Parser MathIndex
parseScript = (Sub <$> (char '_' >> parseAtom')) <|> (Super <$> (char '~' >> parseAtom'))

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
    ys <- many  parseScript
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
    x <- parseExpr
    char ')'
    return $ Exp x

parseQuote :: Parser MathExpr
parseQuote = do
    char '\''
    x <- parseExpr'
    return $ Quote x

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

elemCount :: Eq a => [a] -> [(a, Int)]
elemCount [] = []
elemCount (x:xs) = (x, (length $ filter (== x) xs) + 1) : elemCount (filter (/= x) xs)
