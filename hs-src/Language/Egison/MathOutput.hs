{- |
Module      : Language.Egison.MathOutput
Copyright   : Satoshi Egi
Licence     : MIT

This module provides utility functions.
-}

module Language.Egison.MathOutput (mathExprToHaskell, mathExprToAsciiMath, mathExprToLatex, mathExprToMathematica, mathExprToMaxima) where

import           Control.Monad
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

mathExprToHaskell :: String -> String
mathExprToHaskell input = case parse parseExpr "math-expr" input of
                            Left err  -> input
                            Right val -> "#haskell|" ++ show val ++ "|#"

mathExprToAsciiMath :: String -> String
mathExprToAsciiMath input = case parse parseExpr "math-expr" input of
                              Left err -> input
                              Right val -> case showMathExprAsciiMath val of
                                             "undefined" -> "undefined"
                                             output -> "#asciimath|" ++ output ++ "|#"

mathExprToLatex :: String -> String
mathExprToLatex input = case parse parseExpr "math-expr" input of
                          Left err -> input
                          Right val -> case showMathExprLatex val of
                                         "undefined" -> "undefined"
                                         output -> "#latex|" ++ output ++ "|#"

mathExprToMathematica :: String -> String
mathExprToMathematica input = case parse parseExpr "math-expr" input of
                                Left err -> input
                                Right val -> case showMathExprMathematica val of
                                               "undefined" -> "undefined"
                                               output -> "#mathematica|" ++ output ++ "|#"

mathExprToMaxima :: String -> String
mathExprToMaxima input = case parse parseExpr "math-expr" input of
                          Left err -> input
                          Right val -> case showMathExprMaxima val of
                                         "undefined" -> "undefined"
                                         output -> "#maxima|" ++ output ++ "|#"

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
  showMathExprAsciiMathForPlus (NegativeAtom a:xs) = " - " ++ a ++ showMathExprAsciiMathForPlus xs
  showMathExprAsciiMathForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathExprAsciiMath (Multiply ys) ++ showMathExprAsciiMathForPlus xs
  showMathExprAsciiMathForPlus (Multiply (NegativeAtom a:ys):xs) = " - " ++ showMathExprAsciiMath (Multiply (Atom a []:ys)) ++ " " ++ showMathExprAsciiMathForPlus xs
  showMathExprAsciiMathForPlus (x:xs) = " + " ++ showMathExprAsciiMath x ++ showMathExprAsciiMathForPlus xs
showMathExprAsciiMath (Multiply []) = ""
showMathExprAsciiMath (Multiply [a]) = showMathExprAsciiMath a
showMathExprAsciiMath (Multiply (NegativeAtom "1":lvs)) = "-" ++ showMathExprAsciiMath (Multiply lvs)
showMathExprAsciiMath (Multiply lvs) = showMathExprAsciiMath' (head lvs) ++ " " ++ showMathExprAsciiMath (Multiply (tail lvs))
showMathExprAsciiMath (Power lv1 lv2) = showMathExprAsciiMath lv1 ++ "^" ++ showMathExprAsciiMath lv2
showMathExprAsciiMath (Func (Atom "sqrt" []) [x]) = "sqrt " ++ showMathExprAsciiMath x
showMathExprAsciiMath (Func (Atom "rt" []) [x, y]) = "root " ++ showMathExprAsciiMath x ++ " " ++ showMathExprAsciiMath y
showMathExprAsciiMath (Func (Atom "/" []) [x, y]) = "frac{" ++ showMathExprAsciiMath x ++ "}{" ++ showMathExprAsciiMath y ++ "}"
showMathExprAsciiMath (Func f lvs) = showMathExprAsciiMath f ++ "(" ++ showMathExprAsciiMathArg lvs ++ ")"
showMathExprAsciiMath (Tensor lvs mis)
  | null mis = "(" ++ showMathExprAsciiMathArg lvs ++ ")"
  | not (any isSub mis) = "(" ++ showMathExprAsciiMathArg lvs ++ ")^(" ++ showMathExprAsciiMathIndices mis ++ ")"
  | not (any (not . isSub) mis) = "(" ++ showMathExprAsciiMathArg lvs ++ ")_(" ++ showMathExprAsciiMathIndices mis ++ ")"
  | otherwise = "(" ++ showMathExprAsciiMathArg lvs ++ ")_(" ++ showMathExprAsciiMathIndices (filter isSub mis) ++ ")^(" ++ showMathExprAsciiMathIndices (filter (not . isSub) mis) ++ ")"
showMathExprAsciiMath (Tuple lvs) = "(" ++ showMathExprAsciiMathArg lvs ++ ")"
showMathExprAsciiMath (Collection lvs) = "{" ++ showMathExprAsciiMathArg lvs ++ "}"
showMathExprAsciiMath (Exp x) = "e^(" ++ showMathExprAsciiMath x ++ ")"

isSub :: MathIndex -> Bool
isSub (Sub _) = True
isSub _       = False

showMathExprAsciiMath' :: MathExpr -> String
showMathExprAsciiMath' (Plus lvs) = "(" ++ showMathExprAsciiMath (Plus lvs) ++ ")"
showMathExprAsciiMath' val = showMathExprAsciiMath val

showMathExprAsciiMathArg :: [MathExpr] -> String
showMathExprAsciiMathArg [] = ""
showMathExprAsciiMathArg [a] = showMathExprAsciiMath a
showMathExprAsciiMathArg lvs = showMathExprAsciiMath (head lvs) ++ ", " ++ showMathExprAsciiMathArg (tail lvs)

showMathExprAsciiMathIndices :: [MathIndex] -> String
showMathExprAsciiMathIndices [a] = showMathIndexAsciiMath a
showMathExprAsciiMathIndices lvs = showMathIndexAsciiMath (head lvs) ++ showMathExprAsciiMathIndices (tail lvs)

showMathIndexAsciiMath :: MathIndex -> String
showMathIndexAsciiMath (Super a) = showMathExprAsciiMath a
showMathIndexAsciiMath (Sub a)   = showMathExprAsciiMath a

--
-- Show (Latex)
--

showMathExprLatex :: MathExpr -> String
showMathExprLatex (Atom a []) = a
showMathExprLatex (Atom a xs) = a ++ showMathExprLatexScript xs
showMathExprLatex (Partial f xs) = "\\frac{" ++ convertToPartial (f, length xs) ++ "}{" ++ showPartial xs ++ "}"
 where
  showPartial :: [MathExpr] -> String
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
  showMathExprLatexForPlus (NegativeAtom a:xs) = " - " ++ a ++ showMathExprLatexForPlus xs
  showMathExprLatexForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathExprLatex (Multiply ys) ++ showMathExprLatexForPlus xs
  showMathExprLatexForPlus (Multiply (NegativeAtom a:ys):xs) = " - " ++ showMathExprLatex (Multiply (Atom a []:ys)) ++ showMathExprLatexForPlus xs
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
showMathExprLatex (Tensor xs mis) = "\\begin{pmatrix} " ++ showMathExprLatexVectors xs ++ "\\end{pmatrix}" ++ showMathExprLatexScript mis
showMathExprLatex (Tuple xs) = "(" ++ showMathExprLatexArg xs ", " ++ ")"
showMathExprLatex (Collection xs) = "\\{" ++ showMathExprLatexArg xs ", " ++ "\\}"
showMathExprLatex (Exp x) = "e^{" ++ showMathExprLatex x ++ "}"
showMathExprLatex (Quote x) = "(" ++ showMathExprLatex x ++ ")"

showMathExprLatex' :: MathExpr -> String
showMathExprLatex' (Plus xs) = "(" ++ showMathExprLatex (Plus xs) ++ ")"
showMathExprLatex' x         = showMathExprLatex x

showMathExprLatexArg :: [MathExpr] -> String -> String
showMathExprLatexArg [] _ = ""
showMathExprLatexArg [a] _ = showMathExprLatex a
showMathExprLatexArg lvs s = showMathExprLatex (head lvs) ++ s ++ showMathExprLatexArg  (tail lvs) s

showMathExprLatexSuper :: MathIndex -> String
showMathExprLatexSuper (Super (Atom "#" [])) = "\\#"
showMathExprLatexSuper (Super x)             = showMathExprLatex x
showMathExprLatexSuper (Sub x)               = "\\;"

showMathExprLatexSub :: MathIndex -> String
showMathExprLatexSub (Sub (Atom "#" [])) = "\\#"
showMathExprLatexSub (Sub x)             = showMathExprLatex x
showMathExprLatexSub (Super x)           = "\\;"

showMathExprLatexScript :: [MathIndex] -> String
showMathExprLatexScript [] = ""
showMathExprLatexScript is = "_{" ++ concatMap showMathExprLatexSub is ++ "}^{" ++ concatMap showMathExprLatexSuper is ++ "}"

showMathExprLatexVectors :: [MathExpr] -> String
showMathExprLatexVectors [] = ""
showMathExprLatexVectors (Tensor lvs []:r) = showMathExprLatexArg lvs " & " ++ " \\\\ " ++ showMathExprLatexVectors r
showMathExprLatexVectors lvs = showMathExprLatexArg lvs " \\\\ " ++ "\\\\ "

--
-- Show (Mathematica)
--

showMathExprMathematica :: MathExpr -> String
showMathExprMathematica (Atom a []) = a
showMathExprMathematica (Partial f xs) = showMathExprMathematica f ++ "_" ++ (showMathExprsMathematica "_" xs)
showMathExprMathematica (NegativeAtom a) = "-" ++ a
showMathExprMathematica (Plus []) = ""
showMathExprMathematica (Plus (x:xs)) = showMathExprMathematica x ++ showMathExprMathematicaForPlus xs
 where
  showMathExprMathematicaForPlus :: [MathExpr] -> String
  showMathExprMathematicaForPlus [] = ""
  showMathExprMathematicaForPlus (NegativeAtom a:xs) = " - " ++ a ++ showMathExprMathematicaForPlus xs
  showMathExprMathematicaForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathExprMathematica (Multiply ys) ++ showMathExprMathematicaForPlus xs
  showMathExprMathematicaForPlus (Multiply (NegativeAtom a:ys):xs) = " - " ++ showMathExprMathematica (Multiply (Atom a []:ys)) ++ showMathExprMathematicaForPlus xs
  showMathExprMathematicaForPlus (x:xs) = " + " ++  showMathExprMathematica x ++ showMathExprMathematicaForPlus xs
showMathExprMathematica (Multiply []) = ""
showMathExprMathematica (Multiply [x]) = showMathExprMathematica x
showMathExprMathematica (Multiply (Atom "1" []:xs)) = showMathExprMathematica (Multiply xs)
showMathExprMathematica (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathExprMathematica (Multiply xs)
showMathExprMathematica (Multiply (x:xs)) = showMathExprMathematica' x ++ " " ++ showMathExprMathematica (Multiply xs)
showMathExprMathematica (Power lv1 lv2) = showMathExprMathematica lv1 ++ "^" ++ showMathExprMathematica lv2
showMathExprMathematica (Func (Atom "sqrt" []) [x]) = "Sqrt[" ++ showMathExprMathematica x ++ "]"
showMathExprMathematica (Func (Atom "rt" []) [x, y]) = "Surd[" ++ showMathExprMathematica x ++ "," ++ showMathExprMathematica y ++ "]"
showMathExprMathematica (Func (Atom "/" []) [x, y]) = addBracket x ++ "/" ++ addBracket y
 where
   addBracket x@(Atom _ []) = showMathExprMathematica x
   addBracket x             = "(" ++ showMathExprMathematica x ++ ")"
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

showMathExprsMathematica :: String -> [MathExpr] -> String
showMathExprsMathematica _ [] = ""
showMathExprsMathematica _ [a] = showMathExprMathematica a
showMathExprsMathematica s lvs = showMathExprMathematica (head lvs) ++ s ++ showMathExprsMathematica s (tail lvs)

showMathExprMathematicaArg :: [MathExpr] -> String
showMathExprMathematicaArg xs = showMathExprsMathematica ", " xs

showMathExprMathematicaIndices :: [MathIndex] -> String
showMathExprMathematicaIndices [a] = showMathIndexMathematica a
showMathExprMathematicaIndices lvs = showMathIndexMathematica (head lvs) ++ showMathExprMathematicaIndices (tail lvs)

showMathIndexMathematica :: MathIndex -> String
showMathIndexMathematica (Super a) = showMathExprMathematica a
showMathIndexMathematica (Sub a)   = showMathExprMathematica a

--
-- Show (Maxima)
--

showMathExprMaxima :: MathExpr -> String
showMathExprMaxima (Atom a []) = a
showMathExprMaxima (Partial f xs) = "undefined"
showMathExprMaxima (NegativeAtom a) = "-" ++ a
showMathExprMaxima (Plus []) = ""
showMathExprMaxima (Plus (x:xs)) = showMathExprMaxima x ++ showMathExprMaximaForPlus xs
 where
  showMathExprMaximaForPlus :: [MathExpr] -> String
  showMathExprMaximaForPlus [] = ""
  showMathExprMaximaForPlus (NegativeAtom a:xs) = " - " ++ a ++ showMathExprMaximaForPlus xs
  showMathExprMaximaForPlus (Multiply (NegativeAtom "1":ys):xs) = " - " ++ showMathExprMaxima (Multiply ys) ++ showMathExprMaximaForPlus xs
  showMathExprMaximaForPlus (Multiply (NegativeAtom a:ys):xs) = " - " ++ showMathExprMaxima (Multiply (Atom a []:ys)) ++ showMathExprMaximaForPlus xs
  showMathExprMaximaForPlus (x:xs) = " + " ++  showMathExprMaxima x ++ showMathExprMaximaForPlus xs
showMathExprMaxima (Multiply []) = ""
showMathExprMaxima (Multiply [x]) = showMathExprMaxima x
showMathExprMaxima (Multiply (Atom "1" []:xs)) = showMathExprMaxima (Multiply xs)
showMathExprMaxima (Multiply (NegativeAtom "1":xs)) = "-" ++ showMathExprMaxima (Multiply xs)
showMathExprMaxima (Multiply (x:xs)) = showMathExprMaxima' x ++ " * " ++ showMathExprMaxima (Multiply xs)
showMathExprMaxima (Power lv1 lv2) = showMathExprMaxima lv1 ++ "^" ++ showMathExprMaxima lv2
showMathExprMaxima (Func (Atom "sqrt" []) [x]) = "sqrt(" ++ showMathExprMaxima x ++ ")"
showMathExprMaxima (Func (Atom "rt" []) [x, y]) = showMathExprMaxima y ++ "^(1/" ++ showMathExprMaxima x ++ ")"
showMathExprMaxima (Func (Atom "/" []) [x, y]) = addBracket x ++ "/" ++ addBracket y
 where
   addBracket x@(Atom _ []) = showMathExprMaxima x
   addBracket x             = "(" ++ showMathExprMaxima x ++ ")"
showMathExprMaxima (Func f xs) = showMathExprMaxima f ++ "(" ++ showMathExprMaximaArg xs ++ ")"
showMathExprMaxima (Tensor lvs mis) = "undefined"
showMathExprMaxima (Tuple xs) = "undefined"
showMathExprMaxima (Collection xs) = "[" ++ showMathExprMaximaArg xs ++ "]"
showMathExprMaxima (Exp x) = "exp(" ++ showMathExprMaxima x ++ ")"
showMathExprMaxima (Quote x) = "(" ++ showMathExprMaxima x ++ ")"

showMathExprMaxima' :: MathExpr -> String
showMathExprMaxima' (Plus xs) = "(" ++ showMathExprMaxima (Plus xs) ++ ")"
showMathExprMaxima' x         = showMathExprMaxima x

showMathExprMaximaArg :: [MathExpr] -> String
showMathExprMaximaArg [] = ""
showMathExprMaximaArg [Tensor lvs []] = "undefined"
showMathExprMaximaArg [a] = showMathExprMaxima a
showMathExprMaximaArg lvs = showMathExprMaxima (head lvs) ++ ", " ++ showMathExprMaximaArg (tail lvs)


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

elemCount :: Eq a => [a] -> [(a, Int)]
elemCount [] = []
elemCount (x:xs) = (x, length (filter (== x) xs) + 1) : elemCount (filter (/= x) xs)
