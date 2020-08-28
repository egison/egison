{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}
{-# OPTIONS_GHC -Wno-all      #-} -- Since we will soon deprecate this parser

{- |
Module      : Language.Egison.Parser.SExpr
Licence     : MIT

This module provides Egison parser.
-}

module Language.Egison.Parser.SExpr
       (
       -- * Parse a string
         parseTopExprs
       , parseTopExpr
       , parseExprs
       , parseExpr
       ) where

import           Control.Applicative     (pure, (*>), (<$>), (<*), (<*>))
import           Control.Monad.Except    (throwError)
import           Control.Monad.Identity  (Identity)

import           Data.Char               (isLower, isUpper, toUpper)
import           Data.Either
import           Data.Functor            (($>))
import           Data.List.Split         (splitOn)
import           Data.Ratio
import qualified Data.Set                as Set
import qualified Data.Text               as T

import           Text.Parsec
import           Text.Parsec.String
import qualified Text.Parsec.Token       as P

import           Language.Egison.AST
import           Language.Egison.Data

parseTopExprs :: String -> Either EgisonError [TopExpr]
parseTopExprs = doParse $ do
  ret <- whiteSpace >> endBy topExpr whiteSpace
  eof
  return ret

parseTopExpr :: String -> Either EgisonError TopExpr
parseTopExpr = doParse $ do
  ret <- whiteSpace >> topExpr
  whiteSpace >> eof
  return ret

parseExprs :: String -> Either EgisonError [Expr]
parseExprs = doParse $ do
  ret <- whiteSpace >> endBy expr whiteSpace
  eof
  return ret

parseExpr :: String -> Either EgisonError Expr
parseExpr = doParse $ do
  ret <- whiteSpace >> expr
  whiteSpace >> eof
  return ret

--
-- Parser
--

doParse :: Parser a -> String -> Either EgisonError a
doParse p input = either (throwError . fromParsecError) return $ parse p "egison" input
  where
    fromParsecError :: ParseError -> EgisonError
    fromParsecError = Parser . show

doParse' :: Parser a -> String -> a
doParse' p input = case doParse p input of
                     Right x -> x

--
-- Expressions
--
topExpr :: Parser TopExpr
topExpr = try (Test <$> expr)
      <|> try defineExpr
      <|> try (parens (testExpr
                   <|> executeExpr
                   <|> loadFileExpr
                   <|> loadExpr))
      <?> "top-level expression"

defineExpr :: Parser TopExpr
defineExpr = try (parens (keywordDefine >> Define <$> (char '$' >> identVar) <*> expr))
         <|> try (parens (keywordDefine >> DefineWithIndices <$> (char '$' >> identVarWithIndices) <*> expr))

testExpr :: Parser TopExpr
testExpr = keywordTest >> Test <$> expr

executeExpr :: Parser TopExpr
executeExpr = keywordExecute >> Execute <$> expr

loadFileExpr :: Parser TopExpr
loadFileExpr = keywordLoadFile >> LoadFile <$> stringLiteral

loadExpr :: Parser TopExpr
loadExpr = keywordLoad >> Load <$> stringLiteral

expr :: Parser Expr
expr = P.lexeme lexer (do expr0 <- expr' <|> quoteExpr
                          expr1 <- option expr0 $ try (string "..." >> IndexedExpr False expr0 <$> parseindex)
                                                  <|> IndexedExpr True expr0 <$> parseindex
                          option expr1 $ (\x -> makeApply "**" [expr1, x]) <$> try (char '^' >> expr'))
                            where parseindex :: Parser [Index Expr]
                                  parseindex = many1 (try (MultiSubscript   <$> (char '_' >> expr') <*> (string "..._" >> expr'))
                                                  <|> try (MultiSuperscript <$> (char '~' >> expr') <*> (string "...~" >> expr'))
                                                  <|> try (Subscript    <$> (char '_' >> expr'))
                                                  <|> try (Superscript  <$> (char '~' >> expr'))
                                                  <|> try (SupSubscript <$> (string "~_" >> expr'))
                                                  <|> try (Userscript   <$> (char '|' >> expr')))


quoteExpr :: Parser Expr
quoteExpr = char '\'' >> QuoteExpr <$> expr'

expr' :: Parser Expr
expr' = try anonParamFuncExpr
            <|> try (ConstantExpr <$> constantExpr)
            <|> try anonParamExpr
            <|> try freshVarExpr
            <|> try varExpr
            <|> inductiveDataExpr
            <|> try vectorExpr
            <|> try tupleExpr
            <|> try hashExpr
            <|> collectionExpr
            <|> quoteSymbolExpr
            <|> wedgeExpr
            <|> parens (ifExpr
                        <|> lambdaExpr
                        <|> memoizedLambdaExpr
                        <|> cambdaExpr
                        <|> patternFunctionExpr
                        <|> letRecExpr
                        <|> letExpr
                        <|> letStarExpr
                        <|> withSymbolsExpr
                        <|> doExpr
                        <|> ioExpr
                        <|> matchAllExpr
                        <|> matchAllDFSExpr
                        <|> matchExpr
                        <|> matchDFSExpr
                        <|> matchAllLambdaExpr
                        <|> matchLambdaExpr
                        <|> matcherExpr
                        <|> seqExpr
                        <|> applyExpr
                        <|> cApplyExpr
                        <|> algebraicDataMatcherExpr
                        <|> generateTensorExpr
                        <|> tensorExpr
                        <|> tensorContractExpr
                        <|> tensorMapExpr
                        <|> tensorMap2Expr
                        <|> transposeExpr
                        <|> subrefsExpr
                        <|> suprefsExpr
                        <|> userrefsExpr
                        <|> functionWithArgExpr
                        )
            <?> "expression"

varExpr :: Parser Expr
varExpr = VarExpr <$> identVarWithoutIndex

freshVarExpr :: Parser Expr
freshVarExpr = char '#' >> return FreshVarExpr

inductiveDataExpr :: Parser Expr
inductiveDataExpr = angles $ do
  name <- upperName
  args <- sepEndBy expr whiteSpace
  return $ ApplyExpr (stringToVarExpr name) (TupleExpr args)

tupleExpr :: Parser Expr
tupleExpr = brackets $ TupleExpr <$> sepEndBy expr whiteSpace

data InnerExpr
  = ElementExpr Expr
  | SubCollectionExpr Expr

collectionExpr :: Parser Expr
collectionExpr = do
  inners <- braces $ sepEndBy innerExpr whiteSpace
  return $ f [] inners
 where
  innerExpr :: Parser InnerExpr
  innerExpr = (char '@' >> SubCollectionExpr <$> expr)
               <|> ElementExpr <$> expr

  isElementExpr :: InnerExpr -> Bool
  isElementExpr ElementExpr{} = True
  isElementExpr _             = False

  f :: [Expr] -> [InnerExpr] -> Expr
  f xs [] = CollectionExpr xs
  f xs [ElementExpr y] = CollectionExpr (xs ++ [y])
  f []  [SubCollectionExpr y] = y
  f [x] [SubCollectionExpr y] = ConsExpr x y
  f xs  [SubCollectionExpr y] = JoinExpr (CollectionExpr xs) y
  f xs (ElementExpr y : ys) = f (xs ++ [y]) ys
  f []  (SubCollectionExpr y : ys) = JoinExpr y (f [] ys)
  f [x] (SubCollectionExpr y : ys) = ConsExpr x (JoinExpr y (f [] ys))
  f xs  (SubCollectionExpr y : ys) = JoinExpr (CollectionExpr xs) (JoinExpr y (f [] ys))


vectorExpr :: Parser Expr
vectorExpr = between lp rp $ VectorExpr <$> sepEndBy expr whiteSpace
  where
    lp = P.lexeme lexer (string "[|")
    rp = string "|]"

hashExpr :: Parser Expr
hashExpr = between lp rp $ HashExpr <$> sepEndBy pairExpr whiteSpace
  where
    lp = P.lexeme lexer (string "{|")
    rp = string "|}"
    pairExpr :: Parser (Expr, Expr)
    pairExpr = brackets $ (,) <$> expr <*> expr

wedgeExpr :: Parser Expr
wedgeExpr = do
  e <- char '!' >> expr
  case e of
    ApplyExpr e1 e2 -> return $ WedgeApplyExpr e1 e2

functionWithArgExpr :: Parser Expr
functionWithArgExpr = keywordFunction >> FunctionExpr <$> between lp rp (sepEndBy expr whiteSpace)
  where
    lp = P.lexeme lexer (char '[')
    rp = char ']'

quoteSymbolExpr :: Parser Expr
quoteSymbolExpr = char '`' >> QuoteSymbolExpr <$> expr

matchAllExpr :: Parser Expr
matchAllExpr = keywordMatchAll >> MatchAllExpr BFSMode <$> expr <*> expr <*> (((:[]) <$> matchClause) <|> matchClauses)

matchAllDFSExpr :: Parser Expr
matchAllDFSExpr = keywordMatchAllDFS >> MatchAllExpr DFSMode <$> expr <*> expr <*> (((:[]) <$> matchClause) <|> matchClauses)

matchExpr :: Parser Expr
matchExpr = keywordMatch >> MatchExpr BFSMode <$> expr <*> expr <*> matchClauses

matchDFSExpr :: Parser Expr
matchDFSExpr = keywordMatchDFS >> MatchExpr DFSMode <$> expr <*> expr <*> matchClauses

matchAllLambdaExpr :: Parser Expr
matchAllLambdaExpr = keywordMatchAllLambda >> MatchAllLambdaExpr <$> expr <*> (((:[]) <$> matchClause) <|> matchClauses)

matchLambdaExpr :: Parser Expr
matchLambdaExpr = keywordMatchLambda >> MatchLambdaExpr <$> expr <*> matchClauses

matchClauses :: Parser [MatchClause]
matchClauses = braces $ sepEndBy matchClause whiteSpace

matchClause :: Parser MatchClause
matchClause = brackets $ (,) <$> pattern <*> expr

matcherExpr :: Parser Expr
matcherExpr = keywordMatcher >> MatcherExpr <$> ppMatchClauses

ppMatchClauses :: Parser [PatternDef]
ppMatchClauses = braces $ sepEndBy ppMatchClause whiteSpace

ppMatchClause :: Parser PatternDef
ppMatchClause = brackets $ (,,) <$> ppPattern <*> expr <*> pdMatchClauses

pdMatchClauses :: Parser [(PrimitiveDataPattern, Expr)]
pdMatchClauses = braces $ sepEndBy pdMatchClause whiteSpace

pdMatchClause :: Parser (PrimitiveDataPattern, Expr)
pdMatchClause = brackets $ (,) <$> pdPattern <*> expr

ppPattern :: Parser PrimitivePatPattern
ppPattern = P.lexeme lexer (ppWildCard
                        <|> ppPatVar
                        <|> ppValuePat
                        <|> ppInductivePat
                        <|> ppTuplePat
                        <?> "primitive-pattren-pattern")

ppWildCard :: Parser PrimitivePatPattern
ppWildCard = reservedOp "_" $> PPWildCard

ppPatVar :: Parser PrimitivePatPattern
ppPatVar = reservedOp "$" $> PPPatVar

ppValuePat :: Parser PrimitivePatPattern
ppValuePat = reservedOp ",$" >> PPValuePat <$> ident

ppInductivePat :: Parser PrimitivePatPattern
ppInductivePat = angles (PPInductivePat <$> lowerName <*> sepEndBy ppPattern whiteSpace)

ppTuplePat :: Parser PrimitivePatPattern
ppTuplePat = brackets $ PPTuplePat <$> sepEndBy ppPattern whiteSpace

pdPattern :: Parser PrimitiveDataPattern
pdPattern = P.lexeme lexer pdPattern'

pdPattern' :: Parser PrimitiveDataPattern
pdPattern' = reservedOp "_" $> PDWildCard
                    <|> (char '$' >> PDPatVar <$> identVar)
                    <|> braces ((PDConsPat <$> pdPattern <*> (char '@' *> pdPattern))
                            <|> (PDSnocPat <$> (char '@' *> pdPattern) <*> pdPattern)
                            <|> pure PDEmptyPat)
                    <|> angles (PDInductivePat <$> upperName <*> sepEndBy pdPattern whiteSpace)
                    <|> brackets (PDTuplePat <$> sepEndBy pdPattern whiteSpace)
                    <|> PDConstantPat <$> constantExpr
                    <?> "primitive-data-pattern"

ifExpr :: Parser Expr
ifExpr = keywordIf >> IfExpr <$> expr <*> expr <*> expr

lambdaExpr :: Parser Expr
lambdaExpr = keywordLambda >> LambdaExpr Nothing <$> argNames <*> expr

memoizedLambdaExpr :: Parser Expr
memoizedLambdaExpr = keywordMemoizedLambda >> MemoizedLambdaExpr <$> varNames <*> expr

memoizeFrame :: Parser [(Expr, Expr, Expr)]
memoizeFrame = braces $ sepEndBy memoizeBinding whiteSpace

memoizeBinding :: Parser (Expr, Expr, Expr)
memoizeBinding = brackets $ (,,) <$> expr <*> expr <*> expr

cambdaExpr :: Parser Expr
cambdaExpr = keywordCambda >> char '$' >> CambdaExpr <$> ident <*> expr

patternFunctionExpr :: Parser Expr
patternFunctionExpr = keywordPatternFunction >> PatternFunctionExpr <$> varNames <*> pattern

letRecExpr :: Parser Expr
letRecExpr =  keywordLetRec >> LetRecExpr <$> bindings <*> expr

letExpr :: Parser Expr
letExpr = keywordLet >> LetExpr <$> bindings <*> expr

letStarExpr :: Parser Expr
letStarExpr = keywordLetStar >> LetRecExpr <$> bindings <*> expr

withSymbolsExpr :: Parser Expr
withSymbolsExpr = keywordWithSymbols >> WithSymbolsExpr <$> braces (sepEndBy ident whiteSpace) <*> expr

doExpr :: Parser Expr
doExpr = keywordDo >> DoExpr <$> statements <*> option (ApplyExpr (stringToVarExpr "return") (TupleExpr [])) expr

statements :: Parser [BindingExpr]
statements = braces $ sepEndBy statement whiteSpace

statement :: Parser BindingExpr
statement = try binding
        <|> try (brackets ((PDTuplePat [],) <$> expr))
        <|> ((PDTuplePat [],) <$> expr)

bindings :: Parser [BindingExpr]
bindings = braces $ sepEndBy binding whiteSpace

binding :: Parser BindingExpr
binding = brackets $ (,) <$> varNames' <*> expr

varNames :: Parser [String]
varNames = return <$> (char '$' >> ident)
            <|> brackets (sepEndBy (char '$' >> ident) whiteSpace)

varNames' :: Parser PrimitiveDataPattern
varNames' = PDPatVar <$> (char '$' >> identVar)
        <|> PDTuplePat <$> brackets (sepEndBy (PDPatVar <$> (char '$' >> identVar)) whiteSpace)

argNames :: Parser [Arg]
argNames = return <$> argName
            <|> brackets (sepEndBy argName whiteSpace)

argName :: Parser Arg
argName = try (ScalarArg <$> (char '$' >> ident))
      <|> try (InvertedScalarArg <$> (string "*$" >> ident))
      <|> try (TensorArg <$> (char '%' >> ident))

ioExpr :: Parser Expr
ioExpr = keywordIo >> IoExpr <$> expr

seqExpr :: Parser Expr
seqExpr = keywordSeq >> SeqExpr <$> expr <*> expr

cApplyExpr :: Parser Expr
cApplyExpr = keywordCApply >> CApplyExpr <$> expr <*> expr

applyExpr :: Parser Expr
applyExpr = do
  func <- expr
  args <- sepEndBy arg whiteSpace
  let vars = lefts args
  case vars of
    [] -> return . ApplyExpr func . TupleExpr $ rights args
    _ | all null vars ->
        let n = toInteger (length vars)
            args' = f args 1
         in return $ AnonParamFuncExpr n $ ApplyExpr func (TupleExpr args')
      | all (not . null) vars ->
        let ns = Set.fromList $ map read vars
            n = Set.size ns
        in if Set.findMin ns == 1 && Set.findMax ns == n
             then
               let args' = map g args
                in return $ AnonParamFuncExpr (toInteger n) $ ApplyExpr func (TupleExpr args')
             else fail "invalid anonymous parameter function"
      | otherwise -> fail "invalid anonymous parameter function"
 where
  arg = try (Right <$> expr)
         <|> char '$' *> (Left <$> option "" index)
  index = (:) <$> satisfy (\c -> '1' <= c && c <= '9') <*> many digit
  f [] _                   = []
  f (Left _ : args) n      = AnonParamExpr n : f args (n + 1)
  f (Right expr : args) n  = expr : f args n
  g (Left arg)   = AnonParamExpr (read arg)
  g (Right expr) = expr

anonParamFuncExpr :: Parser Expr
anonParamFuncExpr = (AnonParamFuncExpr . read <$> index) <*> (char '#' >> expr)
 where
  index = (:) <$> satisfy (\c -> '1' <= c && c <= '9') <*> many digit

anonParamExpr :: Parser Expr
anonParamExpr = char '%' >> AnonParamExpr <$> integerLiteral

algebraicDataMatcherExpr :: Parser Expr
algebraicDataMatcherExpr = keywordAlgebraicDataMatcher
                                >> braces (AlgebraicDataMatcherExpr <$> sepEndBy1 inductivePat' whiteSpace)
  where
    inductivePat' :: Parser (String, [Expr])
    inductivePat' = angles $ (,) <$> lowerName <*> sepEndBy expr whiteSpace

generateTensorExpr :: Parser Expr
generateTensorExpr = keywordGenerateTensor >> GenerateTensorExpr <$> expr <*> expr

tensorExpr :: Parser Expr
tensorExpr = keywordTensor >> TensorExpr <$> expr <*> expr

tensorContractExpr :: Parser Expr
tensorContractExpr = keywordTensorContract >> TensorContractExpr <$> expr
--tensorContractExpr = keywordTensorContract >> TensorContractExpr <$> expr <*> expr

tensorMapExpr :: Parser Expr
tensorMapExpr = keywordTensorMap >> TensorMapExpr <$> expr <*> expr

tensorMap2Expr :: Parser Expr
tensorMap2Expr = keywordTensorMap2 >> TensorMap2Expr <$> expr <*> expr <*> expr

transposeExpr :: Parser Expr
transposeExpr = keywordTranspose >> TransposeExpr <$> expr <*> expr

subrefsExpr :: Parser Expr
subrefsExpr = (keywordSubrefs >> SubrefsExpr False <$> expr <*> expr)
               <|> (keywordSubrefsNew >> SubrefsExpr True <$> expr <*> expr)

suprefsExpr :: Parser Expr
suprefsExpr = (keywordSuprefs >> SuprefsExpr False <$> expr <*> expr)
               <|> (keywordSuprefsNew >> SuprefsExpr True <$> expr <*> expr)

userrefsExpr :: Parser Expr
userrefsExpr = (keywordUserrefs >> UserrefsExpr False <$> expr <*> expr)
                <|> (keywordUserrefsNew >> UserrefsExpr True <$> expr <*> expr)

-- Patterns

pattern :: Parser Pattern
pattern = P.lexeme lexer (do pattern <- pattern'
                             option pattern $ IndexedPat pattern <$> many1 (try $ char '_' >> expr'))

pattern' :: Parser Pattern
pattern' = wildCard
            <|> contPat
            <|> patVar
            <|> varPat
            <|> valuePat
            <|> predPat
            <|> notPat
            <|> tuplePat
            <|> inductivePat
            <|> laterPatVar
            <|> try seqNilPat
            <|> try seqConsPat
            <|> try seqPat
            <|> parens (andPat
                    <|> notPat'
                    <|> orPat
                    <|> loopPat
                    <|> letPat
                    <|> try dApplyPat
                    <|> try pApplyPat
                    )

pattern'' :: Parser Pattern
pattern'' = wildCard
            <|> patVar
            <|> valuePat

wildCard :: Parser Pattern
wildCard = reservedOp "_" >> pure WildCard

patVar :: Parser Pattern
patVar = char '$' >> PatVar <$> identVarWithoutIndex

varPat :: Parser Pattern
varPat = VarPat <$> ident

valuePat :: Parser Pattern
valuePat = char ',' >> ValuePat <$> expr

predPat :: Parser Pattern
predPat = char '?' >> PredPat <$> expr

letPat :: Parser Pattern
letPat = keywordLet >> LetPat <$> bindings <*> pattern

notPat :: Parser Pattern
notPat = char '!' >> NotPat <$> pattern

notPat' :: Parser Pattern
notPat' = keywordNot >> NotPat <$> pattern

tuplePat :: Parser Pattern
tuplePat = brackets $ TuplePat <$> sepEndBy pattern whiteSpace

inductivePat :: Parser Pattern
inductivePat = angles $ InductivePat <$> lowerName <*> sepEndBy pattern whiteSpace

contPat :: Parser Pattern
contPat = keywordCont >> pure ContPat

andPat :: Parser Pattern
andPat = do
  pats <- (reservedOp "&" <|> keywordAnd) >> sepEndBy pattern whiteSpace
  case pats of
    [] -> return WildCard
    _  -> return $ foldr1 AndPat pats

orPat :: Parser Pattern
orPat = do
  pats <- (reservedOp "|" <|> keywordOr) >> sepEndBy pattern whiteSpace
  case pats of
    [] -> return (NotPat WildCard)
    _  -> return $ foldr1 OrPat pats

pApplyPat :: Parser Pattern
pApplyPat = PApplyPat <$> expr <*> sepEndBy pattern whiteSpace

dApplyPat :: Parser Pattern
dApplyPat = DApplyPat <$> pattern'' <*> sepEndBy pattern whiteSpace

loopPat :: Parser Pattern
loopPat = keywordLoop >> char '$' >> LoopPat <$> identVarWithoutIndex <*> loopRange <*> pattern <*> option (NotPat WildCard) pattern

loopRange :: Parser LoopRange
loopRange = brackets (try (LoopRange <$> expr <*> expr <*> option WildCard pattern)
                      <|> (do s <- expr
                              ep <- option WildCard pattern
                              return (LoopRange s (ApplyExpr (stringToVarExpr "from") (ApplyExpr (stringToVarExpr "-'") (TupleExpr [s, ConstantExpr (IntegerExpr 1)]))) ep)))

seqNilPat :: Parser Pattern
seqNilPat = braces $ pure SeqNilPat

seqConsPat :: Parser Pattern
seqConsPat = braces $ SeqConsPat <$> pattern <*> (char '@' >> pattern)

seqPat :: Parser Pattern
seqPat = braces $ do
  pats <- sepEndBy pattern whiteSpace
  tailPat <- option SeqNilPat (char '@' >> pattern)
  return $ foldr SeqConsPat tailPat pats

laterPatVar :: Parser Pattern
laterPatVar = char '#' >> pure LaterPatVar

-- Constants

constantExpr :: Parser ConstantExpr
constantExpr = StringExpr . T.pack <$> stringLiteral
                 <|> BoolExpr <$> boolLiteral
                 <|> try (CharExpr <$> oneChar)
                 <|> try (FloatExpr <$> positiveFloatLiteral)
                 <|> try (IntegerExpr <$> integerLiteral)
                 <|> (keywordSomething $> SomethingExpr)
                 <|> (keywordUndefined $> UndefinedExpr)
                 <?> "constant"

positiveFloatLiteral :: Parser Double
positiveFloatLiteral = do
  n <- integerLiteral
  char '.'
  mStr <- many1 digit
  let m = read mStr
  let l = m % (10 ^ fromIntegral (length mStr))
  if n < 0 then return (fromRational (fromIntegral n - l) :: Double)
           else return (fromRational (fromIntegral n + l) :: Double)

--
-- Tokens
--

egisonDef :: P.GenLanguageDef String () Identity
egisonDef =
  P.LanguageDef { P.commentStart       = "#|"
                , P.commentEnd         = "|#"
                , P.commentLine        = ";"
                , P.identStart         = letter <|> symbol1 <|> symbol0
                , P.identLetter        = letter <|> digit <|> symbol2
                , P.opStart            = symbol1
                , P.opLetter           = symbol1
                , P.reservedNames      = reservedKeywords
                , P.reservedOpNames    = reservedOperators
                , P.nestedComments     = True
                , P.caseSensitive      = True }

symbol0 = char '^'
-- Don't allow three consecutive dots to be a part of identifier
symbol1 = oneOf "+-*/=∂∇" <|> try (char '.' <* notFollowedBy (string ".."))
symbol2 = symbol1 <|> oneOf "'!?₀₁₂₃₄₅₆₇₈₉"

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser egisonDef

reservedKeywords :: [String]
reservedKeywords =
  [ "define"
  , "set!"
  , "test"
  , "execute"
  , "load-file"
  , "load"
  , "if"
  , "seq"
  , "capply"
  , "lambda"
  , "memoized-lambda"
  , "memoize"
  , "cambda"
  , "pattern-function"
  , "letrec"
  , "let"
  , "let*"
  , "with-symbols"
--  , "not"
--  , "and"
--  , "or"
  , "loop"
  , "match-all"
  , "match"
  , "match-all-dfs"
  , "match-dfs"
  , "match-all-lambda"
  , "match-lambda"
  , "matcher"
  , "do"
  , "io"
  , "algebraic-data-matcher"
  , "generate-tensor"
  , "tensor"
  , "contract"
  , "tensor-map"
  , "tensor-map2"
  , "transpose"
  , "subrefs"
  , "subrefs!"
  , "suprefs"
  , "suprefs!"
  , "user-refs"
  , "user-refs!"
  , "function"
  , "something"
  , "undefined"]

reservedOperators :: [String]
reservedOperators =
  [ "$"
  , ",$"
  , "_"
  , "^"
  , "&"
  , "|*"
--  , "'"
--  , "~"
--  , "!"
--  , ","
--  , "@"
  , "..."]

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

keywordDefine               = reserved "define"
keywordSet                  = reserved "set!"
keywordTest                 = reserved "test"
keywordExecute              = reserved "execute"
keywordLoadFile             = reserved "load-file"
keywordLoad                 = reserved "load"
keywordIf                   = reserved "if"
keywordNot                  = reserved "not"
keywordAnd                  = reserved "and"
keywordOr                   = reserved "or"
keywordSeq                  = reserved "seq"
keywordCApply               = reserved "capply"
keywordLambda               = reserved "lambda"
keywordMemoizedLambda       = reserved "memoized-lambda"
keywordMemoize              = reserved "memoize"
keywordCambda               = reserved "cambda"
keywordPatternFunction      = reserved "pattern-function"
keywordLetRec               = reserved "letrec"
keywordLet                  = reserved "let"
keywordLetStar              = reserved "let*"
keywordWithSymbols          = reserved "with-symbols"
keywordLoop                 = reserved "loop"
keywordCont                 = reserved "..."
keywordMatchAll             = reserved "match-all"
keywordMatchAllDFS          = reserved "match-all-dfs"
keywordMatchAllLambda       = reserved "match-all-lambda"
keywordMatch                = reserved "match"
keywordMatchDFS             = reserved "match-dfs"
keywordMatchLambda          = reserved "match-lambda"
keywordMatcher              = reserved "matcher"
keywordDo                   = reserved "do"
keywordIo                   = reserved "io"
keywordSomething            = reserved "something"
keywordUndefined            = reserved "undefined"
keywordAlgebraicDataMatcher = reserved "algebraic-data-matcher"
keywordGenerateTensor       = reserved "generate-tensor"
keywordTensor               = reserved "tensor"
keywordTensorContract       = reserved "contract"
keywordTensorMap            = reserved "tensor-map"
keywordTensorMap2           = reserved "tensor-map2"
keywordTranspose            = reserved "transpose"
keywordSubrefs              = reserved "subrefs"
keywordSubrefsNew           = reserved "subrefs!"
keywordSuprefs              = reserved "suprefs"
keywordSuprefsNew           = reserved "suprefs!"
keywordUserrefs             = reserved "user-refs"
keywordUserrefsNew          = reserved "user-refs!"
keywordFunction             = reserved "function"

sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate)
   <|> (char '+' >> return id)
   <|> return id

integerLiteral :: Parser Integer
integerLiteral = sign <*> P.natural lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

charLiteral :: Parser Char
charLiteral = P.charLiteral lexer

oneChar :: Parser Char
oneChar = do
  string "c#"
  x <- (char '\\' >> anyChar >>= (\x -> return ['\\', x])) <|> (anyChar >>= (\x -> return [x]))
  return $ doParse' charLiteral $ "'" ++ x ++ "'"

boolLiteral :: Parser Bool
boolLiteral = char '#' >> (char 't' $> True <|> char 'f' $> False)

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

brackets :: Parser a -> Parser a
brackets = P.brackets lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

angles :: Parser a -> Parser a
angles = P.angles lexer

ident :: Parser String
ident = toCamelCase <$> P.identifier lexer

identVar :: Parser Var
identVar = P.lexeme lexer (do
  name <- ident
  is <- many indexType
  return $ Var (splitOn "." name) is)

identVarWithoutIndex :: Parser Var
identVarWithoutIndex = stringToVar <$> ident

identVarWithIndices :: Parser VarWithIndices
identVarWithIndices = P.lexeme lexer (do
  name <- ident
  is <- many indexForVar
  return $ VarWithIndices (splitOn "." name) is)

indexForVar :: Parser (Index String)
indexForVar = try (char '~' >> Superscript <$> ident)
        <|> try (char '_' >> Subscript <$> ident)

indexType :: Parser (Index ())
indexType = try (char '~' >> return (Superscript ()))
        <|> try (char '_' >> return (Subscript ()))

upperName :: Parser String
upperName = P.lexeme lexer upperName'

upperName' :: Parser String
upperName' = (:) <$> upper <*> option "" ident
 where
  upper :: Parser Char
  upper = satisfy isUpper

lowerName :: Parser String
lowerName = P.lexeme lexer lowerName'

lowerName' :: Parser String
lowerName' = (:) <$> lower <*> option "" ident
 where
  lower :: Parser Char
  lower = satisfy isLower

renamedFunctions :: [(String, String)]
renamedFunctions =
  [ ("empty?",      "isEmpty")
  , ("S.empty?",    "S.isEmpty")
  , ("bool?",       "isBool")
  , ("integer?",    "isInteger")
  , ("rational?",   "isRational")
  , ("scalar?",     "isScalar")
  , ("float?",      "isFloat")
  , ("char?",       "isChar")
  , ("string?",     "isString")
  , ("collection?", "isCollection")
  , ("hash?",       "isHash")
  , ("tensor?",     "isTensor")
  , ("even?",       "isEven")
  , ("odd?",        "isOdd")
  , ("prime?",      "isPrime")
  , ("eof?",        "isEof")
  , ("eof-port?",   "isEofPort")
  , ("alphabet?",   "isAlphabet")
  , ("C.between?",  "C.isBetween")
  , ("alphabets?",  "isAlphabetString")
  , ("include?",    "include")
  , ("include/m?",  "includeAs")
  , ("member?",     "member")
  , ("member/m?",   "memberAs")
  , ("divisor?",    "divisor")
  , ("tree-member?","treeMember")
  , ("eq/m?",       "eqAs")
  , ("eq?",         "equal")
  , ("lt?",         "lt")
  , ("lte?",        "lte")
  , ("gt?",         "gt")
  , ("gte?",        "gte")
  , ("car",         "head")
  , ("cdr",         "tail")
  , ("rac",         "last")
  , ("rdc",         "init")
  ]

-- Translate identifiers for Non-S syntax
toCamelCase :: String -> String
toCamelCase "-'" = "-'"
toCamelCase "f.-'" = "f.-'"
toCamelCase "b.." = "b."
toCamelCase "b..'" = "b.'"
toCamelCase (flip lookup renamedFunctions -> Just name') =
  name'
toCamelCase (reverse -> 'm':'/':xs) =
  toCamelCase (reverse xs ++ "-as")
toCamelCase x =
  let heads:tails = splitOn "-" x
   in concat $ heads : map capitalize tails
  where
    capitalize [] = "-"
    capitalize (x:xs) = toUpper x : xs
