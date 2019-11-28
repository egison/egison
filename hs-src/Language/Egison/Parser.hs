{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

{- |
Module      : Language.Egison.Parser
Copyright   : Satoshi Egi
Licence     : MIT

This module provide Egison parser.
-}

module Language.Egison.Parser
       (
       -- * Parse a string
         readTopExprs
       , readTopExpr
       , readExprs
       , readExpr
       , parseTopExprs
       , parseTopExpr
       , parseExprs
       , parseExpr
       -- * Parse a file
       , loadLibraryFile
       , loadFile
       ) where

import           Prelude                 hiding (mapM)

import           Control.Applicative     (pure, (*>), (<$>), (<*), (<*>))
import           Control.Monad.Except    (liftIO, throwError)
import           Control.Monad.Identity  (Identity, unless)

import           Data.Char               (isLower, isUpper)
import           Data.Either
import           Data.Functor            (($>))
import           Data.List.Split         (splitOn)
import           Data.Ratio
import qualified Data.Set                as Set
import qualified Data.Text               as T
import           Data.Traversable        (mapM)

import           Text.Parsec
import           Text.Parsec.String
import qualified Text.Parsec.Token       as P
import           System.Directory        (doesFileExist, getHomeDirectory)
import           System.IO

import           Language.Egison.AST
import           Language.Egison.Desugar
import           Language.Egison.Types
import           Paths_egison            (getDataFileName)

readTopExprs :: String -> EgisonM [EgisonTopExpr]
readTopExprs = either throwError (mapM desugarTopExpr) . parseTopExprs

readTopExpr :: String -> EgisonM EgisonTopExpr
readTopExpr = either throwError desugarTopExpr . parseTopExpr

readExprs :: String -> EgisonM [EgisonExpr]
readExprs = either throwError (mapM desugarExpr) . parseExprs

readExpr :: String -> EgisonM EgisonExpr
readExpr = either throwError desugarExpr . parseExpr

parseTopExprs :: String -> Either EgisonError [EgisonTopExpr]
parseTopExprs = doParse $ do
  ret <- whiteSpace >> endBy topExpr whiteSpace
  eof
  return ret

parseTopExpr :: String -> Either EgisonError EgisonTopExpr
parseTopExpr = doParse $ do
  ret <- whiteSpace >> topExpr
  whiteSpace >> eof
  return ret

parseExprs :: String -> Either EgisonError [EgisonExpr]
parseExprs = doParse $ do
  ret <- whiteSpace >> endBy expr whiteSpace
  eof
  return ret

parseExpr :: String -> Either EgisonError EgisonExpr
parseExpr = doParse $ do
  ret <- whiteSpace >> expr
  whiteSpace >> eof
  return ret

-- |Load a libary file
loadLibraryFile :: FilePath -> EgisonM [EgisonTopExpr]
loadLibraryFile file = do
  homeDir <- liftIO getHomeDirectory
  doesExist <- liftIO $ doesFileExist $ homeDir ++ "/.egison/" ++ file
  if doesExist
    then loadFile $ homeDir ++ "/.egison/" ++ file
    else liftIO (getDataFileName file) >>= loadFile

-- |Load a file
loadFile :: FilePath -> EgisonM [EgisonTopExpr]
loadFile file = do
  doesExist <- liftIO $ doesFileExist file
  unless doesExist $ throwError $ Default ("file does not exist: " ++ file)
  input <- liftIO $ readUTF8File file
  exprs <- readTopExprs $ shebang input
  concat <$> mapM  recursiveLoad exprs
 where
  recursiveLoad (Load file)     = loadLibraryFile file
  recursiveLoad (LoadFile file) = loadFile file
  recursiveLoad expr            = return [expr]
  shebang :: String -> String
  shebang ('#':'!':cs) = ';':'#':'!':cs
  shebang cs           = cs

readUTF8File :: FilePath -> IO String
readUTF8File name = do
  h <- openFile name ReadMode
  hSetEncoding h utf8
  hGetContents h

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
topExpr :: Parser EgisonTopExpr
topExpr = try (Test <$> expr)
      <|> try defineExpr
      <|> try (parens (redefineExpr
                   <|> testExpr
                   <|> executeExpr
                   <|> loadFileExpr
                   <|> loadExpr))
      <?> "top-level expression"

defineExpr :: Parser EgisonTopExpr
defineExpr = try (parens (keywordDefine >> Define <$> (char '$' >> identVar) <*> expr))
         <|> try (parens (do keywordDefine
                             (VarWithIndices name is) <- char '$' >> identVarWithIndices
                             Define (Var name (map f is)) . WithSymbolsExpr (map g is) . TransposeExpr (CollectionExpr (map (ElementExpr . VarExpr . stringToVar . g) is)) <$> expr))
 where
  f (Superscript _)  = Superscript ()
  f (Subscript _)    = Subscript ()
  f (SupSubscript _) = SupSubscript ()
  g (Superscript i)  = i
  g (Subscript i)    = i
  g (SupSubscript i) = i

redefineExpr :: Parser EgisonTopExpr
redefineExpr = (keywordRedefine <|> keywordSet) >> Redefine <$> (char '$' >> identVar) <*> expr

testExpr :: Parser EgisonTopExpr
testExpr = keywordTest >> Test <$> expr

executeExpr :: Parser EgisonTopExpr
executeExpr = keywordExecute >> Execute <$> expr

loadFileExpr :: Parser EgisonTopExpr
loadFileExpr = keywordLoadFile >> LoadFile <$> stringLiteral

loadExpr :: Parser EgisonTopExpr
loadExpr = keywordLoad >> Load <$> stringLiteral

expr :: Parser EgisonExpr
expr = P.lexeme lexer (do expr0 <- expr' <|> quoteExpr
                          expr1 <- option expr0 $ try (string "..." >> IndexedExpr False expr0 <$> parseindex)
                                                  <|> IndexedExpr True expr0 <$> parseindex
                          option expr1 $ PowerExpr expr1 <$> try (char '^' >> expr'))
                            where parseindex :: Parser [Index EgisonExpr]
                                  parseindex = many1 (try (MultiSubscript   <$> (char '_' >> expr') <*> (string "..._" >> expr'))
                                                  <|> try (MultiSuperscript <$> (char '~' >> expr') <*> (string "...~" >> expr'))
                                                  <|> try (Subscript    <$> (char '_' >> expr'))
                                                  <|> try (Superscript  <$> (char '~' >> expr'))
                                                  <|> try (SupSubscript <$> (string "~_" >> expr'))
                                                  <|> try (Userscript   <$> (char '|' >> expr')))


quoteExpr :: Parser EgisonExpr
quoteExpr = char '\'' >> QuoteExpr <$> expr'

expr' :: Parser EgisonExpr
expr' = try partialExpr
            <|> try constantExpr
            <|> try partialVarExpr
            <|> try freshVarExpr
            <|> try varExpr
            <|> inductiveDataExpr
            <|> try arrayExpr
            <|> try vectorExpr
            <|> try tupleExpr
            <|> try hashExpr
            <|> collectionExpr
            <|> quoteSymbolExpr
            <|> wedgeExpr
            <|> parens (ifExpr
                        <|> lambdaExpr
                        <|> memoizedLambdaExpr
                        <|> memoizeExpr
                        <|> cambdaExpr
                        <|> procedureExpr
                        <|> macroExpr
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
                        <|> generateArrayExpr
                        <|> arrayBoundsExpr
                        <|> arrayRefExpr
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

varExpr :: Parser EgisonExpr
varExpr = VarExpr <$> identVarWithoutIndex

freshVarExpr :: Parser EgisonExpr
freshVarExpr = char '#' >> return FreshVarExpr

inductiveDataExpr :: Parser EgisonExpr
inductiveDataExpr = angles $ InductiveDataExpr <$> upperName <*> sepEndBy expr whiteSpace

tupleExpr :: Parser EgisonExpr
tupleExpr = brackets $ TupleExpr <$> sepEndBy expr whiteSpace

collectionExpr :: Parser EgisonExpr
collectionExpr = braces $ CollectionExpr <$> sepEndBy innerExpr whiteSpace
 where
  innerExpr :: Parser InnerExpr
  innerExpr = (char '@' >> SubCollectionExpr <$> expr)
               <|> ElementExpr <$> expr

arrayExpr :: Parser EgisonExpr
arrayExpr = between lp rp $ ArrayExpr <$> sepEndBy expr whiteSpace
  where
    lp = P.lexeme lexer (string "(|")
    rp = string "|)"

vectorExpr :: Parser EgisonExpr
vectorExpr = between lp rp $ VectorExpr <$> sepEndBy expr whiteSpace
  where
    lp = P.lexeme lexer (string "[|")
    rp = string "|]"

hashExpr :: Parser EgisonExpr
hashExpr = between lp rp $ HashExpr <$> sepEndBy pairExpr whiteSpace
  where
    lp = P.lexeme lexer (string "{|")
    rp = string "|}"
    pairExpr :: Parser (EgisonExpr, EgisonExpr)
    pairExpr = brackets $ (,) <$> expr <*> expr

wedgeExpr :: Parser EgisonExpr
wedgeExpr = do
  e <- char '!' >> expr
  case e of
    ApplyExpr e1 e2 -> return $ WedgeApplyExpr e1 e2

functionWithArgExpr :: Parser EgisonExpr
functionWithArgExpr = keywordFunction >> FunctionExpr <$> between lp rp (sepEndBy expr whiteSpace)
  where
    lp = P.lexeme lexer (char '[')
    rp = char ']'

quoteSymbolExpr :: Parser EgisonExpr
quoteSymbolExpr = char '`' >> QuoteSymbolExpr <$> expr

matchAllExpr :: Parser EgisonExpr
matchAllExpr = keywordMatchAll >> MatchAllExpr BFSMode <$> expr <*> expr <*> (((:[]) <$> matchClause) <|> matchClauses)

matchAllDFSExpr :: Parser EgisonExpr
matchAllDFSExpr = keywordMatchAllDFS >> MatchAllExpr DFSMode <$> expr <*> expr <*> (((:[]) <$> matchClause) <|> matchClauses)

matchExpr :: Parser EgisonExpr
matchExpr = keywordMatch >> MatchExpr BFSMode <$> expr <*> expr <*> matchClauses

matchDFSExpr :: Parser EgisonExpr
matchDFSExpr = keywordMatchDFS >> MatchExpr DFSMode <$> expr <*> expr <*> matchClauses

matchAllLambdaExpr :: Parser EgisonExpr
matchAllLambdaExpr = keywordMatchAllLambda >> MatchAllLambdaExpr <$> expr <*> (((:[]) <$> matchClause) <|> matchClauses)

matchLambdaExpr :: Parser EgisonExpr
matchLambdaExpr = keywordMatchLambda >> MatchLambdaExpr <$> expr <*> matchClauses

matchClauses :: Parser [MatchClause]
matchClauses = braces $ sepEndBy matchClause whiteSpace

matchClause :: Parser MatchClause
matchClause = brackets $ (,) <$> pattern <*> expr

matcherExpr :: Parser EgisonExpr
matcherExpr = keywordMatcher >> MatcherExpr <$> ppMatchClauses

ppMatchClauses :: Parser [PatternDef]
ppMatchClauses = braces $ sepEndBy ppMatchClause whiteSpace

ppMatchClause :: Parser PatternDef
ppMatchClause = brackets $ (,,) <$> ppPattern <*> expr <*> pdMatchClauses

pdMatchClauses :: Parser [(PrimitiveDataPattern, EgisonExpr)]
pdMatchClauses = braces $ sepEndBy pdMatchClause whiteSpace

pdMatchClause :: Parser (PrimitiveDataPattern, EgisonExpr)
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
                    <|> (char '$' >> PDPatVar <$> ident)
                    <|> braces ((PDConsPat <$> pdPattern <*> (char '@' *> pdPattern))
                            <|> (PDSnocPat <$> (char '@' *> pdPattern) <*> pdPattern)
                            <|> pure PDEmptyPat)
                    <|> angles (PDInductivePat <$> upperName <*> sepEndBy pdPattern whiteSpace)
                    <|> brackets (PDTuplePat <$> sepEndBy pdPattern whiteSpace)
                    <|> PDConstantPat <$> constantExpr
                    <?> "primitive-data-pattern"

ifExpr :: Parser EgisonExpr
ifExpr = keywordIf >> IfExpr <$> expr <*> expr <*> expr

lambdaExpr :: Parser EgisonExpr
lambdaExpr = keywordLambda >> LambdaExpr <$> argNames <*> expr

memoizedLambdaExpr :: Parser EgisonExpr
memoizedLambdaExpr = keywordMemoizedLambda >> MemoizedLambdaExpr <$> varNames <*> expr

memoizeExpr :: Parser EgisonExpr
memoizeExpr = keywordMemoize >> MemoizeExpr <$> memoizeFrame <*> expr

memoizeFrame :: Parser [(EgisonExpr, EgisonExpr, EgisonExpr)]
memoizeFrame = braces $ sepEndBy memoizeBinding whiteSpace

memoizeBinding :: Parser (EgisonExpr, EgisonExpr, EgisonExpr)
memoizeBinding = brackets $ (,,) <$> expr <*> expr <*> expr

cambdaExpr :: Parser EgisonExpr
cambdaExpr = keywordCambda >> char '$' >> CambdaExpr <$> ident <*> expr

procedureExpr :: Parser EgisonExpr
procedureExpr = keywordProcedure >> ProcedureExpr <$> varNames <*> expr

macroExpr :: Parser EgisonExpr
macroExpr = keywordMacro >> MacroExpr <$> varNames <*> expr

patternFunctionExpr :: Parser EgisonExpr
patternFunctionExpr = keywordPatternFunction >> PatternFunctionExpr <$> varNames <*> pattern

letRecExpr :: Parser EgisonExpr
letRecExpr =  keywordLetRec >> LetRecExpr <$> bindings <*> expr

letExpr :: Parser EgisonExpr
letExpr = keywordLet >> LetExpr <$> bindings <*> expr

letStarExpr :: Parser EgisonExpr
letStarExpr = keywordLetStar >> LetStarExpr <$> bindings <*> expr

withSymbolsExpr :: Parser EgisonExpr
withSymbolsExpr = keywordWithSymbols >> WithSymbolsExpr <$> braces (sepEndBy ident whiteSpace) <*> expr

doExpr :: Parser EgisonExpr
doExpr = keywordDo >> DoExpr <$> statements <*> option (ApplyExpr (stringToVarExpr "return") (TupleExpr [])) expr

statements :: Parser [BindingExpr]
statements = braces $ sepEndBy statement whiteSpace

statement :: Parser BindingExpr
statement = try binding
        <|> try (brackets (([],) <$> expr))
        <|> (([],) <$> expr)

bindings :: Parser [BindingExpr]
bindings = braces $ sepEndBy binding whiteSpace

binding :: Parser BindingExpr
binding = brackets $ (,) <$> varNames' <*> expr

varNames :: Parser [String]
varNames = return <$> (char '$' >> ident)
            <|> brackets (sepEndBy (char '$' >> ident) whiteSpace)

varNames' :: Parser [Var]
varNames' = return <$> (char '$' >> identVar)
            <|> brackets (sepEndBy (char '$' >> identVar) whiteSpace)

argNames :: Parser [Arg]
argNames = return <$> argName
            <|> brackets (sepEndBy argName whiteSpace)

argName :: Parser Arg
argName = try (ScalarArg <$> (char '$' >> ident))
      <|> try (InvertedScalarArg <$> (string "*$" >> ident))
      <|> try (TensorArg <$> (char '%' >> ident))

ioExpr :: Parser EgisonExpr
ioExpr = keywordIo >> IoExpr <$> expr

seqExpr :: Parser EgisonExpr
seqExpr = keywordSeq >> SeqExpr <$> expr <*> expr

cApplyExpr :: Parser EgisonExpr
cApplyExpr = keywordCApply >> CApplyExpr <$> expr <*> expr

applyExpr :: Parser EgisonExpr
applyExpr = (keywordApply >> ApplyExpr <$> expr <*> expr)
             <|> applyExpr'

applyExpr' :: Parser EgisonExpr
applyExpr' = do
  func <- expr
  args <- args
  let vars = lefts args
  case vars of
    [] -> return . ApplyExpr func . TupleExpr $ rights args
    _ | all null vars ->
        let args' = rights args
            args'' = zipWith (curry f) args (annonVars 1 (length args))
            args''' = map (VarExpr . stringToVar . either id id) args''
        in return $ ApplyExpr (LambdaExpr (map ScalarArg (rights args'')) (LambdaExpr (map ScalarArg (lefts args'')) $ ApplyExpr func $ TupleExpr args''')) $ TupleExpr args'
      | all (not . null) vars ->
        let ns = Set.fromList $ map read vars
            n = Set.size ns
        in if Set.findMin ns == 1 && Set.findMax ns == n
             then
               let args' = rights args
                   args'' = zipWith (curry g) args (annonVars (n + 1) (length args))
                   args''' = map (VarExpr . stringToVar . either id id) args''
               in return $ ApplyExpr (LambdaExpr (map ScalarArg (rights args'')) (LambdaExpr (map ScalarArg (annonVars 1 n)) $ ApplyExpr func $ TupleExpr args''')) $ TupleExpr args'
             else fail "invalid partial application"
      | otherwise -> fail "invalid partial application"
 where
  args = sepEndBy arg whiteSpace
  arg = try (Right <$> expr)
         <|> char '$' *> (Left <$> option "" index)
  index = (:) <$> satisfy (\c -> '1' <= c && c <= '9') <*> many digit
  annonVars m n = take n $ map ((':':) . show) [m..]
  f (Left _, var)  = Left var
  f (Right _, var) = Right var
  g (Left arg, _)  = Left (':':arg)
  g (Right _, var) = Right var

partialExpr :: Parser EgisonExpr
partialExpr = (PartialExpr . read <$> index) <*> (char '#' >> expr)
 where
  index = (:) <$> satisfy (\c -> '1' <= c && c <= '9') <*> many digit

partialVarExpr :: Parser EgisonExpr
partialVarExpr = char '%' >> PartialVarExpr <$> integerLiteral

algebraicDataMatcherExpr :: Parser EgisonExpr
algebraicDataMatcherExpr = keywordAlgebraicDataMatcher
                                >> braces (AlgebraicDataMatcherExpr <$> sepEndBy1 inductivePat' whiteSpace)
  where
    inductivePat' :: Parser (String, [EgisonExpr])
    inductivePat' = angles $ (,) <$> lowerName <*> sepEndBy expr whiteSpace

generateArrayExpr :: Parser EgisonExpr
generateArrayExpr = keywordGenerateArray >> GenerateArrayExpr <$> expr <*> arrayRange

arrayRange :: Parser (EgisonExpr, EgisonExpr)
arrayRange = brackets $ (,) <$> expr <*> expr

arrayBoundsExpr :: Parser EgisonExpr
arrayBoundsExpr = keywordArrayBounds >> ArrayBoundsExpr <$> expr

arrayRefExpr :: Parser EgisonExpr
arrayRefExpr = keywordArrayRef >> ArrayRefExpr <$> expr <*> expr

generateTensorExpr :: Parser EgisonExpr
generateTensorExpr = keywordGenerateTensor >> GenerateTensorExpr <$> expr <*> expr

tensorExpr :: Parser EgisonExpr
tensorExpr = keywordTensor >> TensorExpr <$> expr <*> expr <*> option (CollectionExpr []) expr <*> option (CollectionExpr []) expr

tensorContractExpr :: Parser EgisonExpr
tensorContractExpr = keywordTensorContract >> TensorContractExpr <$> expr <*> expr

tensorMapExpr :: Parser EgisonExpr
tensorMapExpr = keywordTensorMap >> TensorMapExpr <$> expr <*> expr

tensorMap2Expr :: Parser EgisonExpr
tensorMap2Expr = keywordTensorMap2 >> TensorMap2Expr <$> expr <*> expr <*> expr

transposeExpr :: Parser EgisonExpr
transposeExpr = keywordTranspose >> TransposeExpr <$> expr <*> expr

subrefsExpr :: Parser EgisonExpr
subrefsExpr = (keywordSubrefs >> SubrefsExpr False <$> expr <*> expr)
               <|> (keywordSubrefsNew >> SubrefsExpr True <$> expr <*> expr)

suprefsExpr :: Parser EgisonExpr
suprefsExpr = (keywordSuprefs >> SuprefsExpr False <$> expr <*> expr)
               <|> (keywordSuprefsNew >> SuprefsExpr True <$> expr <*> expr)

userrefsExpr :: Parser EgisonExpr
userrefsExpr = (keywordUserrefs >> UserrefsExpr False <$> expr <*> expr)
                <|> (keywordUserrefsNew >> UserrefsExpr True <$> expr <*> expr)

-- Patterns

pattern :: Parser EgisonPattern
pattern = P.lexeme lexer (do pattern <- pattern'
                             option pattern $ IndexedPat pattern <$> many1 (try $ char '_' >> expr'))

pattern' :: Parser EgisonPattern
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
                    <|> try divPat
                    <|> try plusPat
                    <|> try multPat
                    <|> try dApplyPat
                    <|> try pApplyPat
                    )

pattern'' :: Parser EgisonPattern
pattern'' = wildCard
            <|> patVar
            <|> valuePat

wildCard :: Parser EgisonPattern
wildCard = reservedOp "_" >> pure WildCard

patVar :: Parser EgisonPattern
patVar = char '$' >> PatVar <$> identVarWithoutIndex

varPat :: Parser EgisonPattern
varPat = VarPat <$> ident

valuePat :: Parser EgisonPattern
valuePat = char ',' >> ValuePat <$> expr

predPat :: Parser EgisonPattern
predPat = char '?' >> PredPat <$> expr

letPat :: Parser EgisonPattern
letPat = keywordLet >> LetPat <$> bindings <*> pattern

notPat :: Parser EgisonPattern
notPat = char '!' >> NotPat <$> pattern

notPat' :: Parser EgisonPattern
notPat' = keywordNot >> NotPat <$> pattern

tuplePat :: Parser EgisonPattern
tuplePat = brackets $ TuplePat <$> sepEndBy pattern whiteSpace

inductivePat :: Parser EgisonPattern
inductivePat = angles $ InductivePat <$> lowerName <*> sepEndBy pattern whiteSpace

contPat :: Parser EgisonPattern
contPat = keywordCont >> pure ContPat

andPat :: Parser EgisonPattern
andPat = (reservedOp "&" <|> keywordAnd) >> AndPat <$> sepEndBy pattern whiteSpace

orPat :: Parser EgisonPattern
orPat = (reservedOp "|" <|> keywordOr) >> OrPat <$> sepEndBy pattern whiteSpace

pApplyPat :: Parser EgisonPattern
pApplyPat = PApplyPat <$> expr <*> sepEndBy pattern whiteSpace

dApplyPat :: Parser EgisonPattern
dApplyPat = DApplyPat <$> pattern'' <*> sepEndBy pattern whiteSpace

loopPat :: Parser EgisonPattern
loopPat = keywordLoop >> char '$' >> LoopPat <$> identVarWithoutIndex <*> loopRange <*> pattern <*> option (NotPat WildCard) pattern

loopRange :: Parser LoopRange
loopRange = brackets (try (LoopRange <$> expr <*> expr <*> option WildCard pattern)
                      <|> (do s <- expr
                              ep <- option WildCard pattern
                              return (LoopRange s (ApplyExpr (stringToVarExpr "from") (ApplyExpr (stringToVarExpr "-'") (TupleExpr [s, IntegerExpr 1]))) ep)))

seqNilPat :: Parser EgisonPattern
seqNilPat = braces $ pure SeqNilPat

seqConsPat :: Parser EgisonPattern
seqConsPat = braces $ SeqConsPat <$> pattern <*> (char '@' >> pattern)

seqPat :: Parser EgisonPattern
seqPat = braces $ do
  pats <- sepEndBy pattern whiteSpace
  tailPat <- option SeqNilPat (char '@' >> pattern)
  return $ foldr SeqConsPat tailPat pats

laterPatVar :: Parser EgisonPattern
laterPatVar = char '#' >> pure LaterPatVar

divPat :: Parser EgisonPattern
divPat = reservedOp "/" >> DivPat <$> pattern <*> pattern

plusPat :: Parser EgisonPattern
plusPat = reservedOp "+" >> PlusPat <$> sepEndBy pattern whiteSpace

multPat :: Parser EgisonPattern
multPat = reservedOp "*" >> MultPat <$> sepEndBy powerPat whiteSpace

powerPat :: Parser EgisonPattern
powerPat = try (PowerPat <$> pattern <* char '^' <*> pattern)
            <|> pattern

-- Constants

constantExpr :: Parser EgisonExpr
constantExpr = stringExpr
                 <|> boolExpr
                 <|> try charExpr
                 <|> try floatExpr
                 <|> try integerExpr
                 <|> (keywordSomething $> SomethingExpr)
                 <|> (keywordUndefined $> UndefinedExpr)
                 <?> "constant"

charExpr :: Parser EgisonExpr
charExpr = CharExpr <$> oneChar

stringExpr :: Parser EgisonExpr
stringExpr = StringExpr . T.pack <$> stringLiteral

boolExpr :: Parser EgisonExpr
boolExpr = BoolExpr <$> boolLiteral

floatExpr :: Parser EgisonExpr
floatExpr = FloatExpr <$> positiveFloatLiteral

integerExpr :: Parser EgisonExpr
integerExpr = IntegerExpr <$> integerLiteral'

integerLiteral' :: Parser Integer
integerLiteral' = sign <*> positiveIntegerLiteral

positiveIntegerLiteral :: Parser Integer
positiveIntegerLiteral = read <$> many1 digit

positiveFloatLiteral :: Parser Double
positiveFloatLiteral = do
  n <- positiveIntegerLiteral
  char '.'
  mStr <- many1 digit
  let m = read mStr
  let l = m % (10 ^ fromIntegral (length mStr))
  return (fromRational (fromIntegral n + l) :: Double)

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

symbol0 = oneOf "^"
symbol1 = oneOf "+-*/.=∂∇"
symbol2 = symbol1 <|> oneOf "'!?₀₁₂₃₄₅₆₇₈₉"

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser egisonDef

reservedKeywords :: [String]
reservedKeywords =
  [ "define"
  , "redefine"
  , "set!"
  , "test"
  , "execute"
  , "load-file"
  , "load"
  , "if"
  , "seq"
  , "apply"
  , "capply"
  , "lambda"
  , "memoized-lambda"
  , "memoize"
  , "cambda"
  , "procedure"
  , "macro"
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
  , "generate-array"
  , "array-bounds"
  , "array-ref"
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
keywordRedefine             = reserved "redefine"
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
keywordApply                = reserved "apply"
keywordCApply               = reserved "capply"
keywordLambda               = reserved "lambda"
keywordMemoizedLambda       = reserved "memoized-lambda"
keywordMemoize              = reserved "memoize"
keywordCambda               = reserved "cambda"
keywordProcedure            = reserved "procedure"
keywordMacro                = reserved "macro"
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
keywordGenerateArray        = reserved "generate-array"
keywordArrayBounds          = reserved "array-bounds"
keywordArrayRef             = reserved "array-ref"
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
ident = P.identifier lexer

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
