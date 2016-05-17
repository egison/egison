{-# LANGUAGE TupleSections #-}

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

import Prelude hiding (mapM)
import Control.Monad.Identity hiding (mapM)
import Control.Monad.Error hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Applicative ((<$>), (<*>), (*>), (<*), pure)

import System.Directory (doesFileExist, getHomeDirectory)

import qualified Data.Sequence as Sq
import Data.Either
import Data.Char (isLower, isUpper)
import qualified Data.Set as Set
import Data.Traversable (mapM)
import Data.Ratio

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P

import qualified Data.Text as T
import Text.Regex.TDFA

import Language.Egison.Types
import Language.Egison.Desugar
import Paths_egison (getDataFileName)

readTopExprs :: String -> EgisonM [EgisonTopExpr]
readTopExprs = either throwError (mapM desugarTopExpr) . parseTopExprs

readTopExpr :: String -> EgisonM EgisonTopExpr
readTopExpr = either throwError desugarTopExpr . parseTopExpr

readExprs :: String -> EgisonM [EgisonExpr]
readExprs = liftEgisonM . runDesugarM . either throwError (mapM desugar) . parseExprs

readExpr :: String -> EgisonM EgisonExpr
readExpr = liftEgisonM . runDesugarM . either throwError desugar . parseExpr

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
loadLibraryFile file =
  if file =~ "^lib/"
    then liftIO (getDataFileName file) >>= loadFile
    else do homeDir <- liftIO $ getHomeDirectory
            loadFile $ homeDir ++ "/.egison/" ++ file

-- |Load a file
loadFile :: FilePath -> EgisonM [EgisonTopExpr]
loadFile file = do
  doesExist <- liftIO $ doesFileExist file
  unless doesExist $ throwError $ strMsg ("file does not exist: " ++ file)
  input <- liftIO $ readFile file
  exprs <- readTopExprs $ shebang input
  concat <$> mapM  recursiveLoad exprs
 where
  recursiveLoad (Load file) = loadLibraryFile file
  recursiveLoad (LoadFile file) = loadFile file
  recursiveLoad expr = return [expr]
  shebang :: String -> String
  shebang ('#':'!':cs) = ';':'#':'!':cs
  shebang cs = cs

--
-- Parser
--

doParse :: Parser a -> String -> Either EgisonError a
doParse p input = either (throwError . fromParsecError) return $ parse p "egison" input
  where
    fromParsecError :: ParseError -> EgisonError
    fromParsecError = Parser . show

--
-- Expressions
--
topExpr :: Parser EgisonTopExpr
topExpr = try (Test <$> expr)
      <|> try (parens (defineExpr
                   <|> testExpr
                   <|> executeExpr
                   <|> loadFileExpr
                   <|> loadExpr))
      <?> "top-level expression"

defineExpr :: Parser EgisonTopExpr
defineExpr = keywordDefine >> Define <$> varName <*> expr

testExpr :: Parser EgisonTopExpr
testExpr = keywordTest >> Test <$> expr

executeExpr :: Parser EgisonTopExpr
executeExpr = keywordExecute >> Execute <$> expr

loadFileExpr :: Parser EgisonTopExpr
loadFileExpr = keywordLoadFile >> LoadFile <$> stringLiteral

loadExpr :: Parser EgisonTopExpr
loadExpr = keywordLoad >> Load <$> stringLiteral

exprs :: Parser [EgisonExpr]
exprs = endBy expr whiteSpace

expr :: Parser EgisonExpr
expr = P.lexeme lexer (do expr0 <- expr'
                          expr1 <- option expr0 $ PowerExpr expr0 <$> (try $ char '^' >> expr')
                          option expr1 $ IndexedExpr expr1 <$> many1 (try (char '_' >> expr' >>= return . Subscript) <|> try (char '~' >> expr' >>= return . Superscript)))

expr' :: Parser EgisonExpr
expr' = (try partialExpr
             <|> try constantExpr
             <|> try partialVarExpr
             <|> recVarExpr
             <|> try varExpr
             <|> inductiveDataExpr
             <|> try arrayExpr
             <|> try tensorExpr
             <|> try tupleExpr
             <|> try hashExpr
             <|> collectionExpr
             <|> quoteExpr
             <|> parens (ifExpr
                         <|> lambdaExpr
                         <|> memoizedLambdaExpr
                         <|> memoizeExpr
                         <|> cambdaExpr
                         <|> macroExpr
                         <|> patternFunctionExpr
                         <|> letRecExpr
                         <|> letExpr
                         <|> letStarExpr
                         <|> doExpr
                         <|> ioExpr
                         <|> matchAllExpr
                         <|> matchExpr
                         <|> matchAllLambdaExpr
                         <|> matchLambdaExpr
                         <|> nextMatchAllExpr
                         <|> nextMatchExpr
                         <|> nextMatchAllLambdaExpr
                         <|> nextMatchLambdaExpr
                         <|> matcherExpr
                         <|> matcherBFSExpr
                         <|> matcherDFSExpr
                         <|> seqExpr
                         <|> applyExpr
                         <|> cApplyExpr
                         <|> algebraicDataMatcherExpr
                         <|> generateArrayExpr
                         <|> arrayBoundsExpr
                         <|> arrayRefExpr
                         <|> generateTensorExpr
                         <|> initTensorExpr
                         <|> tensorMapExpr
                         <|> tensorMap2Expr
                         )
             <?> "expression")

varExpr :: Parser EgisonExpr
varExpr = VarExpr <$> ident

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
    lp = P.lexeme lexer (string "[|")
    rp = string "|]"

tensorExpr :: Parser EgisonExpr
tensorExpr = between lp rp $ TensorExpr <$> expr <*> expr
  where
    lp = P.lexeme lexer (string "(|")
    rp = string "|)"

hashExpr :: Parser EgisonExpr
hashExpr = between lp rp $ HashExpr <$> sepEndBy pairExpr whiteSpace
  where
    lp = P.lexeme lexer (string "{|")
    rp = string "|}"
    pairExpr :: Parser (EgisonExpr, EgisonExpr)
    pairExpr = brackets $ (,) <$> expr <*> expr

quoteExpr :: Parser EgisonExpr
quoteExpr = char '\'' >> QuoteExpr <$> expr

matchAllExpr :: Parser EgisonExpr
matchAllExpr = keywordMatchAll >> MatchAllExpr <$> expr <*> expr <*> matchClause

matchExpr :: Parser EgisonExpr
matchExpr = keywordMatch >> MatchExpr <$> expr <*> expr <*> matchClauses

matchAllLambdaExpr :: Parser EgisonExpr
matchAllLambdaExpr = keywordMatchAllLambda >> MatchAllLambdaExpr <$> expr <*> matchClause

matchLambdaExpr :: Parser EgisonExpr
matchLambdaExpr = keywordMatchLambda >> MatchLambdaExpr <$> expr <*> matchClauses

nextMatchAllExpr :: Parser EgisonExpr
nextMatchAllExpr = keywordNextMatchAll >> NextMatchAllExpr <$> expr <*> expr <*> matchClause

nextMatchExpr :: Parser EgisonExpr
nextMatchExpr = keywordNextMatch >> NextMatchExpr <$> expr <*> expr <*> matchClauses

nextMatchAllLambdaExpr :: Parser EgisonExpr
nextMatchAllLambdaExpr = keywordNextMatchAllLambda >> NextMatchAllLambdaExpr <$> expr <*> matchClause

nextMatchLambdaExpr :: Parser EgisonExpr
nextMatchLambdaExpr = keywordNextMatchLambda >> NextMatchLambdaExpr <$> expr <*> matchClauses

matchClauses :: Parser [MatchClause]
matchClauses = braces $ sepEndBy matchClause whiteSpace

matchClause :: Parser MatchClause
matchClause = brackets $ (,) <$> pattern <*> expr

matcherExpr :: Parser EgisonExpr
matcherExpr = keywordMatcher >> MatcherBFSExpr <$> ppMatchClauses

matcherBFSExpr :: Parser EgisonExpr
matcherBFSExpr = keywordMatcherBFS >> MatcherBFSExpr <$> ppMatchClauses

matcherDFSExpr :: Parser EgisonExpr
matcherDFSExpr = keywordMatcherDFS >> MatcherDFSExpr <$> ppMatchClauses

ppMatchClauses :: Parser MatcherInfo
ppMatchClauses = braces $ sepEndBy ppMatchClause whiteSpace

ppMatchClause :: Parser (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
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
                        <?> "primitive-pattren-pattern")
                       
ppWildCard :: Parser PrimitivePatPattern
ppWildCard = reservedOp "_" *> pure PPWildCard

ppPatVar :: Parser PrimitivePatPattern
ppPatVar = reservedOp "$" *> pure PPPatVar

ppValuePat :: Parser PrimitivePatPattern
ppValuePat = reservedOp ",$" >> PPValuePat <$> ident

ppInductivePat :: Parser PrimitivePatPattern
ppInductivePat = angles (PPInductivePat <$> lowerName <*> sepEndBy ppPattern whiteSpace)

pdPattern :: Parser PrimitiveDataPattern
pdPattern = P.lexeme lexer $ pdPattern'

pdPattern' :: Parser PrimitiveDataPattern
pdPattern' = reservedOp "_" *> pure PDWildCard
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
lambdaExpr = keywordLambda >> LambdaExpr <$> varNames <*> expr

memoizedLambdaExpr :: Parser EgisonExpr
memoizedLambdaExpr = keywordMemoizedLambda >> MemoizedLambdaExpr <$> varNames <*> expr

memoizeExpr :: Parser EgisonExpr
memoizeExpr = keywordMemoize >> MemoizeExpr <$> memoizeFrame <*> expr

memoizeFrame :: Parser [(EgisonExpr, EgisonExpr, EgisonExpr)]
memoizeFrame = braces $ sepEndBy memoizeBinding whiteSpace

memoizeBinding :: Parser (EgisonExpr, EgisonExpr, EgisonExpr)
memoizeBinding = brackets $ (,,) <$> expr <*> expr <*> expr

cambdaExpr :: Parser EgisonExpr
cambdaExpr = keywordCambda >> CambdaExpr <$> varName <*> expr

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

doExpr :: Parser EgisonExpr
doExpr = keywordDo >> DoExpr <$> statements <*> option (ApplyExpr (VarExpr "return") (TupleExpr [])) expr

statements :: Parser [BindingExpr]
statements = braces $ sepEndBy statement whiteSpace

statement :: Parser BindingExpr
statement = try binding
        <|> try (brackets (([],) <$> expr))
        <|> (([],) <$> expr)

bindings :: Parser [BindingExpr]
bindings = braces $ sepEndBy binding whiteSpace

binding :: Parser BindingExpr
binding = brackets $ (,) <$> varNames <*> expr

varNames :: Parser [String]
varNames = return <$> varName
            <|> brackets (sepEndBy varName whiteSpace) 

varName :: Parser String
varName = char '$' >> ident

ioExpr :: Parser EgisonExpr
ioExpr = keywordIo >> IoExpr <$> expr

seqExpr :: Parser EgisonExpr
seqExpr = keywordSeq >> SeqExpr <$> expr <*> expr

recVarExpr :: Parser EgisonExpr
recVarExpr = reservedOp "#" >> pure RecVarExpr

cApplyExpr :: Parser EgisonExpr
cApplyExpr = (keywordCApply >> CApplyExpr <$> expr <*> expr) 

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
        let genVar = modify (1+) >> gets (VarExpr . (':':) . show)
            args' = evalState (mapM (either (const genVar) return) args) 0
        in return . LambdaExpr (annonVars $ length vars) . ApplyExpr func $ TupleExpr args'
      | all (not . null) vars ->
        let ns = Set.fromList $ map read vars
            n = Set.size ns
        in if Set.findMin ns == 1 && Set.findMax ns == n
             then
               let args' = map (either (VarExpr . (':':)) id) args
               in return . LambdaExpr (annonVars n) . ApplyExpr func $ TupleExpr args'
             else fail "invalid partial application"
      | otherwise -> fail "invalid partial application"
 where
  args = sepEndBy arg whiteSpace
  arg = try (Right <$> expr)
         <|> char '$' *> (Left <$> option "" index)
  index = (:) <$> satisfy (\c -> '1' <= c && c <= '9') <*> many digit
  annonVars n = take n $ map ((':':) . show) [1..]

partialExpr :: Parser EgisonExpr
partialExpr = PartialExpr <$> read <$> index <*> (char '#' >> expr)
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
arrayRange = brackets (do s <- expr
                          e <- expr
                          return (s, e))

arrayBoundsExpr :: Parser EgisonExpr
arrayBoundsExpr = keywordArrayBounds >> ArrayBoundsExpr <$> expr

arrayRefExpr :: Parser EgisonExpr
arrayRefExpr = keywordArrayRef >> ArrayRefExpr <$> expr <*> expr

generateTensorExpr :: Parser EgisonExpr
generateTensorExpr = keywordGenerateTensor >> GenerateTensorExpr <$> expr <*> expr

initTensorExpr :: Parser EgisonExpr
initTensorExpr = keywordInitTensor >> InitTensorExpr <$> expr <*> expr <*> expr <*> expr

tensorMapExpr :: Parser EgisonExpr
tensorMapExpr = keywordTensorMap >> TensorMapExpr <$> expr <*> expr

tensorMap2Expr :: Parser EgisonExpr
tensorMap2Expr = keywordTensorMap2 >> TensorMap2Expr <$> expr <*> expr <*> expr

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
            <|> parens (andPat
                    <|> orderedOrPat
                    <|> orPat
                    <|> loopPat
                    <|> letPat
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
patVar = PatVar <$> varName

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

tuplePat :: Parser EgisonPattern
tuplePat = brackets $ TuplePat <$> sepEndBy pattern whiteSpace

inductivePat :: Parser EgisonPattern
inductivePat = angles $ InductivePat <$> lowerName <*> sepEndBy pattern whiteSpace

contPat :: Parser EgisonPattern
contPat = keywordCont >> pure ContPat

andPat :: Parser EgisonPattern
andPat = reservedOp "&" >> AndPat <$> sepEndBy pattern whiteSpace

orPat :: Parser EgisonPattern
orPat = reservedOp "|" >> OrPat <$> sepEndBy pattern whiteSpace

orderedOrPat :: Parser EgisonPattern
orderedOrPat = reservedOp "|*" >> OrderedOrPat <$> sepEndBy pattern whiteSpace

pApplyPat :: Parser EgisonPattern
pApplyPat = PApplyPat <$> expr <*> sepEndBy pattern whiteSpace 

dApplyPat :: Parser EgisonPattern
dApplyPat = DApplyPat <$> pattern'' <*> sepEndBy pattern whiteSpace 

loopPat :: Parser EgisonPattern
loopPat = keywordLoop >> LoopPat <$> varName <*> loopRange <*> pattern <*> option (NotPat WildCard) pattern

loopRange :: Parser LoopRange
loopRange = brackets (try (do s <- expr
                              e <- expr
                              ep <- option WildCard pattern
                              return (LoopRange s e ep))
                 <|> (do s <- expr
                         ep <- option WildCard pattern
                         return (LoopRange s (ApplyExpr (VarExpr "from") (ApplyExpr (VarExpr "-'") (TupleExpr [s, (IntegerExpr 1)]))) ep)))

-- Constants

constantExpr :: Parser EgisonExpr
constantExpr = stringExpr
                 <|> boolExpr
                 <|> try charExpr
                 <|> try floatExpr
                 <|> try integerExpr
                 <|> (keywordSomething *> pure SomethingExpr)
                 <|> (keywordUndefined *> pure UndefinedExpr)
                 <?> "constant"

charExpr :: Parser EgisonExpr
charExpr = CharExpr <$> charLiteral

stringExpr :: Parser EgisonExpr
stringExpr = StringExpr . T.pack <$> stringLiteral

boolExpr :: Parser EgisonExpr
boolExpr = BoolExpr <$> boolLiteral

floatExpr :: Parser EgisonExpr
floatExpr = do
  (x,y) <- try (do x <- floatLiteral'
                   y <- sign' <*> positiveFloatLiteral
                   char 'i'
                   return (x,y))
            <|> try (do y <- floatLiteral'
                        char 'i'
                        return (0,y))
            <|> try (do x <- floatLiteral'
                        return (x,0))
  return $ FloatExpr x y

integerExpr :: Parser EgisonExpr
integerExpr = do
  n <- integerLiteral'
  return $ IntegerExpr n

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
  let l = m % (10 ^ (fromIntegral (length mStr)))
  return (fromRational ((fromIntegral n) + l) :: Double)

floatLiteral' :: Parser Double
floatLiteral' = sign <*> positiveFloatLiteral

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
symbol1 = oneOf "+-*/=.∂∇"
symbol2 = symbol1 <|> oneOf "'!?"

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser egisonDef

reservedKeywords :: [String]
reservedKeywords = 
  [ "define"
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
  , "macro"
  , "pattern-function"
  , "letrec"
  , "let"
  , "let*"
  , "loop"
  , "match-all"
  , "match"
  , "match-all-lambda"
  , "match-lambda"
  , "matcher"
  , "matcher-bfs"
  , "matcher-dfs"
  , "do"
  , "io"
  , "algebraic-data-matcher"
  , "generate-array"
  , "array-bounds"
  , "array-ref"
  , "generate-tensor"
  , "init-tensor"
  , "tensor-map"
  , "tensor-map2"
  , "something"
  , "undefined"]
  
reservedOperators :: [String]
reservedOperators = 
  [ "$"
  , ",$"
  , "_"
  , "^"
  , "&"
  , "|"
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
keywordTest                 = reserved "test"
keywordExecute              = reserved "execute"
keywordLoadFile             = reserved "load-file"
keywordLoad                 = reserved "load"
keywordIf                   = reserved "if"
keywordThen                 = reserved "then"
keywordElse                 = reserved "else"
keywordSeq                  = reserved "seq"
keywordApply                = reserved "apply"
keywordCApply               = reserved "capply"
keywordLambda               = reserved "lambda"
keywordMemoizedLambda       = reserved "memoized-lambda"
keywordMemoize              = reserved "memoize"
keywordCambda               = reserved "cambda"
keywordMacro                = reserved "macro"
keywordPatternFunction      = reserved "pattern-function"
keywordLetRec               = reserved "letrec"
keywordLet                  = reserved "let"
keywordLetStar              = reserved "let*"
keywordLoop                 = reserved "loop"
keywordCont                 = reserved "..."
keywordMatchAll             = reserved "match-all"
keywordMatchAllLambda       = reserved "match-all-lambda"
keywordMatch                = reserved "match"
keywordMatchLambda          = reserved "match-lambda"
keywordNextMatchAll         = reserved "next-match-all"
keywordNextMatchAllLambda   = reserved "next-match-all-lambda"
keywordNextMatch            = reserved "next-match"
keywordNextMatchLambda      = reserved "next-match-lambda"
keywordMatcher              = reserved "matcher"
keywordMatcherBFS           = reserved "matcher-bfs"
keywordMatcherDFS           = reserved "matcher-dfs"
keywordDo                   = reserved "do"
keywordIo                   = reserved "io"
keywordSomething            = reserved "something"
keywordUndefined            = reserved "undefined"
keywordAlgebraicDataMatcher = reserved "algebraic-data-matcher"
keywordGenerateArray        = reserved "generate-array"
keywordArrayBounds          = reserved "array-bounds"
keywordArrayRef             = reserved "array-ref"
keywordGenerateTensor       = reserved "generate-tensor"
keywordInitTensor           = reserved "init-tensor"
keywordTensorMap            = reserved "tensor-map"
keywordTensorMap2           = reserved "tensor-map2"

sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate)
   <|> (char '+' >> return id)
   <|> return id

sign' :: Num a => Parser (a -> a)
sign' = (char '-' >> return negate)
    <|> (char '+' >> return id)

naturalLiteral :: Parser Integer
naturalLiteral = P.natural lexer

integerLiteral :: Parser Integer
integerLiteral = sign <*> P.natural lexer

floatLiteral :: Parser Double
floatLiteral = sign <*> P.float lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

--charLiteral :: Parser Char
--charLiteral = P.charLiteral lexer
charLiteral :: Parser Char
charLiteral = string "c#" >> anyChar

boolLiteral :: Parser Bool
boolLiteral = char '#' >> (char 't' *> pure True <|> char 'f' *> pure False)

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

colon :: Parser String
colon = P.colon lexer

comma :: Parser String
comma = P.comma lexer

dot :: Parser String
dot = P.dot lexer

ident :: Parser String
ident = P.identifier lexer

upperName :: Parser String
upperName = P.lexeme lexer $ upperName'

upperName' :: Parser String
upperName' = (:) <$> upper <*> option "" ident
 where
  upper :: Parser Char 
  upper = satisfy isUpper

lowerName :: Parser String
lowerName = P.lexeme lexer $ lowerName'

lowerName' :: Parser String
lowerName' = (:) <$> lower <*> option "" ident
 where
  lower :: Parser Char 
  lower = satisfy isLower
