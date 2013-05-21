module Language.Egison.Parser 
       ( readTopExprs
       , readTopExpr
       , readExprs
       , readExpr 
       , parseTopExprs
       , parseTopExpr
       , parseExprs
       , parseExpr ) where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative ((<$>), (<*>), (*>), (<*), pure)

import qualified Data.Sequence as Sq
import Data.Either
import Data.Set (Set)
import Data.Char (isLower, isUpper)
import qualified Data.Set as Set

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P

import Language.Egison.Types
import Language.Egison.Desugar
  
doParse :: Parser a -> String -> Either EgisonError a
doParse p input = either (throwError . Parser) return $ parse p "egison" $ B.pack input

readTopExprs :: String -> Fresh (Either EgisonError [EgisonTopExpr])
readTopExprs = runDesugarM . either throwError (mapM desugarTopExpr) . parseTopExprs

readTopExpr :: String -> Fresh (Either EgisonError EgisonTopExpr)
readTopExpr = runDesugarM . either throwError desugarTopExpr . parseTopExpr

readExprs :: String -> Fresh (Either EgisonError [EgisonExpr])
readExprs = runDesugarM . either throwError (mapM desugar) . parseExprs

readExpr :: String -> Fresh (Either EgisonError EgisonExpr)
readExpr = runDesugarM . either throwError desugar . parseExpr

parseTopExprs :: String -> Either EgisonError [EgisonTopExpr]
parseTopExprs = doParse $ whiteSpace >> endBy topExpr whiteSpace

parseTopExpr :: String -> Either EgisonError EgisonTopExpr
parseTopExpr = doParse $ whiteSpace >> topExpr

parseExprs :: String -> Either EgisonError [EgisonExpr]
parseExprs = doParse $ whiteSpace >> endBy expr whiteSpace

parseExpr :: String -> Either EgisonError EgisonExpr
parseExpr = doParse $ whiteSpace >> expr

--
-- Expressions
--

topExpr :: Parser EgisonTopExpr
topExpr = parens (defineExpr
                  <|> testExpr
                  <|> executeExpr
                  <|> loadFileExpr
                  <|> loadExpr
                  <?> "top-level expression")

defineExpr :: Parser EgisonTopExpr
defineExpr = keywordDefine >> Define <$> varName <*> expr

testExpr :: Parser EgisonTopExpr
testExpr = keywordTest >> Test <$> expr

executeExpr :: Parser EgisonTopExpr
executeExpr = keywordExecute >> Execute <$> sepEndBy stringLiteral whiteSpace

loadFileExpr :: Parser EgisonTopExpr
loadFileExpr = keywordLoadFile >> LoadFile <$> stringLiteral

loadExpr :: Parser EgisonTopExpr
loadExpr = keywordLoad >> Load <$> stringLiteral

exprs :: Parser [EgisonExpr]
exprs = endBy expr whiteSpace

expr :: Parser EgisonExpr
expr = do expr <- expr'
          option expr $ IndexedExpr expr <$> many1 (try $ char '_' >> expr')

expr' :: Parser EgisonExpr
expr' = (try constantExpr
             <|> try varExpr
             <|> inductiveDataExpr
             <|> try arrayExpr
             <|> tupleExpr
             <|> collectionExpr
             <|> parens (ifExpr
                         <|> lambdaExpr
                         <|> patternFunctionExpr
                         <|> letRecExpr
                         <|> letExpr
                         <|> doExpr
                         <|> matchAllExpr
                         <|> matchExpr
                         <|> matcherExpr
                         <|> matchLambdaExpr
                         <|> applyExpr
                         <|> algebraicDataMatcherExpr
                         <|> generateArrayExpr
                         <|> arraySizeExpr
                         <|> arrayRefExpr)
                         <?> "expression")

varExpr :: Parser EgisonExpr
varExpr = VarExpr <$> ident

inductiveDataExpr :: Parser EgisonExpr
inductiveDataExpr = angles $ InductiveDataExpr <$> upperName <*> sepEndBy expr whiteSpace

tupleExpr :: Parser EgisonExpr
tupleExpr = brackets $ TupleExpr <$> sepEndBy expr whiteSpace

collectionExpr :: Parser EgisonExpr
collectionExpr = braces $ CollectionExpr . Sq.fromList <$> sepEndBy innerExpr whiteSpace
 where
  innerExpr :: Parser InnerExpr
  innerExpr = (char '@' >> SubCollectionExpr <$> expr)
               <|> ElementExpr <$> expr

arrayExpr :: Parser EgisonExpr
arrayExpr = between lp rp $ ArrayExpr <$> sepEndBy expr whiteSpace
  where
    lp = P.lexeme lexer (string "[|")
    rp = P.lexeme lexer (string "|]")

matchAllExpr :: Parser EgisonExpr
matchAllExpr = keywordMatchAll >> MatchAllExpr <$> expr <*> expr <*> matchClause

matchExpr :: Parser EgisonExpr
matchExpr = keywordMatch >> MatchExpr <$> expr <*> expr <*> matchClauses

matchLambdaExpr :: Parser EgisonExpr
matchLambdaExpr = keywordMatchLambda >> MatchLambdaExpr <$> expr <*> matchClauses

matchClauses :: Parser [MatchClause]
matchClauses = braces $ sepEndBy matchClause whiteSpace

matchClause :: Parser MatchClause
matchClause = brackets $ (,) <$> pattern <*> expr

matcherExpr :: Parser EgisonExpr
matcherExpr = keywordMatcher >> MatcherExpr <$> ppMatchClauses

ppMatchClauses :: Parser MatcherInfo
ppMatchClauses = braces $ sepEndBy ppMatchClause whiteSpace

ppMatchClause :: Parser (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
ppMatchClause = brackets $ (,,) <$> pppattern <*> expr <*> pdMatchClauses

pdMatchClauses :: Parser [(PrimitiveDataPattern, EgisonExpr)]
pdMatchClauses = braces $ sepEndBy pdMatchClause whiteSpace

pdMatchClause :: Parser (PrimitiveDataPattern, EgisonExpr)
pdMatchClause = brackets $ (,) <$> pdPattern <*> expr

pppattern :: Parser PrimitivePatPattern
pppattern = ppWildCard
                 <|> pppatVar
                 <|> ppValuePat
                 <|> ppInductivePat
                 <?> "primitive-pattren-pattern"
                       
ppWildCard :: Parser PrimitivePatPattern
ppWildCard = reservedOp "_" *> pure PPWildCard

pppatVar :: Parser PrimitivePatPattern
pppatVar = reservedOp "$" *> pure PPPatVar

ppValuePat :: Parser PrimitivePatPattern
ppValuePat = string ",$" >> PPValuePat <$> ident

ppInductivePat :: Parser PrimitivePatPattern
ppInductivePat = angles (PPInductivePat <$> lowerName <*> sepEndBy pppattern whiteSpace)

pdPattern :: Parser PrimitiveDataPattern
pdPattern = reservedOp "_" *> pure PDWildCard
                    <|> (char '$' >> PDPatVar <$> ident)
                    <|> braces ((PDConsPat <$> pdPattern <*> (char '@' *> pdPattern))
                            <|> (PDSnocPat <$> (char '@' *> pdPattern) <*> pdPattern) 
                            <|> pure PDEmptyPat)
                    <|> angles (PDInductivePat <$> upperName <*> sepEndBy pdPattern whiteSpace)
                    <|> PDConstantPat <$> constantExpr
                    <?> "primitive-data-pattern"

ifExpr :: Parser EgisonExpr
ifExpr = keywordIf >> IfExpr <$> expr <*> expr <*> expr

lambdaExpr :: Parser EgisonExpr
lambdaExpr = keywordLambda >> LambdaExpr <$> varNames <*> expr

patternFunctionExpr :: Parser EgisonExpr
patternFunctionExpr = keywordPatternFunction >> PatternFunctionExpr <$> varNames <*> pattern

letRecExpr :: Parser EgisonExpr
letRecExpr =  keywordLetRec >> LetRecExpr <$> bindings <*> expr

letExpr :: Parser EgisonExpr
letExpr = keywordLet >> LetExpr <$> bindings <*> expr

doExpr :: Parser EgisonExpr
doExpr = keywordDo >> DoExpr <$> bindings <*> expr

bindings :: Parser [BindingExpr]
bindings = braces $ sepEndBy binding whiteSpace

binding :: Parser BindingExpr
binding = brackets $ (,) <$> varNames <*> expr

varNames :: Parser [String]
varNames = return <$> varName
            <|> brackets (sepEndBy varName whiteSpace) 

varName :: Parser String
varName = char '$' >> ident

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
        let genVar = modify (1+) >> gets (VarExpr . ('#':) . show)
            args' = evalState (mapM (either (const genVar) return) args) 0
        in return . LambdaExpr (annonVars $ length vars) . ApplyExpr func $ TupleExpr args'
      | all (not . null) vars ->
        let ns = Set.fromList $ map read vars
            n = Set.size ns
        in if Set.findMin ns == 1 && Set.findMax ns == n
             then
               let args' = map (either (VarExpr . ('#':)) id) args
               in return . LambdaExpr (annonVars n) . ApplyExpr func $ TupleExpr args'
             else fail "invalid partial application"
      | otherwise -> fail "invalid partial application"
 where
  args = sepEndBy arg whiteSpace
  arg = try (Right <$> expr)
         <|> char '$' *> (Left <$> option "" index)
  index = (:) <$> satisfy (\c -> '1' <= c && c <= '9') <*> many digit
  annonVars n = take n $ map (('#':) . show) [1..]

algebraicDataMatcherExpr :: Parser EgisonExpr
algebraicDataMatcherExpr = keywordAlgebraicDataMatcher
                                >> braces (AlgebraicDataMatcherExpr <$> sepEndBy1 inductivePat' whiteSpace)
  where
    inductivePat' :: Parser (String, [EgisonExpr]) 
    inductivePat' = angles $ (,) <$> lowerName <*> sepEndBy expr whiteSpace

generateArrayExpr :: Parser EgisonExpr
generateArrayExpr = keywordGenerateArray >> GenerateArrayExpr <$> varNames <*> expr <*> expr

arraySizeExpr :: Parser EgisonExpr
arraySizeExpr = keywordArraySize >> ArraySizeExpr <$> expr

arrayRefExpr :: Parser EgisonExpr
arrayRefExpr = keywordArrayRef >> ArrayRefExpr <$> expr <*> expr

-- Patterns

pattern :: Parser EgisonPattern
pattern = do pattern <- pattern'
             option pattern $ IndexedPat pattern <$> many1 (try $ char '_' >> expr')

pattern' :: Parser EgisonPattern
pattern' = wildCard
            <|> patVar
            <|> varPat
            <|> valuePat
            <|> predPat
            <|> cutPat
            <|> notPat
            <|> tuplePat
            <|> inductivePat
            <|> contPat
            <|> parens (andPat
                    <|> orPat
                    <|> applyPat
                    <|> looppat
                    <|> letPat)

wildCard :: Parser EgisonPattern
wildCard = reservedOp "_" >> pure WildCard

patVar :: Parser EgisonPattern
patVar = P.lexeme lexer $ PatVar <$> varName

varPat :: Parser EgisonPattern
varPat = VarPat <$> ident

valuePat :: Parser EgisonPattern
valuePat = reservedOp "," >> ValuePat <$> expr

predPat :: Parser EgisonPattern
predPat = reservedOp "?" >> PredPat <$> expr

letPat :: Parser EgisonPattern
letPat = keywordLet >> LetPat <$> bindings <*> pattern

cutPat :: Parser EgisonPattern
cutPat = reservedOp "!" >> CutPat <$> pattern

notPat :: Parser EgisonPattern
notPat = reservedOp "^" >> NotPat <$> pattern

tuplePat :: Parser EgisonPattern
tuplePat = brackets $ TuplePat <$> sepEndBy pattern whiteSpace

inductivePat :: Parser EgisonPattern
inductivePat = angles $ InductivePat <$> lowerName <*> sepEndBy pattern whiteSpace

contPat :: Parser EgisonPattern
contPat = reservedOp "..." >> pure ContPat

andPat :: Parser EgisonPattern
andPat = reservedOp "&" >> AndPat <$> sepEndBy pattern whiteSpace

orPat :: Parser EgisonPattern
orPat = reservedOp "|" >> OrPat <$> sepEndBy pattern whiteSpace

applyPat :: Parser EgisonPattern
applyPat = ApplyPat <$> expr <*> sepEndBy pattern whiteSpace 

looppat :: Parser EgisonPattern
looppat = keywordLoop >> LoopPat <$> varName <*> expr <*> pattern <*> pattern

-- Constants

constantExpr :: Parser EgisonExpr
constantExpr =  charExpr
                 <|> stringExpr
                 <|> boolExpr
                 <|> try floatExpr
                 <|> integerExpr
                 <|> (keywordSomething *> pure SomethingExpr)
                 <|> (keywordUndefined *> pure UndefinedExpr)
                 <?> "constant"

charExpr :: Parser EgisonExpr
charExpr = CharExpr <$> charLiteral

stringExpr :: Parser EgisonExpr
stringExpr = StringExpr <$> stringLiteral

boolExpr :: Parser EgisonExpr
boolExpr = BoolExpr <$> boolLiteral

integerExpr :: Parser EgisonExpr
integerExpr = IntegerExpr <$> integerLiteral

floatExpr :: Parser EgisonExpr
floatExpr = FloatExpr <$> floatLiteral

--
-- Tokens
--

egisonDef :: P.GenLanguageDef ByteString () Identity
egisonDef = 
  P.LanguageDef { P.commentStart       = "#|"
                , P.commentEnd         = "|#"
                , P.commentLine        = ";"
                , P.identStart         = letter <|> symbol1
                , P.identLetter        = letter <|> digit <|> symbol2
                , P.opStart            = symbol1
                , P.opLetter           = symbol1
                , P.reservedNames      = reservedKeywords
                , P.reservedOpNames    = reservedOperators
                , P.nestedComments     = True
                , P.caseSensitive      = True }
 where
  symbol1 = oneOf "&*/:="
  symbol2 = symbol1 <|> oneOf "+-!?"

lexer :: P.GenTokenParser ByteString () Identity
lexer = P.makeTokenParser egisonDef

reservedKeywords :: [String]
reservedKeywords = 
  [ "define"
  , "test"
  , "execute"
  , "load-file"
  , "load"
  , "if"
  , "apply"
  , "lambda"
  , "pattern-constructor"
  , "letrec"
  , "let"
  , "loop"
  , "match-all"
  , "match-lambda"
  , "match"
  , "matcher"
  , "do"
  , "algebraic-data-matcher"
  , "generate-array"
  , "array-size"
  , "array-ref"
  , "something"
  , "undefined"]
  
reservedOperators :: [String]
reservedOperators = 
  [ "$"
  , "_"
  , "&"
  , "|"
  , "^"
  , "!"
  , ","
  , "@"
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
keywordApply                = reserved "apply"
keywordLambda               = reserved "lambda"
keywordPatternFunction      = reserved "pattern-function"
keywordLetRec               = reserved "letrec"
keywordLet                  = reserved "let"
keywordLoop                 = reserved "loop"
keywordMatchAll             = reserved "match-all"
keywordMatch                = reserved "match"
keywordMatchLambda          = reserved "match-lambda"
keywordMatcher              = reserved "matcher"
keywordDo                   = reserved "do"
keywordSomething            = reserved "something"
keywordUndefined            = reserved "undefined"
keywordAlgebraicDataMatcher = reserved "algebraic-data-matcher"
keywordGenerateArray        = reserved "generate-array"
keywordArraySize            = reserved "array-size"
keywordArrayRef             = reserved "array-ref"

sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate)
   <|> (char '+' >> return id)
   <|> return id

integerLiteral :: Parser Integer
integerLiteral = sign <*> P.natural lexer

floatLiteral :: Parser Double
floatLiteral = sign <*> P.float lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

charLiteral :: Parser Char
charLiteral = P.charLiteral lexer

boolLiteral :: Parser Bool
boolLiteral = P.lexeme lexer $ char '#' >> (char 't' *> pure True <|> char 'f' *> pure False)

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
    <|> try ((:) <$> char '+' <*> ident)
    <|> try ((:) <$> char '-' <*> ident)
    <|> (P.lexeme lexer $ string "+")
    <|> (P.lexeme lexer $ string "-")

upperName :: Parser String
upperName = P.lexeme lexer $ (:) <$> upper <*> option "" ident
 where
  upper :: Parser Char 
  upper = satisfy isUpper

lowerName :: Parser String
lowerName = P.lexeme lexer $ (:) <$> lower <*> option "" ident
 where
  lower :: Parser Char 
  lower = satisfy isLower
