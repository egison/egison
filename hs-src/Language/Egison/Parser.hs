module Language.Egison.Parser 
       ( readTopExprs
       , readTopExpr
       , readExprs
       , readExpr ) where

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
readTopExprs input = runDesugarM $ either throwError (mapM desugarTopExpr) $ doParse (whiteSpace >> parseTopExprs) input

readTopExpr :: String -> Fresh (Either EgisonError EgisonTopExpr)
readTopExpr input = runDesugarM $ either throwError desugarTopExpr $ doParse (whiteSpace >> parseTopExpr) input

readExprs :: String -> Fresh (Either EgisonError [EgisonExpr])
readExprs input = runDesugarM $ either throwError (mapM desugar) $ doParse (whiteSpace >> parseExprs) input

readExpr :: String -> Fresh (Either EgisonError EgisonExpr)
readExpr input = runDesugarM $ either throwError desugar $ doParse (whiteSpace >> parseExpr) input

--
-- Expressions
--

parseTopExprs :: Parser [EgisonTopExpr]
parseTopExprs = endBy parseTopExpr whiteSpace

parseTopExpr :: Parser EgisonTopExpr
parseTopExpr = parens (parseDefineExpr
                       <|> parseTestExpr
                       <|> parseExecuteExpr
                       <|> parseLoadFileExpr
                       <|> parseLoadExpr
                       <?> "top-level expression")

parseDefineExpr :: Parser EgisonTopExpr
parseDefineExpr = keywordDefine >> Define <$> parseVarName <*> parseExpr

parseTestExpr :: Parser EgisonTopExpr
parseTestExpr = keywordTest >> Test <$> parseExpr

parseExecuteExpr :: Parser EgisonTopExpr
parseExecuteExpr = keywordExecute >> Execute <$> sepEndBy stringLiteral whiteSpace

parseLoadFileExpr :: Parser EgisonTopExpr
parseLoadFileExpr = keywordLoadFile >> LoadFile <$> stringLiteral

parseLoadExpr :: Parser EgisonTopExpr
parseLoadExpr = keywordLoad >> Load <$> stringLiteral

parseExprs :: Parser [EgisonExpr]
parseExprs = endBy parseExpr whiteSpace

parseExpr :: Parser EgisonExpr
parseExpr = do expr <- parseExpr'
               option expr $ IndexedExpr expr <$> many1 (try $ char '_' >> parseExpr')

parseExpr' :: Parser EgisonExpr
parseExpr' = (try parseConstantExpr
             <|> try parseVarExpr
             <|> parseInductiveDataExpr
             <|> try parseArrayExpr
             <|> parseTupleExpr
             <|> parseCollectionExpr
             <|> parens (parseIfExpr
                         <|> parseLambdaExpr
                         <|> parsePatternFunctionExpr
                         <|> parseLetRecExpr
                         <|> parseLetExpr
                         <|> parseIndexLoopExpr
                         <|> parseDoExpr
                         <|> parseMatchAllExpr
                         <|> parseMatchExpr
                         <|> parseMatcherExpr
                         <|> parseMatchLambdaExpr
                         <|> parseApplyExpr
                         <|> parseAlgebraicDataMatcherExpr
                         <|> parseGenerateArrayExpr
                         <|> parseArraySizeExpr
                         <|> parseArrayRefExpr)
                         <?> "expression")

parseVarExpr :: Parser EgisonExpr
parseVarExpr = VarExpr <$> ident

parseInductiveDataExpr :: Parser EgisonExpr
parseInductiveDataExpr = angles $ InductiveDataExpr <$> upperName <*> sepEndBy parseExpr whiteSpace

parseTupleExpr :: Parser EgisonExpr
parseTupleExpr = brackets $ TupleExpr <$> sepEndBy parseExpr whiteSpace

parseCollectionExpr :: Parser EgisonExpr
parseCollectionExpr = braces $ CollectionExpr . Sq.fromList <$> sepEndBy parseInnerExpr whiteSpace
 where
  parseInnerExpr :: Parser InnerExpr
  parseInnerExpr = (char '@' >> SubCollectionExpr <$> parseExpr)
               <|> ElementExpr <$> parseExpr

parseArrayExpr :: Parser EgisonExpr
parseArrayExpr = between lp rp $ ArrayExpr <$> sepEndBy parseExpr whiteSpace
  where
    lp = P.lexeme lexer (string "[|")
    rp = P.lexeme lexer (string "|]")

parseMatchAllExpr :: Parser EgisonExpr
parseMatchAllExpr = keywordMatchAll >> MatchAllExpr <$> parseExpr <*> parseExpr <*> parseMatchClause

parseMatchExpr :: Parser EgisonExpr
parseMatchExpr = keywordMatch >> MatchExpr <$> parseExpr <*> parseExpr <*> parseMatchClauses

parseMatchLambdaExpr :: Parser EgisonExpr
parseMatchLambdaExpr = keywordMatchLambda >> MatchLambdaExpr <$> parseExpr <*> parseMatchClauses

parseMatchClauses :: Parser [MatchClause]
parseMatchClauses = braces $ sepEndBy parseMatchClause whiteSpace

parseMatchClause :: Parser MatchClause
parseMatchClause = brackets $ (,) <$> parsePattern <*> parseExpr

parseMatcherExpr :: Parser EgisonExpr
parseMatcherExpr = keywordMatcher >> MatcherExpr <$> parsePPMatchClauses

parsePPMatchClauses :: Parser MatcherInfo
parsePPMatchClauses = braces $ sepEndBy parsePPMatchClause whiteSpace

parsePPMatchClause :: Parser (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
parsePPMatchClause = brackets $ (,,) <$> parsePPPattern <*> parseExpr <*> parsePDMatchClauses

parsePDMatchClauses :: Parser [(PrimitiveDataPattern, EgisonExpr)]
parsePDMatchClauses = braces $ sepEndBy parsePDMatchClause whiteSpace

parsePDMatchClause :: Parser (PrimitiveDataPattern, EgisonExpr)
parsePDMatchClause = brackets $ (,) <$> parsePDPattern <*> parseExpr

parsePPPattern :: Parser PrimitivePatPattern
parsePPPattern = parsePPWildCard
                 <|> parsePPPatVar
                 <|> parsePPValuePat
                 <|> parsePPInductivePat
                 <?> "primitive-pattren-pattern"
                       
parsePPWildCard :: Parser PrimitivePatPattern
parsePPWildCard = reservedOp "_" *> pure PPWildCard

parsePPPatVar :: Parser PrimitivePatPattern
parsePPPatVar = reservedOp "$" *> pure PPPatVar

parsePPValuePat :: Parser PrimitivePatPattern
parsePPValuePat = string ",$" >> PPValuePat <$> ident

parsePPInductivePat :: Parser PrimitivePatPattern
parsePPInductivePat = angles (PPInductivePat <$> lowerName <*> sepEndBy parsePPPattern whiteSpace)

parsePDPattern :: Parser PrimitiveDataPattern
parsePDPattern = reservedOp "_" *> pure PDWildCard
                    <|> (char '$' >> PDPatVar <$> ident)
                    <|> braces ((PDConsPat <$> parsePDPattern <*> (char '@' *> parsePDPattern))
                            <|> (PDSnocPat <$> (char '@' *> parsePDPattern) <*> parsePDPattern) 
                            <|> pure PDEmptyPat)
                    <|> angles (PDInductivePat <$> upperName <*> sepEndBy parsePDPattern whiteSpace)
                    <|> PDConstantPat <$> parseConstantExpr
                    <?> "primitive-data-pattern"

parseIfExpr :: Parser EgisonExpr
parseIfExpr = keywordIf >> IfExpr <$> parseExpr <*> parseExpr <*> parseExpr

parseLambdaExpr :: Parser EgisonExpr
parseLambdaExpr = keywordLambda >> LambdaExpr <$> parseVarNames <*> parseExpr

parsePatternFunctionExpr :: Parser EgisonExpr
parsePatternFunctionExpr = keywordPatternFunction >> PatternFunctionExpr <$> parseVarNames <*> parsePattern

parseLetRecExpr :: Parser EgisonExpr
parseLetRecExpr =  keywordLetRec >> LetRecExpr <$> parseBindings <*> parseExpr

parseLetExpr :: Parser EgisonExpr
parseLetExpr = keywordLet >> LetExpr <$> parseBindings <*> parseExpr

parseDoExpr :: Parser EgisonExpr
parseDoExpr = keywordDo >> DoExpr <$> parseBindings <*> parseExpr

parseBindings :: Parser [BindingExpr]
parseBindings = braces $ sepEndBy parseBinding whiteSpace

parseBinding :: Parser BindingExpr
parseBinding = brackets $ (,) <$> parseVarNames <*> parseExpr

parseVarNames :: Parser [String]
parseVarNames = return <$> parseVarName
            <|> brackets (sepEndBy parseVarName whiteSpace) 

parseVarName :: Parser String
parseVarName = char '$' >> ident

parseIndexLoopExpr :: Parser EgisonExpr
parseIndexLoopExpr = keywordIndexLoop >> IndexLoopExpr <$> parseVarName <*> parseVarName <*> parseVarName
                                                       <*> parseExpr <*> parseExpr <*> parseExpr <*> parseExpr

parseApplyExpr :: Parser EgisonExpr
parseApplyExpr = (keywordApply >> ApplyExpr <$> parseExpr <*> parseExpr) 
             <|> parseApplyExpr'

parseApplyExpr' :: Parser EgisonExpr
parseApplyExpr' = do
  func <- parseExpr
  args <- parseArgs
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
  parseArgs = sepEndBy parseArg whiteSpace
  parseArg = try (Right <$> parseExpr)
         <|> char '$' *> (Left <$> option "" parseIndex)
  parseIndex = (:) <$> satisfy (\c -> '1' <= c && c <= '9') <*> many digit
  annonVars n = take n $ map (('#':) . show) [1..]

parseAlgebraicDataMatcherExpr :: Parser EgisonExpr
parseAlgebraicDataMatcherExpr = keywordAlgebraicDataMatcher
                                >> braces (AlgebraicDataMatcherExpr <$> sepEndBy1 parseInductivePat' whiteSpace)
  where
    parseInductivePat' :: Parser (String, [EgisonExpr]) 
    parseInductivePat' = angles $ (,) <$> lowerName <*> sepEndBy parseExpr whiteSpace

parseGenerateArrayExpr :: Parser EgisonExpr
parseGenerateArrayExpr = keywordGenerateArray >> GenerateArrayExpr <$> parseVarNames <*> parseExpr <*> parseExpr

parseArraySizeExpr :: Parser EgisonExpr
parseArraySizeExpr = keywordArraySize >> ArraySizeExpr <$> parseExpr

parseArrayRefExpr :: Parser EgisonExpr
parseArrayRefExpr = keywordArrayRef >> ArrayRefExpr <$> parseExpr <*> parseExpr

-- Patterns

parsePattern :: Parser EgisonPattern
parsePattern = do pattern <- parsePattern'
                  option pattern $ IndexedPat pattern <$> many1 (try $ char '_' >> parseExpr')

parsePattern' :: Parser EgisonPattern
parsePattern' = parseWildCard
            <|> parsePatVar
            <|> parseVarPat
            <|> parseValuePat
            <|> parsePredPat
            <|> parseCutPat
            <|> parseNotPat
            <|> parseTuplePat
            <|> parseInductivePat
            <|> parens (parseAndPat
                    <|> parseOrPat
                    <|> parseApplyPat)

parseWildCard :: Parser EgisonPattern
parseWildCard = reservedOp "_" >> pure WildCard

parsePatVar :: Parser EgisonPattern
parsePatVar = P.lexeme lexer $ PatVar <$> parseVarName

parseVarPat :: Parser EgisonPattern
parseVarPat = VarPat <$> ident

parseValuePat :: Parser EgisonPattern
parseValuePat = reservedOp "," >> ValuePat <$> parseExpr

parsePredPat :: Parser EgisonPattern
parsePredPat = reservedOp "?" >> PredPat <$> parseExpr

parseCutPat :: Parser EgisonPattern
parseCutPat = reservedOp "!" >> CutPat <$> parsePattern

parseNotPat :: Parser EgisonPattern
parseNotPat = reservedOp "^" >> NotPat <$> parsePattern

parseTuplePat :: Parser EgisonPattern
parseTuplePat = brackets $ TuplePat <$> sepEndBy parsePattern whiteSpace

parseInductivePat :: Parser EgisonPattern
parseInductivePat = angles $ InductivePat <$> lowerName <*> sepEndBy parsePattern whiteSpace

parseAndPat :: Parser EgisonPattern
parseAndPat = reservedOp "&" >> AndPat <$> sepEndBy parsePattern whiteSpace

parseOrPat :: Parser EgisonPattern
parseOrPat = reservedOp "|" >> OrPat <$> sepEndBy parsePattern whiteSpace

parseApplyPat :: Parser EgisonPattern
parseApplyPat = ApplyPat <$> parseExpr <*> sepEndBy parsePattern whiteSpace 

-- Constants

parseConstantExpr :: Parser EgisonExpr
parseConstantExpr =  parseCharExpr
                 <|> parseStringExpr
                 <|> parseBoolExpr
                 <|> try parseFloatExpr
                 <|> parseIntegerExpr
                 <|> (keywordSomething *> pure SomethingExpr)
                 <|> (keywordUndefined *> pure UndefinedExpr)
                 <?> "constant"

parseCharExpr :: Parser EgisonExpr
parseCharExpr = CharExpr <$> charLiteral

parseStringExpr :: Parser EgisonExpr
parseStringExpr = StringExpr <$> stringLiteral

parseBoolExpr :: Parser EgisonExpr
parseBoolExpr = BoolExpr <$> boolLiteral

parseIntegerExpr :: Parser EgisonExpr
parseIntegerExpr = IntegerExpr <$> integerLiteral

parseFloatExpr :: Parser EgisonExpr
parseFloatExpr = FloatExpr <$> floatLiteral

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
  , "index-loop"
  , "match-all"
  , "match"
  , "matcher"
  , "do"
  , "function"
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
  , "@"]

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
keywordIndexLoop            = reserved "index-loop"
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
