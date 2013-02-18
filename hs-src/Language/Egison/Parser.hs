module Language.Egison.Parser where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative ((<$>), (<*>), (*>), (<*), pure)

import Data.Either
import Data.Set (Set)
import qualified Data.Set as Set

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Text.Parsec.Char
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P

import Language.Egison.Types
  
notImplemented :: Parser a
notImplemented = choice []

-- Expressions

parseEgisonTopExprs :: Parser [EgisonTopExpr]
parseEgisonTopExprs = endBy parseEgisonTopExpr whiteSpace

parseEgisonTopExpr :: Parser EgisonTopExpr
parseEgisonTopExpr = whiteSpace >> parens (parseDefineExpr
                                       <|> parseTestExpr
                                       <|> parseExecuteExpr
                                       <|> parseLoadFileExpr
                                       <|> parseLoadExpr
                                       <?> "top-level expression")

parseDefineExpr :: Parser EgisonTopExpr
parseDefineExpr = keywordDefine >> (Define .) . (,) <$> parseVarNames <*> parseEgisonExpr

parseTestExpr :: Parser EgisonTopExpr
parseTestExpr = keywordTest >> Test <$> parseEgisonExpr

parseExecuteExpr :: Parser EgisonTopExpr
parseExecuteExpr = keywordExecute >> Execute <$> sepEndBy stringLiteral whiteSpace

parseLoadFileExpr :: Parser EgisonTopExpr
parseLoadFileExpr = keywordLoadFile >> LoadFile <$> stringLiteral

parseLoadExpr :: Parser EgisonTopExpr
parseLoadExpr = keywordLoad >> Load <$> stringLiteral

parseEgisonExpr :: Parser EgisonExpr
parseEgisonExpr = whiteSpace >> (parseVar
                             <|> parseSym
                             <|> parseOmitExpr
                             <|> try parsePatVarExpr
                             <|> parsePatVarOmitExpr
                             
                             <|> parseWildCardExpr
                             <|> parseCutPatExpr
                             <|> parseNotPatExpr
                             <|> parseValuePatExpr
                             <|> parsePredPatExpr 
                              
                             <|> parseConstantExpr
                             <|> parseInductiveExpr
                             <|> parseTupleExpr
                             <|> parseCollectionExpr
                             <|> parens (parseAndPatExpr 
                                     <|> parseOrPatExpr
                                     <|> parseIfExpr
                                     <|> parseLambdaExpr
                                     <|> parseFunctionExpr
                                     <|> parseLetRecExpr
                                     <|> parseLetExpr
                                     <|> parseDoExpr
                                     <|> parseMatchAllExpr
                                     <|> parseMatchExpr
                                     <|> parseMatcherExpr
                                     <|> parseApplyExpr)
                             <?> "expression")

parseVar :: Parser EgisonExpr
parseVar = VarExpr <$> ident <*> parseIndexNums

parseSym :: Parser EgisonExpr
parseSym = char '%' >> SymExpr <$> ident

parseIndexNums :: Parser [EgisonExpr]
parseIndexNums = (char '_' >> ((:) <$> parseEgisonExpr <*> parseIndexNums))
              <|> pure []

parseInductiveExpr :: Parser EgisonExpr
parseInductiveExpr = angles $ InductiveDataExpr <$> ident <*> exprs
 where exprs = sepEndBy parseEgisonExpr whiteSpace

parseTupleExpr :: Parser EgisonExpr
parseTupleExpr = brackets $ TupleExpr <$> sepEndBy parseEgisonExpr whiteSpace

parseCollectionExpr :: Parser EgisonExpr
parseCollectionExpr = braces $ CollectionExpr <$> sepEndBy parseInnerExpr whiteSpace
 where
  parseInnerExpr :: Parser InnerExpr
  parseInnerExpr = (char '@' >> SubCollectionExpr <$> parseEgisonExpr)
               <|> ElementExpr <$> parseEgisonExpr

parseMatchAllExpr :: Parser EgisonExpr
parseMatchAllExpr = keywordMatchAll >> MatchAllExpr <$> parseEgisonExpr <*> parseEgisonExpr <*> parseMatchClause

parseMatchExpr :: Parser EgisonExpr
parseMatchExpr = keywordMatch >> MatchExpr <$> parseEgisonExpr <*> parseEgisonExpr <*> parseMatchClauses

parseFunctionExpr :: Parser EgisonExpr
parseFunctionExpr = keywordFunction >> FunctionExpr <$> parseEgisonExpr <*> parseMatchClauses

parseMatchClauses :: Parser [MatchClause]
parseMatchClauses = braces $ sepEndBy parseMatchClause whiteSpace

parseMatchClause :: Parser MatchClause
parseMatchClause = brackets $ (,) <$> parseEgisonExpr <*> parseEgisonExpr

parseMatcherExpr :: Parser EgisonExpr
parseMatcherExpr = keywordMatcher >> MatcherExpr <$> parsePPMatchClauses

parsePPMatchClauses :: Parser MatcherInfoExpr
parsePPMatchClauses = braces $ sepEndBy parsePPMatchClause whiteSpace

parsePPMatchClause :: Parser (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
parsePPMatchClause = brackets $ (,,) <$> parsePrimitivePatPattern <*> parseEgisonExpr <*> parsePMatchClauses

parsePMatchClauses :: Parser [(PrimitiveDataPattern, EgisonExpr)]
parsePMatchClauses = braces $ sepEndBy parsePMatchClause whiteSpace

parsePMatchClause :: Parser (PrimitiveDataPattern, EgisonExpr)
parsePMatchClause = brackets $ (,) <$> parsePrimitivePattern <*> parseEgisonExpr

parsePrimitivePatPattern :: Parser PrimitivePatPattern
parsePrimitivePatPattern = char '_' *> pure PPWildCard
                       <|> char '$' *> pure PPPatVar
                       <|> (string ",$" >> PPValuePat <$> ident)
                       <|> angles (PPInductivePat <$> ident <*> sepEndBy parsePrimitivePatPattern whiteSpace)
                       <?> "primitive-pattren-pattern"

parsePrimitivePattern :: Parser PrimitiveDataPattern
parsePrimitivePattern = char '_' *> pure PWildCard
                    <|> (char '$' >> PPatVar <$> ident)
                    <|> braces ((PConsPat <$> parsePrimitivePattern <*> (char '@' *> parsePrimitivePattern))
                            <|> (PSnocPat <$> (char '@' *> parsePrimitivePattern) <*> parsePrimitivePattern) 
                            <|> pure PEmptyPat)
                    <|> angles (PInductivePat <$> ident <*> sepEndBy parsePrimitivePattern whiteSpace)
                    <|> PConstantPat <$> parseConstantExpr
                    <?> "primitive-data-pattern"

parseIfExpr :: Parser EgisonExpr
parseIfExpr = keywordIf >> IfExpr <$> parseEgisonExpr <*> parseEgisonExpr <*> parseEgisonExpr

parseLambdaExpr :: Parser EgisonExpr
parseLambdaExpr = keywordLambda >> LambdaExpr <$> parseVarNames <*> parseEgisonExpr

parseLetRecExpr :: Parser EgisonExpr
parseLetRecExpr =  keywordLetRec >> LetRecExpr <$> parseBindings <*> parseEgisonExpr

parseLetExpr :: Parser EgisonExpr
parseLetExpr = keywordLet >> LetExpr <$> parseBindings <*> parseEgisonExpr

parseDoExpr :: Parser EgisonExpr
parseDoExpr = keywordDo >> DoExpr <$> parseBindings <*> parseEgisonExpr

parseBindings :: Parser [Binding]
parseBindings = braces $ sepEndBy parseBinding whiteSpace

parseBinding :: Parser Binding
parseBinding = brackets $ (,) <$> parseVarNames <*> parseEgisonExpr

parseVarNames :: Parser [String]
parseVarNames = return <$> parseVarName
            <|> brackets (sepEndBy parseVarName whiteSpace) 

parseVarName :: Parser String
parseVarName = char '$' >> ident

parseApplyExpr :: Parser EgisonExpr
parseApplyExpr = do
  func <- parseEgisonExpr
  args <- parseArgs
  let vars = lefts args
  case vars of
    [] -> return . ApplyExpr func . TupleExpr $ rights args
    _ | all null vars ->
        let genVar = modify (1+) >> gets (flip VarExpr [] . ('#':) . show)
            args' = evalState (mapM (either (const genVar) return) args) 0
        in return . LambdaExpr (annonVars $ length vars) . ApplyExpr func $ TupleExpr args'
      | all (not . null) vars ->
        let ns = Set.fromList $ map read vars
            n = Set.size ns
        in if Set.findMin ns == 1 && Set.findMax ns == n
             then
               let args' = map (either (flip VarExpr [] . ('#':)) id) args
               in return . LambdaExpr (annonVars n) . ApplyExpr func $ TupleExpr args'
             else fail "invalid partial application"
      | otherwise -> fail "invalid partial application"
 where
  parseArgs = sepEndBy parseArg whiteSpace
  parseArg = (char '$' >> Left <$> option "" parseIndex)
         <|> Right <$> parseEgisonExpr
  parseIndex = (:) <$> satisfy (\c -> '1' <= c && c <= '9') <*> many digit
  annonVars n = take n $ map (('#':) . show) [1..]

parseCutPatExpr :: Parser EgisonExpr
parseCutPatExpr = char '!' >> CutPatExpr <$> parseEgisonExpr

parseNotPatExpr :: Parser EgisonExpr
parseNotPatExpr = char '^' >> NotPatExpr <$> parseEgisonExpr

parseWildCardExpr :: Parser EgisonExpr
parseWildCardExpr = char '_' >> pure WildCardExpr

parseValuePatExpr :: Parser EgisonExpr
parseValuePatExpr = char ',' >> ValuePatExpr <$> parseEgisonExpr

parsePatVarExpr :: Parser EgisonExpr
parsePatVarExpr = PatVarExpr <$> parseVarName <*> parseIndexNums

parsePredPatExpr :: Parser EgisonExpr
parsePredPatExpr = char '?' >> PredPatExpr <$> parseEgisonExpr

parseAndPatExpr :: Parser EgisonExpr
parseAndPatExpr = char '&' >> AndPatExpr <$> sepEndBy parseEgisonExpr whiteSpace

parseOrPatExpr :: Parser EgisonExpr
parseOrPatExpr = char '|' >> OrPatExpr <$> sepEndBy parseEgisonExpr whiteSpace

parseOmitExpr :: Parser EgisonExpr
parseOmitExpr = char '`' >> OmitExpr <$> ident <*> parseIndexNums

parsePatVarOmitExpr :: Parser EgisonExpr
parsePatVarOmitExpr = string "$`" >> PatVarOmitExpr <$> ident <*> parseIndexNums

parseConstantExpr :: Parser EgisonExpr
parseConstantExpr =  parseCharExpr
                 <|> parseStringExpr
                 <|> parseBoolExpr
                 <|> parseIntegerExpr
                 <|> parseFloatExpr
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

-- Tokens

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
  symbol1 = oneOf "&*+-/:="
  symbol2 = symbol1 <|> oneOf "!?"

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
  , "then"
  , "else" 
  , "lambda"
  , "letrec"
  , "let"
  , "match-all"
  , "match"
  , "matcher"
  , "do"
  , "function"
  , "something"
  , "undefined"]
  
reservedOperators :: [String]
reservedOperators = []

reserved :: String -> Parser ()
reserved = P.reserved lexer

keywordDefine     = reserved "define"
keywordTest       = reserved "test"
keywordExecute    = reserved "execute"
keywordLoadFile   = reserved "load-file"
keywordLoad       = reserved "load"
keywordIf         = reserved "if"
keywordThen       = reserved "then"
keywordElse       = reserved "else"
keywordLambda     = reserved "lambda"
keywordLetRec     = reserved "letrec"
keywordLet        = reserved "let"
keywordMatchAll   = reserved "match-all"
keywordMatch      = reserved "match"
keywordMatcher    = reserved "matcher"
keywordDo         = reserved "do"
keywordFunction   = reserved "function"
keywordSomething  = reserved "something"
keywordUndefined  = reserved "undefiend"

integerLiteral :: Parser Integer
integerLiteral = P.integer lexer

floatLiteral :: Parser Double
floatLiteral = P.float lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

charLiteral :: Parser Char
charLiteral = P.charLiteral lexer

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
