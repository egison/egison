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
import Text.Parsec hiding (char)
import Text.Parsec.ByteString.Lazy
import Text.Parsec.Combinator
import qualified Text.Parsec.Char  as C
import qualified Text.Parsec.Token as P

import Language.Egison.Types
  
notImplemented :: Parser a
notImplemented = choice []

-- Expressions

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
parseDefineExpr = keywordDefine >> (Define .) . (,) <$> parseVarNames <*> parseExpr

parseTestExpr :: Parser EgisonTopExpr
parseTestExpr = keywordTest >> Test <$> parseExpr

parseExecuteExpr :: Parser EgisonTopExpr
parseExecuteExpr = keywordExecute >> Execute <$> sepEndBy stringLiteral whiteSpace

parseLoadFileExpr :: Parser EgisonTopExpr
parseLoadFileExpr = keywordLoadFile >> LoadFile <$> stringLiteral

parseLoadExpr :: Parser EgisonTopExpr
parseLoadExpr = keywordLoad >> Load <$> stringLiteral

parseExpr :: Parser EgisonExpr
parseExpr = (parseVarExpr
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

parseVarExpr :: Parser EgisonExpr
parseVarExpr = VarExpr <$> ident <*> parseIndexNums

parseIndexNums :: Parser [EgisonExpr]
parseIndexNums = (prefixChar '_' >> ((:) <$> parseExpr <*> parseIndexNums))
              <|> pure []

parseInductiveExpr :: Parser EgisonExpr
parseInductiveExpr = angles $ InductiveDataExpr <$> ident <*> exprs
 where exprs = sepEndBy parseExpr whiteSpace

parseTupleExpr :: Parser EgisonExpr
parseTupleExpr = brackets $ TupleExpr <$> sepEndBy parseExpr whiteSpace

parseCollectionExpr :: Parser EgisonExpr
parseCollectionExpr = braces $ CollectionExpr <$> sepEndBy parseInnerExpr whiteSpace
 where
  parseInnerExpr :: Parser InnerExpr
  parseInnerExpr = (prefixChar '@' >> SubCollectionExpr <$> parseExpr)
               <|> ElementExpr <$> parseExpr

parseMatchAllExpr :: Parser EgisonExpr
parseMatchAllExpr = keywordMatchAll >> MatchAllExpr <$> parseExpr <*> parseExpr <*> parseMatchClause

parseMatchExpr :: Parser EgisonExpr
parseMatchExpr = keywordMatch >> MatchExpr <$> parseExpr <*> parseExpr <*> parseMatchClauses

parseFunctionExpr :: Parser EgisonExpr
parseFunctionExpr = keywordFunction >> FunctionExpr <$> parseExpr <*> parseMatchClauses

parseMatchClauses :: Parser [MatchClause]
parseMatchClauses = braces $ sepEndBy parseMatchClause whiteSpace

parseMatchClause :: Parser MatchClause
parseMatchClause = brackets $ (,) <$> parseExpr <*> parseExpr

parseMatcherExpr :: Parser EgisonExpr
parseMatcherExpr = keywordMatcher >> MatcherExpr <$> parsePPMatchClauses

parsePPMatchClauses :: Parser MatcherInfoExpr
parsePPMatchClauses = braces $ sepEndBy parsePPMatchClause whiteSpace

parsePPMatchClause :: Parser (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
parsePPMatchClause = brackets $ (,,) <$> parsePrimitivePatPattern <*> parseExpr <*> parsePMatchClauses

parsePMatchClauses :: Parser [(PrimitiveDataPattern, EgisonExpr)]
parsePMatchClauses = braces $ sepEndBy parsePMatchClause whiteSpace

parsePMatchClause :: Parser (PrimitiveDataPattern, EgisonExpr)
parsePMatchClause = brackets $ (,) <$> parsePrimitivePattern <*> parseExpr

parsePrimitivePatPattern :: Parser PrimitivePatPattern
parsePrimitivePatPattern = char '_' *> pure PPWildCard
                       <|> char '$' *> pure PPPatVar
                       <|> (prefixString ",$" >> PPValuePat <$> ident)
                       <|> angles (PPInductivePat <$> ident <*> sepEndBy parsePrimitivePatPattern whiteSpace)
                       <?> "primitive-pattren-pattern"

parsePrimitivePattern :: Parser PrimitiveDataPattern
parsePrimitivePattern = char '_' *> pure PWildCard
                    <|> (prefixChar '$' >> PPatVar <$> ident)
                    <|> braces ((PConsPat <$> parsePrimitivePattern <*> (prefixChar '@' *> parsePrimitivePattern))
                            <|> (PSnocPat <$> (prefixChar '@' *> parsePrimitivePattern) <*> parsePrimitivePattern) 
                            <|> pure PEmptyPat)
                    <|> angles (PInductivePat <$> ident <*> sepEndBy parsePrimitivePattern whiteSpace)
                    <|> PConstantPat <$> parseConstantExpr
                    <?> "primitive-data-pattern"

parseIfExpr :: Parser EgisonExpr
parseIfExpr = keywordIf >> IfExpr <$> parseExpr <*> parseExpr <*> parseExpr

parseLambdaExpr :: Parser EgisonExpr
parseLambdaExpr = keywordLambda >> LambdaExpr <$> parseVarNames <*> parseExpr

parseLetRecExpr :: Parser EgisonExpr
parseLetRecExpr =  keywordLetRec >> LetRecExpr <$> parseBindings <*> parseExpr

parseLetExpr :: Parser EgisonExpr
parseLetExpr = keywordLet >> LetExpr <$> parseBindings <*> parseExpr

parseDoExpr :: Parser EgisonExpr
parseDoExpr = keywordDo >> DoExpr <$> parseBindings <*> parseExpr

parseBindings :: Parser [Binding]
parseBindings = braces $ sepEndBy parseBinding whiteSpace

parseBinding :: Parser Binding
parseBinding = brackets $ (,) <$> parseVarNames <*> parseExpr

parseVarNames :: Parser [String]
parseVarNames = return <$> parseVarName
            <|> brackets (sepEndBy parseVarName whiteSpace) 

parseVarName :: Parser String
parseVarName = prefixChar '$' >> ident

parseApplyExpr :: Parser EgisonExpr
parseApplyExpr = do
  func <- parseExpr
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
  parseArg = try (Right <$> parseExpr)
         <|> prefixChar '$' *> (Left <$> option "" parseIndex)
  parseIndex = (try . noneOf $ "0") >> digits
  annonVars n = take n $ map (('#':) . show) [1..]

parseCutPatExpr :: Parser EgisonExpr
parseCutPatExpr = char '!' >> CutPatExpr <$> parseExpr

parseNotPatExpr :: Parser EgisonExpr
parseNotPatExpr = char '^' >> NotPatExpr <$> parseExpr

parseWildCardExpr :: Parser EgisonExpr
parseWildCardExpr = char '_' >> pure WildCardExpr

parseValuePatExpr :: Parser EgisonExpr
parseValuePatExpr = char ',' >> ValuePatExpr <$> parseExpr

parsePatVarExpr :: Parser EgisonExpr
parsePatVarExpr = PatVarExpr <$> parseVarName <*> parseIndexNums

parsePredPatExpr :: Parser EgisonExpr
parsePredPatExpr = char '?' >> PredPatExpr <$> parseExpr

parseAndPatExpr :: Parser EgisonExpr
parseAndPatExpr = char '&' >> AndPatExpr <$> sepEndBy parseExpr whiteSpace

parseOrPatExpr :: Parser EgisonExpr
parseOrPatExpr = char '|' >> OrPatExpr <$> sepEndBy parseExpr whiteSpace

parseOmitExpr :: Parser EgisonExpr
parseOmitExpr = prefixChar '`' >> OmitExpr <$> ident <*> parseIndexNums

parsePatVarOmitExpr :: Parser EgisonExpr
parsePatVarOmitExpr = prefixString "$`" >> PatVarOmitExpr <$> ident <*> parseIndexNums

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
                , P.identStart         = C.letter <|> symbol1
                , P.identLetter        = C.letter <|> C.digit <|> symbol2
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
boolLiteral = prefixChar '#' >> (char 't' *> pure True <|> char 'f' *> pure False)

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

prefix :: Parser a -> Parser a
prefix p = whiteSpace *> p

prefixChar :: Char -> Parser Char
prefixChar = prefix . C.char

prefixString :: String -> Parser String
prefixString = prefix . C.string

suffix :: Parser a -> Parser a
suffix p = p <* whiteSpace

suffixChar :: Char -> Parser Char
suffixChar = suffix . C.char

suffixString :: String -> Parser String
suffixString = suffix . C.string

digit :: Parser Char
digit = P.lexeme lexer $ C.digit

digits :: Parser [Char]
digits = P.lexeme lexer $ many1 C.digit

char :: Char -> Parser Char
char c = P.lexeme lexer $ C.char c

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
