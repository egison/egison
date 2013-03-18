module Language.Egison.Parser where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative ((<$>), (<*>), (*>), (<*), pure)

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
  
notImplemented :: Parser a
notImplemented = choice []

-- Expressions

parseTopExprs :: Parser [EgisonTopExpr]
parseTopExprs = whiteSpace >> endBy parseTopExpr whiteSpace

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

parseExpr :: Parser EgisonExpr
parseExpr = (try parseVarExpr
--           <|> parseOmitExpr
             <|> try parsePatVarExpr
--           <|> parsePatVarOmitExpr
                       
             <|> parseWildCardExpr
             <|> parseCutPatExpr
             <|> parseNotPatExpr
             <|> parseValuePatExpr
             <|> parsePredPatExpr 
                        
             <|> parseConstantExpr
             <|> try parseInductiveDataExpr
             <|> parseInductivePatternExpr
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
parseIndexNums = ((:) <$> try (prefix >> parseExpr) <*> parseIndexNums)
              <|> pure []
  where
    prefix :: Parser Char
    prefix = do result <- char '_' >> isConsumed whiteSpace
                if result then parserFail "unexpected whiteSpace"
                          else return '_'
    
    isConsumed :: Parser a -> Parser Bool
    isConsumed p = (/=) <$> getPosition <*> (p >> getPosition)

parseInductiveDataExpr :: Parser EgisonExpr
parseInductiveDataExpr = angles $ InductiveDataExpr <$> upperName <*> sepEndBy parseExpr whiteSpace

parseInductivePatternExpr :: Parser EgisonExpr
parseInductivePatternExpr = angles $ (PatternExpr .) . InductivePattern <$> lowerName <*> sepEndBy parseExpr whiteSpace

parseTupleExpr :: Parser EgisonExpr
parseTupleExpr = brackets $ TupleExpr <$> sepEndBy parseExpr whiteSpace

parseCollectionExpr :: Parser EgisonExpr
parseCollectionExpr = braces $ CollectionExpr <$> sepEndBy parseInnerExpr whiteSpace
 where
  parseInnerExpr :: Parser InnerExpr
  parseInnerExpr = (char '@' >> SubCollectionExpr <$> parseExpr)
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

parsePPMatchClauses :: Parser MatcherInfo
parsePPMatchClauses = braces $ sepEndBy parsePPMatchClause whiteSpace

parsePPMatchClause :: Parser (PrimitivePatPattern, EgisonExpr, [(PrimitiveDataPattern, EgisonExpr)])
parsePPMatchClause = brackets $ (,,) <$> parsePPPattern <*> parseExpr <*> parsePDMatchClauses

parsePDMatchClauses :: Parser [(PrimitiveDataPattern, EgisonExpr)]
parsePDMatchClauses = braces $ sepEndBy parsePDMatchClause whiteSpace

parsePDMatchClause :: Parser (PrimitiveDataPattern, EgisonExpr)
parsePDMatchClause = brackets $ (,) <$> parsePDPattern <*> parseExpr

parsePPPattern :: Parser PrimitivePatPattern
parsePPPattern = reservedOp "_" *> pure PPWildCard
                       <|> reservedOp "$" *> pure PPPatVar
                       <|> (string ",$" >> PPValuePat <$> ident)
                       <|> angles (PPInductivePat <$> lowerName <*> sepEndBy parsePPPattern whiteSpace)
                       <?> "primitive-pattren-pattern"

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
parseLambdaExpr = (keywordLambda <|> keywordPatternConstructor) >> LambdaExpr <$> parseVarNames <*> parseExpr

parseLetRecExpr :: Parser EgisonExpr
parseLetRecExpr =  keywordLetRec >> LetRecExpr <$> parseBindings' <*> parseExpr

parseLetExpr :: Parser EgisonExpr
parseLetExpr = keywordLet >> LetExpr <$> parseBindings <*> parseExpr

parseDoExpr :: Parser EgisonExpr
parseDoExpr = keywordDo >> DoExpr <$> parseBindings <*> parseExpr

parseBindings :: Parser [BindingExpr]
parseBindings = braces $ sepEndBy parseBinding whiteSpace

parseBinding :: Parser BindingExpr
parseBinding = brackets $ (,) <$> parseVarNames <*> parseExpr

parseBindings' :: Parser [(String, EgisonExpr)]
parseBindings' = braces $ sepEndBy parseBinding' whiteSpace

parseBinding' :: Parser (String, EgisonExpr)
parseBinding' = brackets $ (,) <$> parseVarName <*> parseExpr

parseVarNames :: Parser [String]
parseVarNames = return <$> parseVarName
            <|> brackets (sepEndBy parseVarName whiteSpace) 

parseVarName :: Parser String
parseVarName = char '$' >> ident

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
         <|> char '$' *> (Left <$> option "" parseIndex)
  parseIndex = (:) <$> satisfy (\c -> '1' <= c && c <= '9') <*> many digit
  annonVars n = take n $ map (('#':) . show) [1..]

parseCutPatExpr :: Parser EgisonExpr
parseCutPatExpr = reservedOp "!" >> PatternExpr . CutPat <$> parseExpr

parseNotPatExpr :: Parser EgisonExpr
parseNotPatExpr = reservedOp "^" >> PatternExpr . NotPat <$> parseExpr

parseWildCardExpr :: Parser EgisonExpr
parseWildCardExpr = reservedOp "_" >> pure (PatternExpr WildCard)

parseValuePatExpr :: Parser EgisonExpr
parseValuePatExpr = reservedOp "," >> PatternExpr . ValuePat <$> parseExpr

parsePatVarExpr :: Parser EgisonExpr
parsePatVarExpr = P.lexeme lexer $ (PatternExpr .) . PatVar <$> parseVarName <*> parseIndexNums

parsePredPatExpr :: Parser EgisonExpr
parsePredPatExpr = reservedOp "?" >> PatternExpr . PredPat <$> parseExpr

parseAndPatExpr :: Parser EgisonExpr
parseAndPatExpr = reservedOp "&" >> PatternExpr . AndPat <$> sepEndBy parseExpr whiteSpace

parseOrPatExpr :: Parser EgisonExpr
parseOrPatExpr = reservedOp "|" >> PatternExpr . OrPat <$> sepEndBy parseExpr whiteSpace



--parseOmitExpr :: Parser EgisonExpr
--parseOmitExpr = prefixChar '`' >> OmitExpr <$> ident <*> parseIndexNums

--parsePatVarOmitExpr :: Parser EgisonExpr
--parsePatVarOmitExpr = prefixString "$`" >> PatVarOmitExpr <$> ident <*> parseIndexNums

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

keywordDefine             = reserved "define"
keywordTest               = reserved "test"
keywordExecute            = reserved "execute"
keywordLoadFile           = reserved "load-file"
keywordLoad               = reserved "load"
keywordIf                 = reserved "if"
keywordThen               = reserved "then"
keywordElse               = reserved "else"
keywordLambda             = reserved "lambda"
keywordPatternConstructor = reserved "pattern-constructor"
keywordLetRec             = reserved "letrec"
keywordLet                = reserved "let"
keywordMatchAll           = reserved "match-all"
keywordMatch              = reserved "match"
keywordMatcher            = reserved "matcher"
keywordDo                 = reserved "do"
keywordFunction           = reserved "function"
keywordSomething          = reserved "something"
keywordUndefined          = reserved "undefined"

integerLiteral :: Parser Integer
integerLiteral = P.integer lexer

floatLiteral :: Parser Double
floatLiteral = P.float lexer

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
