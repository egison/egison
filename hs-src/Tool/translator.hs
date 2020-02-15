{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Arrow                         ((***))
import           Data.Char                             (toUpper)
import           Data.List                             (find)
import           Data.List.Split                       (splitOn)
import           Data.Maybe                            (fromJust)
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           System.Environment                    (getArgs)

import           Language.Egison.AST
import           Language.Egison.Parser
import           Language.Egison.Pretty

class SyntaxElement a where
  toNonS :: a -> a

instance SyntaxElement EgisonTopExpr where
  toNonS (Define x y)   = Define (toNonS x) (toNonS y)
  toNonS (Redefine _ _) = error "Not supported"
  toNonS (Test x)       = Test (toNonS x)
  toNonS (Execute x)    = Execute (toNonS x)
  toNonS x              = x

instance SyntaxElement EgisonExpr where
  toNonS (IntegerExpr x) = IntegerExpr x
  toNonS (VarExpr v) | any (\op -> func op == prettyS v) reservedExprInfix =
    SectionExpr op Nothing Nothing
      where
        op = fromJust $ find (\op -> func op == prettyS v) reservedExprInfix
  toNonS (VarExpr x) = VarExpr (toNonS x)

  toNonS (IndexedExpr b x ys)  = IndexedExpr  b (toNonS x) (map toNonS ys)
  toNonS (SubrefsExpr b x y)   = SubrefsExpr  b (toNonS x) (toNonS y)
  toNonS (SuprefsExpr b x y)   = SuprefsExpr  b (toNonS x) (toNonS y)
  toNonS (UserrefsExpr b x y)  = UserrefsExpr b (toNonS x) (toNonS y)
  toNonS (PowerExpr x y) = BinaryOpExpr powerOp (toNonS x) (toNonS y)
    where powerOp = fromJust $ find (\op -> repr op == "^") reservedExprInfix
  toNonS (InductiveDataExpr x ys) = InductiveDataExpr x (map toNonS ys)
  toNonS (TupleExpr xs)      = TupleExpr (map toNonS xs)
  toNonS (CollectionExpr xs)
    | all isElementExpr xs = CollectionExpr (map toNonS xs)
    | otherwise            = f xs
    where
      isElementExpr :: InnerExpr -> Bool
      isElementExpr ElementExpr{} = True
      isElementExpr _             = False
      f [] = CollectionExpr []
      f [ElementExpr x] = CollectionExpr [ElementExpr (toNonS x)]
      f [SubCollectionExpr x] = toNonS x
      f (ElementExpr x : xs) = BinaryOpExpr cons (toNonS x) (f xs)
      f (SubCollectionExpr x : xs) = BinaryOpExpr append (toNonS x) (f xs)
      cons = fromJust $ find (\op -> repr op == "::") reservedExprInfix
      append = fromJust $ find (\op -> repr op == "++") reservedExprInfix
  toNonS (ArrayExpr xs)      = ArrayExpr (map toNonS xs)
  toNonS (HashExpr xs)       = HashExpr (map (toNonS *** toNonS) xs)
  toNonS (VectorExpr xs)     = VectorExpr (map toNonS xs)

  toNonS (LambdaExpr xs e)          = LambdaExpr xs (toNonS e)
  toNonS (MemoizedLambdaExpr xs e)  = MemoizedLambdaExpr xs (toNonS e)
  toNonS (CambdaExpr x e)           = CambdaExpr x (toNonS e)
  toNonS (ProcedureExpr xs e)       = ProcedureExpr xs (toNonS e)
  toNonS (PatternFunctionExpr xs p) = PatternFunctionExpr xs (toNonS p)

  toNonS (IfExpr x y z)         = IfExpr (toNonS x) (toNonS y) (toNonS z)
  toNonS (LetRecExpr xs y)      = LetRecExpr (map toNonS xs) (toNonS y)
  toNonS (LetExpr xs y)         = LetRecExpr (map toNonS xs) (toNonS y)
  toNonS (LetStarExpr xs y)     = LetRecExpr (map toNonS xs) (toNonS y)
  toNonS (WithSymbolsExpr xs y) = WithSymbolsExpr xs (toNonS y)

  toNonS (MatchExpr pmmode m p xs)    = MatchExpr pmmode (toNonS m) (toNonS p) (map toNonS xs)
  toNonS (MatchAllExpr pmmode m p xs) = MatchAllExpr pmmode (toNonS m) (toNonS p) (map toNonS xs)
  toNonS (MatchLambdaExpr p xs)       = MatchLambdaExpr    (toNonS p) (map toNonS xs)
  toNonS (MatchAllLambdaExpr p xs)    = MatchAllLambdaExpr (toNonS p) (map toNonS xs)

  toNonS (MatcherExpr xs) = MatcherExpr (map toNonS xs)

  toNonS (QuoteExpr x)        = QuoteExpr (toNonS x)
  toNonS (QuoteSymbolExpr x)  = QuoteSymbolExpr (toNonS x)
  toNonS (WedgeApplyExpr x y) = WedgeApplyExpr (toNonS x) (toNonS y)

  toNonS (DoExpr xs y) = DoExpr (map toNonS xs) (toNonS y)
  toNonS (IoExpr x)    = IoExpr (toNonS x)

  toNonS (ApplyExpr (VarExpr f) (TupleExpr (y:ys)))
    | any (\op -> func op == prettyS f) reservedExprInfix =
      optimize $ foldl (\acc x -> BinaryOpExpr op acc (toNonS x)) (toNonS y) ys
      where
        op = fromJust $ find (\op -> func op == prettyS f) reservedExprInfix

        optimize (BinaryOpExpr (Infix { repr = "*" }) (IntegerExpr (-1)) e2) =
          UnaryOpExpr "-" (optimize e2)
        optimize (BinaryOpExpr op e1 e2) =
          BinaryOpExpr op (optimize e1) (optimize e2)
        optimize e = e

  toNonS (ApplyExpr x y) = ApplyExpr (toNonS x) (toNonS y)
  toNonS (CApplyExpr e1 e2) = CApplyExpr (toNonS e1) (toNonS e2)
  toNonS (PartialExpr n e) =
    -- SectionExpr with only one argument omitted is hard to detect correctly.
    case PartialExpr n (toNonS e) of
      PartialExpr 2 (BinaryOpExpr op (PartialVarExpr 1) (PartialVarExpr 2)) ->
        SectionExpr op Nothing Nothing
      e' -> e'

  toNonS (GenerateArrayExpr e (e1, e2)) = GenerateArrayExpr (toNonS e) (toNonS e1, toNonS e2)
  toNonS (ArrayBoundsExpr e) = ArrayBoundsExpr (toNonS e)
  toNonS (ArrayRefExpr e1 e2) = ArrayRefExpr (toNonS e1) (toNonS e2)

  toNonS (GenerateTensorExpr e1 e2) = GenerateTensorExpr (toNonS e1) (toNonS e2)
  toNonS (TensorExpr e1 e2 e3 e4) = TensorExpr (toNonS e1) (toNonS e2) (toNonS e3) (toNonS e4)
  toNonS (TensorContractExpr e1 e2) = TensorContractExpr (toNonS e1) (toNonS e2)
  toNonS (TensorMapExpr e1 e2) = TensorMapExpr (toNonS e1) (toNonS e2)
  toNonS (TensorMap2Expr e1 e2 e3) = TensorMap2Expr (toNonS e1) (toNonS e2) (toNonS e3)
  toNonS (TransposeExpr e1 e2) = TransposeExpr (toNonS e1) (toNonS e2)
  toNonS (FlipIndicesExpr e1) = FlipIndicesExpr (toNonS e1)

  toNonS x = x

instance SyntaxElement EgisonPattern where
  toNonS (ValuePat e) = ValuePat (toNonS e)
  toNonS (PredPat e) = PredPat (toNonS e)
  toNonS (LetPat binds pat) = LetPat (map toNonS binds) (toNonS pat)
  toNonS (NotPat p) = NotPat (toNonS p)
  toNonS (InfixPat op p1 p2) = InfixPat op (toNonS p1) (toNonS p2)
  toNonS (AndPat []) = error "Not supported: empty and pattern"
  toNonS (AndPat ps) = toNonS (foldr1 (\p acc -> InfixPat op p acc) ps)
    where op = fromJust $ find (\op -> repr op == "&") reservedPatternInfix
  toNonS (OrPat []) = error "Not supported: empty or pattern"
  toNonS (OrPat ps) = toNonS (foldr1 (\p acc -> InfixPat op p acc) ps)
    where op = fromJust $ find (\op -> repr op == "|") reservedPatternInfix
  toNonS (TuplePat ps) = TuplePat (map toNonS ps)
  toNonS (InductivePat name [p1, p2])
    | any (\op -> func op == name) reservedPatternInfix =
      InfixPat op (toNonS p1) (toNonS p2)
        where op = fromJust $ find (\op -> func op == name) reservedPatternInfix
  toNonS (InductivePat name ps) = InductivePat name (map toNonS ps)
  toNonS (LoopPat i range p1 p2) = LoopPat i (toNonS range) (toNonS p1) (toNonS p2)
  toNonS (PApplyPat e p) = PApplyPat (toNonS e) (map toNonS p)
  toNonS (SeqConsPat p1 p2) = SeqConsPat (toNonS p1) (toNonS p2)
  toNonS p = p

instance SyntaxElement LoopRange where
  toNonS (LoopRange e1 e2 p) = LoopRange (toNonS e1) (toNonS e2) (toNonS p)

instance SyntaxElement a => SyntaxElement (Index a) where
  toNonS script = toNonS <$> script

instance SyntaxElement InnerExpr where
  toNonS (ElementExpr x) = ElementExpr (toNonS x)
  toNonS (SubCollectionExpr _) = error "Not supported: SubCollectionExpr"

instance SyntaxElement BindingExpr where
  toNonS (vars, x) = (map toNonS vars, toNonS x)

instance SyntaxElement MatchClause where
  toNonS (pat, body) = (toNonS pat, toNonS body)

instance SyntaxElement PatternDef where
  toNonS (x, y, zs) = (x, toNonS y, map (\(z, w) -> (z, toNonS w)) zs)

instance SyntaxElement Var where
  toNonS (Var xs ys) = Var (map toCamelCase xs) ys
    where
      toCamelCase :: String -> String
      toCamelCase x@('-':_) = x
      toCamelCase x =
        let heads:tails = splitOn "-" x
         in concat $ heads : map (\(x:xs) -> toUpper x : xs) tails


main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  -- 'ast' is not desugared
  let ast = parseTopExprs input
  case ast of
    Left err ->
      print err
    Right ast -> do
      putStrLn "--"
      putStrLn "-- This file has been auto-generated by egison-translator."
      putStrLn "--"
      putStrLn ""
      putDoc $ prettyTopExprs $ map toNonS ast
      putStrLn ""
