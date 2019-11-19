{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Arrow                         ((***))
import           Data.Char                             (toUpper)
import           Data.List                             (find)
import           Data.List.Split                       (splitOn)
import           Data.Maybe                            (fromJust)
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           System.Environment                    (getArgs)

import           Language.Egison.Types
import           Language.Egison.Parser
import           Language.Egison.Pretty

class SyntaxElement a where
  toNonS :: a -> a

instance SyntaxElement EgisonTopExpr where
  toNonS (Define x y)   = Define (toNonS x) (toNonS y)
  toNonS (Redefine x y) = error "Not supported"
  toNonS (Test x)       = Test (toNonS x)
  toNonS (Execute x)    = Execute (toNonS x)
  toNonS x              = x

instance SyntaxElement EgisonExpr where
  toNonS (IntegerExpr x)
    | x < 0     = UnaryOpExpr "-" (IntegerExpr (-x))
    | otherwise = IntegerExpr x
  toNonS (VarExpr x) = VarExpr (toNonS x)

  toNonS (IndexedExpr b x ys)  = IndexedExpr  b (toNonS x) (map toNonS ys)
  toNonS (SubrefsExpr b x y)   = SubrefsExpr  b (toNonS x) (toNonS y)
  toNonS (SuprefsExpr b x y)   = SuprefsExpr  b (toNonS x) (toNonS y)
  toNonS (UserrefsExpr b x y)  = UserrefsExpr b (toNonS x) (toNonS y)
  toNonS (PowerExpr x y) = BinaryOpExpr powerOp (toNonS x) (toNonS y)
    where powerOp = fromJust $ find (\op -> repr op == "^") reservedBinops
  toNonS (InductiveDataExpr x ys) = InductiveDataExpr x (map toNonS ys)
  toNonS (TupleExpr xs)      = TupleExpr (map toNonS xs)
  toNonS (CollectionExpr xs) = CollectionExpr (map toNonS xs)
  toNonS (ArrayExpr xs)      = ArrayExpr (map toNonS xs)
  toNonS (HashExpr xs)       = HashExpr (map (toNonS *** toNonS) xs)
  toNonS (VectorExpr xs)     = VectorExpr (map toNonS xs)

  toNonS (LambdaExpr xs y)          = LambdaExpr xs (toNonS y)
  toNonS (MemoizedLambdaExpr xs y)  = MemoizedLambdaExpr xs (toNonS y)
  toNonS (CambdaExpr _ _)           = error "Not supported"
  toNonS (ProcedureExpr xs y)       = ProcedureExpr xs (toNonS y)
  toNonS (MacroExpr xs y)           = MacroExpr xs (toNonS y)
  -- PatternFunctionExpr

  toNonS (IfExpr x y z)         = IfExpr (toNonS x) (toNonS y) (toNonS z)
  toNonS (LetRecExpr xs y)      = LetRecExpr (map toNonS xs) (toNonS y)
  toNonS (LetExpr xs y)         = LetRecExpr (map toNonS xs) (toNonS y)
  toNonS (LetStarExpr xs y)     = LetRecExpr (map toNonS xs) (toNonS y)
  toNonS (WithSymbolsExpr xs y) = WithSymbolsExpr xs (toNonS y)

  toNonS (MatchExpr x y zs)        = MatchExpr       (toNonS x) (toNonS y) (map toNonS zs)
  toNonS (MatchDFSExpr x y zs)     = MatchDFSExpr    (toNonS x) (toNonS y) (map toNonS zs)
  toNonS (MatchAllExpr x y zs)     = MatchAllExpr    (toNonS x) (toNonS y) (map toNonS zs)
  toNonS (MatchAllDFSExpr x y zs)  = MatchAllDFSExpr (toNonS x) (toNonS y) (map toNonS zs)
  toNonS (MatchLambdaExpr x ys)    = MatchLambdaExpr    (toNonS x) (map toNonS ys)
  toNonS (MatchAllLambdaExpr x ys) = MatchAllLambdaExpr (toNonS x) (map toNonS ys)

  toNonS (MatcherExpr xs) = MatcherExpr (map toNonS xs)

  toNonS (QuoteExpr x)        = QuoteExpr (toNonS x)
  toNonS (QuoteSymbolExpr x)  = QuoteSymbolExpr (toNonS x)
  toNonS (WedgeApplyExpr x y) = WedgeApplyExpr (toNonS x) (toNonS y)

  toNonS (DoExpr xs y) = DoExpr (map toNonS xs) (toNonS y)
  toNonS (IoExpr x)    = IoExpr (toNonS x)

  toNonS (ApplyExpr x@(VarExpr (Var [f] [])) (TupleExpr (y:ys)))
    | any (\op -> repr op == f) reservedBinops =
      foldl (\acc x -> BinaryOpExpr op acc (toNonS x)) (toNonS y) ys
      where
        op = fromJust $ find (\op -> repr op == f) reservedBinops
  toNonS (ApplyExpr x y) = ApplyExpr (toNonS x) (toNonS y)

  toNonS x = x

instance SyntaxElement a => SyntaxElement (Index a) where
  toNonS (Subscript x)          = Subscript (toNonS x)
  toNonS (Superscript x)        = Superscript (toNonS x)
  toNonS (SupSubscript x)       = SupSubscript (toNonS x)
  toNonS (MultiSubscript x y)   = MultiSubscript (toNonS x) (toNonS y)
  toNonS (MultiSuperscript x y) = MultiSuperscript (toNonS x) (toNonS y)
  toNonS (Userscript x)         = Userscript (toNonS x)
  toNonS (DotSubscript x)       = DotSubscript (toNonS x)
  toNonS (DotSupscript x)       = DotSupscript (toNonS x)
  toNonS x = x -- DFScript

instance SyntaxElement InnerExpr where
  toNonS (ElementExpr x) = ElementExpr (toNonS x)
  toNonS (SubCollectionExpr _) = error "Not supported"

instance SyntaxElement BindingExpr where
  toNonS (vars, x) = (map toNonS vars, toNonS x)

instance SyntaxElement MatchClause where
  toNonS (x, y) = (x, toNonS y)

instance SyntaxElement PatternDef where
  toNonS (x, y, zs) = (x, toNonS y, map (\(z, w) -> (z, toNonS w)) zs)

instance SyntaxElement Var where
  toNonS (Var xs ys) = Var (map toCamelCase xs) ys
    where
      toCamelCase :: String -> String
      toCamelCase x =
        let heads:tails = splitOn "-" x
         in concat $ heads : map (\ (x:xs) -> toUpper x : xs) tails

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args
  -- 'ast' is not desugared
  let ast = parseTopExprs input
  case ast of
    Left _ -> return ()
    Right ast -> do
      putDoc $ prettyTopExprs $ map toNonS ast
      putStrLn ""
