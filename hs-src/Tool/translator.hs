module Main where

import           Data.List                             (find)
import           Data.Maybe                            (fromJust)
import           Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import           System.Environment                    (getArgs)

import           Language.Egison.Types
import           Language.Egison.Parser
import           Language.Egison.PrettyPrint

class SyntaxElement a where
  toNonS :: a -> a

instance SyntaxElement EgisonTopExpr where
  toNonS (Define x y)   = Define x (toNonS y)
  toNonS (Redefine x y) = error "Not supported"
  toNonS (Test x)       = Test (toNonS x)
  toNonS (Execute x)    = Execute (toNonS x)
  toNonS x              = x


instance SyntaxElement EgisonExpr where
  toNonS (IntegerExpr x)
    | x < 0     = UnaryOpExpr "-" (IntegerExpr (-x))
    | otherwise = IntegerExpr x
  toNonS (IndexedExpr b x ys)  = IndexedExpr  b (toNonS x) (map toNonS ys)
  toNonS (SubrefsExpr b x y)   = SubrefsExpr  b (toNonS x) (toNonS y)
  toNonS (SuprefsExpr b x y)   = SuprefsExpr  b (toNonS x) (toNonS y)
  toNonS (UserrefsExpr b x y)  = UserrefsExpr b (toNonS x) (toNonS y)
  toNonS (PowerExpr x y) = BinaryOpExpr powerOp (toNonS x) (toNonS y)
    where
      powerOp = fromJust $ find (\op -> repr op == "^") reservedBinops
  toNonS (InductiveDataExpr x ys) = InductiveDataExpr x (map toNonS ys)

  toNonS (LambdaExpr xs y)          = LambdaExpr xs (toNonS y)
  toNonS (MemoizedLambdaExpr xs y)  = MemoizedLambdaExpr xs (toNonS y)
  toNonS (CambdaExpr _ _)           = error "Not supported"
  toNonS (ProcedureExpr xs y)       = ProcedureExpr xs (toNonS y)
  toNonS (MacroExpr xs y)           = MacroExpr xs (toNonS y)
  toNonS (PatternFunctionExpr xs y) = PatternFunctionExpr xs (toNonS y)

  toNonS (IfExpr x y z) = IfExpr (toNonS x) (toNonS y) (toNonS z)

  toNonS (ApplyExpr x@(VarExpr (Var [f] [])) (TupleExpr [y, z])) =
    case find (\op -> repr op == f) reservedBinops of
      Just op -> BinaryOpExpr op (toNonS y) (toNonS z)
      Nothing -> ApplyExpr x (TupleExpr [toNonS y, toNonS z])

  toNonS x = x

instance SyntaxElement EgisonPattern where
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
