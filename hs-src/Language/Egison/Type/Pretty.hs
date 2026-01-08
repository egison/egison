{- |
Module      : Language.Egison.Type.Pretty
Licence     : MIT

This module provides pretty printing for Egison types.
-}

module Language.Egison.Type.Pretty
  ( prettyType
  , prettyTypeScheme
  , prettyTypeExpr
  , prettyTensorShape
  , prettyIndex
  ) where

import           Data.List                  (intercalate)

import           Language.Egison.AST        (ShapeDim (..), TensorIndexExpr (..), TensorShapeExpr (..),
                                             TypeExpr (..))
import           Language.Egison.Type.Types (Constraint(..))
import           Language.Egison.Type.Index (Index (..), IndexKind (..), IndexSpec)
import           Language.Egison.Type.Types (ShapeDimType (..), TensorShape (..), TyVar (..), Type (..),
                                             TypeScheme (..))

-- | Pretty print a Type
prettyType :: Type -> String
prettyType TInt             = "Integer"
prettyType TFloat           = "Float"
prettyType TBool            = "Bool"
prettyType TChar            = "Char"
prettyType TString          = "String"
prettyType (TTuple [])      = "()"
prettyType (TVar (TyVar v)) = v
prettyType (TTuple [])      = "()"
prettyType (TTuple ts)      = "(" ++ intercalate ", " (map prettyType ts) ++ ")"
prettyType (TCollection t)  = "[" ++ prettyType t ++ "]"
prettyType (TInductive name []) = name
prettyType (TInductive name args) = name ++ " " ++ unwords (map prettyTypeAtom args)
prettyType (TTensor t)      = "Tensor " ++ prettyTypeAtom t
prettyType (THash k v)      = "Hash " ++ prettyTypeAtom k ++ " " ++ prettyTypeAtom v
prettyType (TMatcher t)     = "Matcher " ++ prettyTypeAtom t
prettyType (TFun t1 t2)     = prettyTypeArg t1 ++ " -> " ++ prettyType t2
  where
    prettyTypeArg t@(TFun _ _) = "(" ++ prettyType t ++ ")"
    prettyTypeArg t            = prettyType t
prettyType (TIO t)          = "IO " ++ prettyTypeAtom t
prettyType (TIORef t)       = "IORef " ++ prettyTypeAtom t
prettyType TAny             = "_"

-- | Pretty print an atomic type (with parentheses if needed)
prettyTypeAtom :: Type -> String
prettyTypeAtom t@TInt       = prettyType t
prettyTypeAtom t@TFloat     = prettyType t
prettyTypeAtom t@TBool      = prettyType t
prettyTypeAtom t@TChar      = prettyType t
prettyTypeAtom t@TString    = prettyType t
prettyTypeAtom t@(TTuple []) = prettyType t
prettyTypeAtom t@(TVar _)    = prettyType t
prettyTypeAtom t@(TTuple _) = prettyType t
prettyTypeAtom t@(TCollection _) = prettyType t
prettyTypeAtom t            = "(" ++ prettyType t ++ ")"

-- | Pretty print a TypeScheme
prettyTypeScheme :: TypeScheme -> String
prettyTypeScheme (Forall [] [] t) = prettyType t
prettyTypeScheme (Forall [] cs t) =
  prettyConstraints cs ++ " => " ++ prettyType t
prettyTypeScheme (Forall vs [] t) =
  "∀" ++ unwords (map (\(TyVar v) -> v) vs) ++ ". " ++ prettyType t
prettyTypeScheme (Forall vs cs t) =
  "∀" ++ unwords (map (\(TyVar v) -> v) vs) ++ ". " ++ prettyConstraints cs ++ " => " ++ prettyType t

-- | Pretty print constraints
prettyConstraints :: [Constraint] -> String
prettyConstraints []  = ""
prettyConstraints [c] = prettyConstraint c
prettyConstraints cs  = "(" ++ intercalate ", " (map prettyConstraint cs) ++ ")"

-- | Pretty print a single constraint
prettyConstraint :: Constraint -> String
prettyConstraint (Constraint cls ty) = cls ++ " " ++ prettyTypeAtom ty

-- | Pretty print a TensorShape
prettyTensorShape :: TensorShape -> String
prettyTensorShape (ShapeLit dims) = "[" ++ intercalate ", " (map show dims) ++ "]"
prettyTensorShape (ShapeVar v)    = v
prettyTensorShape (ShapeMixed dims) = "[" ++ intercalate ", " (map prettyShapeDimType dims) ++ "]"
prettyTensorShape ShapeUnknown    = "[?]"

-- | Pretty print a ShapeDimType
prettyShapeDimType :: ShapeDimType -> String
prettyShapeDimType (DimLit n) = show n
prettyShapeDimType (DimVar v) = v

-- | Pretty print an IndexSpec
prettyIndices :: IndexSpec -> String
prettyIndices = concatMap prettyIndex

-- | Pretty print an Index
prettyIndex :: Index -> String
prettyIndex (IndexSym Subscript s)      = "_" ++ s
prettyIndex (IndexSym Superscript s)    = "~" ++ s
prettyIndex (IndexPlaceholder Subscript)    = "_#"
prettyIndex (IndexPlaceholder Superscript)  = "~#"
prettyIndex (IndexVar s)                = "_" ++ s

-- | Pretty print a TypeExpr (source-level type)
prettyTypeExpr :: TypeExpr -> String
prettyTypeExpr TEInt          = "Integer"
prettyTypeExpr TEMathExpr     = "MathExpr"
prettyTypeExpr TEFloat        = "Float"
prettyTypeExpr TEBool         = "Bool"
prettyTypeExpr TEChar         = "Char"
prettyTypeExpr TEString       = "String"
prettyTypeExpr (TEVar v)      = v
prettyTypeExpr (TEList t)     = "[" ++ prettyTypeExpr t ++ "]"
prettyTypeExpr (TETuple [])   = "()"
prettyTypeExpr (TETuple ts)   = "(" ++ intercalate ", " (map prettyTypeExpr ts) ++ ")"
prettyTypeExpr (TEFun t1 t2)  = prettyTypeExprArg t1 ++ " -> " ++ prettyTypeExpr t2
  where
    prettyTypeExprArg t@(TEFun _ _) = "(" ++ prettyTypeExpr t ++ ")"
    prettyTypeExprArg t             = prettyTypeExpr t
prettyTypeExpr (TEMatcher t)  = "Matcher " ++ prettyTypeExprAtom t
prettyTypeExpr (TEPattern t)  = "Pattern " ++ prettyTypeExprAtom t
prettyTypeExpr (TETensor t sh is) =
  "Tensor " ++ prettyTypeExprAtom t ++ " " ++ prettyShapeExpr sh ++ prettyIndexExprs is
prettyTypeExpr (TEApp t args) =
  prettyTypeExprAtom t ++ " " ++ unwords (map prettyTypeExprAtom args)

-- | Pretty print an atomic TypeExpr
prettyTypeExprAtom :: TypeExpr -> String
prettyTypeExprAtom t@TEInt       = prettyTypeExpr t
prettyTypeExprAtom t@TEMathExpr  = prettyTypeExpr t
prettyTypeExprAtom t@TEFloat     = prettyTypeExpr t
prettyTypeExprAtom t@TEBool      = prettyTypeExpr t
prettyTypeExprAtom t@TEChar      = prettyTypeExpr t
prettyTypeExprAtom t@TEString    = prettyTypeExpr t
prettyTypeExprAtom t@(TEVar _)   = prettyTypeExpr t
prettyTypeExprAtom t@(TEList _)  = prettyTypeExpr t
prettyTypeExprAtom t@(TETuple _) = prettyTypeExpr t
prettyTypeExprAtom t             = "(" ++ prettyTypeExpr t ++ ")"

-- | Pretty print a TensorShapeExpr
prettyShapeExpr :: TensorShapeExpr -> String
prettyShapeExpr (TSLit dims) = "[" ++ intercalate ", " (map show dims) ++ "]"
prettyShapeExpr (TSVar v)    = v
prettyShapeExpr (TSMixed dims) = "[" ++ intercalate ", " (map prettyShapeDim dims) ++ "]"

-- | Pretty print a ShapeDim
prettyShapeDim :: ShapeDim -> String
prettyShapeDim (SDLit n) = show n
prettyShapeDim (SDVar v) = v

-- | Pretty print TensorIndexExprs
prettyIndexExprs :: [TensorIndexExpr] -> String
prettyIndexExprs = concatMap prettyIndexExpr
  where
    prettyIndexExpr (TISub s)        = "_" ++ s
    prettyIndexExpr (TISup s)        = "~" ++ s
    prettyIndexExpr TIPlaceholderSub = "_#"
    prettyIndexExpr TIPlaceholderSup = "~#"

