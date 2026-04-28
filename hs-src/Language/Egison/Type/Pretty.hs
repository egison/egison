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

import           Language.Egison.AST        (TypeExpr (..), SymbolSetExpr(..), TypeAtomExpr(..))
import           Language.Egison.Type.Types (Constraint(..))
import           Language.Egison.Type.Index (Index (..), IndexKind (..))
import           Language.Egison.Type.Types (ShapeDimType (..), TensorShape (..), TyVar (..), Type (..),
                                             TypeScheme (..), SymbolSet(..), prettyTypeAtomValue)

-- | Pretty print a Type
prettyType :: Type -> String
prettyType TInt             = "Integer"
prettyType TMathValue        = "MathValue"
prettyType TPolyExpr        = "PolyExpr"
prettyType TTermExpr        = "TermExpr"
prettyType TSymbolExpr      = "SymbolExpr"
prettyType TIndexExpr       = "IndexExpr"
prettyType TFloat           = "Float"
prettyType TBool            = "Bool"
prettyType TChar            = "Char"
prettyType TString          = "String"
prettyType (TTuple [])      = "()"
prettyType (TVar (TyVar v)) = v
prettyType (TTuple ts)      = "(" ++ intercalate ", " (map prettyType ts) ++ ")"
prettyType (TCollection t)  = "[" ++ prettyType t ++ "]"
prettyType (TInductive name []) = name
prettyType (TInductive name args) = name ++ " " ++ unwords (map prettyTypeAtom args)
prettyType (TTensor t)      = "Tensor " ++ prettyTypeAtom t
prettyType (THash k v)      = "Hash " ++ prettyTypeAtom k ++ " " ++ prettyHashValueType v
  where
    -- Hash value types need parentheses if they are function types
    prettyHashValueType t@(TFun _ _) = "(" ++ prettyType t ++ ")"
    prettyHashValueType t            = prettyTypeAtom t
prettyType (TMatcher t)     = "Matcher " ++ prettyTypeAtom t
prettyType (TFun t1 t2)     = prettyTypeArg t1 ++ " -> " ++ prettyType t2
  where
    prettyTypeArg t@(TFun _ _) = "(" ++ prettyType t ++ ")"
    prettyTypeArg t            = prettyType t
prettyType (TIO t)          = "IO " ++ prettyTypeAtom t
prettyType (TIORef t)       = "IORef " ++ prettyTypeAtom t
prettyType TPort            = "Port"
prettyType TAny             = "_"
-- New CAS types
prettyType TFactor          = "Factor"
prettyType (TFrac t)         = "Frac " ++ prettyTypeAtom t
prettyType (TPoly t ss)     = "Poly " ++ prettyTypeAtom t ++ " " ++ prettySymbolSet ss

-- | Pretty print a SymbolSet
prettySymbolSet :: SymbolSet -> String
prettySymbolSet (SymbolSetClosed syms) = "[" ++ intercalate ", " (map prettyTypeAtomValue syms) ++ "]"
prettySymbolSet SymbolSetOpen          = "[..]"
prettySymbolSet (SymbolSetVar (TyVar v)) = v

-- | Pretty print an atomic type (with parentheses if needed)
prettyTypeAtom :: Type -> String
prettyTypeAtom t@TInt       = prettyType t
prettyTypeAtom t@TMathValue  = prettyType t
prettyTypeAtom t@TPolyExpr  = prettyType t
prettyTypeAtom t@TTermExpr  = prettyType t
prettyTypeAtom t@TSymbolExpr = prettyType t
prettyTypeAtom t@TIndexExpr = prettyType t
prettyTypeAtom t@TFloat     = prettyType t
prettyTypeAtom t@TBool      = prettyType t
prettyTypeAtom t@TChar      = prettyType t
prettyTypeAtom t@TString    = prettyType t
prettyTypeAtom t@(TTuple []) = prettyType t
prettyTypeAtom t@(TVar _)    = prettyType t
prettyTypeAtom t@(TTuple _) = prettyType t
prettyTypeAtom t@(TCollection _) = prettyType t
prettyTypeAtom t@TPort       = prettyType t
prettyTypeAtom t@TAny        = prettyType t
prettyTypeAtom t@TFactor     = prettyType t
prettyTypeAtom t            = "(" ++ prettyType t ++ ")"

-- | Pretty print a TypeScheme
prettyTypeScheme :: TypeScheme -> String
prettyTypeScheme (Forall [] [] t) = prettyType t
prettyTypeScheme (Forall [] cs t) =
  prettyConstraintsAlt cs ++ " " ++ prettyType t
prettyTypeScheme (Forall vs [] t) =
  "∀" ++ unwords (map (\(TyVar v) -> v) vs) ++ ". " ++ prettyType t
prettyTypeScheme (Forall vs cs t) =
  prettyConstraintsAlt cs ++ " " ++ prettyType t

-- | Pretty print constraints (old format: "Eq a, Ord b")
prettyConstraints :: [Constraint] -> String
prettyConstraints []  = ""
prettyConstraints [c] = prettyConstraint c
prettyConstraints cs  = "(" ++ intercalate ", " (map prettyConstraint cs) ++ ")"

-- | Pretty print constraints (new format: "{Eq a, Ord b}")
prettyConstraintsAlt :: [Constraint] -> String
prettyConstraintsAlt []  = ""
prettyConstraintsAlt cs  = "{" ++ intercalate ", " (map prettyConstraint cs) ++ "}"

-- | Pretty print a single constraint
prettyConstraint :: Constraint -> String
prettyConstraint (Constraint cls tys) = cls ++ concatMap (\t -> " " ++ prettyTypeAtom t) tys

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
prettyTypeExpr TEMathValue     = "MathValue"
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
prettyTypeExpr (TETensor t) = "Tensor " ++ prettyTypeExprAtom t
prettyTypeExpr (TEApp t args) =
  prettyTypeExprAtom t ++ " " ++ unwords (map prettyTypeExprAtom args)
prettyTypeExpr (TEIO t) = "IO " ++ prettyTypeExprAtom t
prettyTypeExpr (TEVector t) = "Vector " ++ prettyTypeExprAtom t
prettyTypeExpr (TEMatrix t) = "Matrix " ++ prettyTypeExprAtom t
prettyTypeExpr (TEDiffForm t) = "DiffForm " ++ prettyTypeExprAtom t
prettyTypeExpr (TEConstrained cs t) = prettyConstraintExprs cs ++ " " ++ prettyTypeExpr t
  where
    prettyConstraintExprs [] = ""
    prettyConstraintExprs constraints = "{" ++ intercalate ", " (map prettyConstraintExpr constraints) ++ "}"
    prettyConstraintExpr _ = "..."  -- TODO: implement constraint printing
-- New CAS types
prettyTypeExpr TEFactor = "Factor"
prettyTypeExpr (TEFrac t) = "Frac " ++ prettyTypeExprAtom t
prettyTypeExpr (TEPoly t ss) = "Poly " ++ prettyTypeExprAtom t ++ " " ++ prettySymbolSetExpr ss

-- | Pretty print a SymbolSetExpr
prettySymbolSetExpr :: SymbolSetExpr -> String
prettySymbolSetExpr (SSEClosed syms) = "[" ++ intercalate ", " (map prettyTypeAtomExpr syms) ++ "]"
  where
    prettyTypeAtomExpr (TAEName s)        = s
    prettyTypeAtomExpr (TAEInt n)         = show n
    prettyTypeAtomExpr (TAEApp fn args)   = unwords (fn : map prettyAtomExprArg args)
    prettyAtomExprArg a@(TAEApp _ _) = "(" ++ prettyTypeAtomExpr a ++ ")"
    prettyAtomExprArg a              = prettyTypeAtomExpr a
prettySymbolSetExpr SSEOpen          = "[..]"

-- | Pretty print an atomic TypeExpr
prettyTypeExprAtom :: TypeExpr -> String
prettyTypeExprAtom t@TEInt       = prettyTypeExpr t
prettyTypeExprAtom t@TEMathValue  = prettyTypeExpr t
prettyTypeExprAtom t@TEFloat     = prettyTypeExpr t
prettyTypeExprAtom t@TEBool      = prettyTypeExpr t
prettyTypeExprAtom t@TEChar      = prettyTypeExpr t
prettyTypeExprAtom t@TEString    = prettyTypeExpr t
prettyTypeExprAtom t@(TEVar _)   = prettyTypeExpr t
prettyTypeExprAtom t@(TEList _)  = prettyTypeExpr t
prettyTypeExprAtom t@(TETuple _) = prettyTypeExpr t
prettyTypeExprAtom t@TEFactor    = prettyTypeExpr t
prettyTypeExprAtom t             = "(" ++ prettyTypeExpr t ++ ")"

