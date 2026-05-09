{-# LANGUAGE DeriveTraversable #-}

{- |
Module      : Language.Egison.AST
Licence     : MIT

This module defines the syntax of Egison.
-}

module Language.Egison.AST
  ( TopExpr (..)
  , RuleLevel (..)
  , ConstantExpr (..)
  , Expr (..)
  , Pattern (..)
  , VarWithIndices (..)
  , makeApply
  , Arg (..)
  , ArgPattern (..)
  , IndexExpr (..)
  , VarIndex (..)
  , PMMode (..)
  , BindingExpr (..)
  , MatchClause
  , PatternDef (..)
  , LoopRange (..)
  , PrimitivePatPattern (..)
  , PDPatternBase (..)
  , PrimitiveDataPattern
  , Op (..)
  , Assoc (..)
  , reservedExprOp
  , reservedPatternOp
  , findOpFrom
  , stringToVarWithIndices
  , extractNameFromVarWithIndices
  -- Type annotations
  , TypeExpr (..)
  , SymbolSetExpr (..)
  , TypeAtomExpr (..)
  , TensorShapeExpr (..)
  , ShapeDim (..)
  , TensorIndexExpr (..)
  , TypedParam (..)
  , TypedVarWithIndices (..)
  -- Inductive data types
  , InductiveConstructor (..)
  -- Pattern inductive types
  , PatternConstructor (..)
  -- Type classes
  , ClassDecl (..)
  , ClassMethod (..)
  , InstanceDecl (..)
  , InstanceMethod (..)
  , ConstraintExpr (..)
  ) where

import           Data.List  (find)
import           Data.Maybe (fromJust)
import           Data.Text  (Text)

data TopExpr
  = Define VarWithIndices Expr
  | DefineWithType TypedVarWithIndices Expr  -- ^ Definition with type annotation
  | Test Expr
  | Execute Expr
    -- temporary : we will replace load to import and export
  | LoadFile String
  | Load String
  | InfixDecl Bool Op -- True for pattern infix; False for expression infix
  | InductiveDecl String [String] [InductiveConstructor]
    -- ^ Inductive data type declaration with type parameters
    -- e.g., inductive Ordering := | Less | Equal | Greater
    --       inductive Maybe a := | Nothing | Just a
    -- String: type name, [String]: type parameters, [InductiveConstructor]: constructors
  | ClassDeclExpr ClassDecl
    -- ^ Type class declaration
    -- e.g., class Eq a where (==) (x: a) (y: a) : Bool
  | InstanceDeclExpr InstanceDecl
    -- ^ Type class instance declaration
    -- e.g., instance Eq Integer where (==) x y := x = y
  | PatternInductiveDecl String [String] [PatternConstructor]
    -- ^ Pattern inductive type declaration
    -- e.g., inductive pattern MyList a := | myNil | myCons a (MyList a)
    -- String: pattern type name, [String]: type parameters, [PatternConstructor]: constructors
  | PatternFunctionDecl String [String] [(String, TypeExpr)] TypeExpr Pattern
    -- ^ Pattern function declaration
    -- e.g., def pattern twin {a} (p1 : a) (p2 : MyList a) : MyList a := ...
    -- String: function name, [String]: type parameters, [(String, TypeExpr)]: parameters, TypeExpr: return type, Pattern: body
  | DeclareSymbol [String] (Maybe TypeExpr)
    -- ^ Symbol declaration
    -- e.g., declare symbol a11, a12, a21, a22
    --       declare symbol x, y, z : Float
    -- [String]: symbol names, Maybe TypeExpr: optional type (defaults to Integer)
  | DeclareRule (Maybe String) RuleLevel Pattern Expr
    -- ^ Reduction rule declaration (Phase 7.4 of type-cas design).
    -- e.g.  declare rule auto term i^2 = -1
    --       declare rule trig_pythagorean poly (sin $x)^2 + (cos #x)^2 = 1
    -- Maybe String:  rule name (Nothing = auto rule)
    -- RuleLevel:     where the LHS pattern binds (term/poly/frac)
    -- Pattern Expr:  LHS pattern (with $x/#x), RHS expression
  | DeclareDerivative String Expr
    -- ^ Derivative declaration (Phase 6.3 of type-cas design).
    -- e.g.  declare derivative sin = cos
    --       declare derivative log = \x -> 1 / x
    -- String: name of the function whose derivative is being declared
    --         (typically introduced by `declare mathfunc` first).
    -- Expr:   the derivative-as-an-expression (typically a function or lambda)
  | DeclareMathFunc String (Maybe TypeExpr)
    -- ^ Math function declaration (Phase 6.3 part 5).
    -- e.g.  declare mathfunc sin
    --       declare mathfunc sqrt : MathValue -> MathValue
    -- Desugars to a wrapper function that quotes the symbol:
    --   def <name> (x : MathValue) : MathValue := '<name> x
    -- Combined with `declare derivative`, this gives the user a callable
    -- function and a registered derivative under one umbrella.
  | DeclareApply String [String] Expr
    -- ^ Math function application rule (Phase A of declare apply impl).
    -- e.g.  declare apply sin x := if x = 0 then 0 else 'sin x
    --       declare apply sqrt x := ...
    -- String:    function name (must have been declared via `declare mathfunc`).
    -- [String]:  argument names (typed as MathValue by default).
    -- Expr:      body. Within body, `'<name> x` produces the symbolic Factor
    --            (no recursion); `<name> x` (unquoted) recurses through this
    --            rule again — RHS must use `'` for the fallback to terminate.
    -- Phase A desugars to a plain `def <name> (args : MathValue ...) : MathValue := <body>`
    -- which simply overrides the wrapper from `declare mathfunc`.
 deriving Show

-- | Where in the CASValue tree a `declare rule` LHS pattern binds.
data RuleLevel
  = TermRuleLevel  -- ^ inside a CASTerm monomial (e.g. i^2 = -1)
  | PolyRuleLevel  -- ^ inside a CASPoly term-list  (e.g. (sin x)^2 + (cos x)^2 = 1)
  | FracRuleLevel  -- ^ on a CASFrac  numerator/denominator
  deriving (Show, Eq)

-- | Type class declaration
-- e.g., class Eq a where ...
--       class Eq a => Ord a where ...
data ClassDecl = ClassDecl
  { className       :: String           -- ^ Class name (e.g., "Eq", "Ord")
  , classTypeParams :: [String]         -- ^ Type parameters (e.g., ["a"])
  , classSuperclasses :: [ConstraintExpr] -- ^ Superclass constraints (e.g., [Eq a] for Ord)
  , classMethods    :: [ClassMethod]    -- ^ Method declarations
  } deriving Show

-- | Type class method declaration
-- e.g., (==) (x: a) (y: a) : Bool
--       (/=) (x: a) (y: a) : Bool := not (x == y)
data ClassMethod = ClassMethod
  { methodName    :: String             -- ^ Method name (e.g., "==")
  , methodParams  :: [TypedParam]       -- ^ Method parameters with types
  , methodRetType :: TypeExpr           -- ^ Return type
  , methodDefault :: Maybe Expr         -- ^ Optional default implementation
  } deriving Show

-- | Type class instance declaration
-- e.g., instance Eq Integer where ...
--       instance Eq a => Eq [a] where ...
data InstanceDecl = InstanceDecl
  { instanceConstraints :: [ConstraintExpr] -- ^ Instance constraints (e.g., [Eq a] for Eq [a])
  , instanceClass       :: String           -- ^ Class name (e.g., "Eq")
  , instanceTypes       :: [TypeExpr]       -- ^ Instance types (e.g., [Integer] or [[a]])
  , instanceMethods     :: [InstanceMethod] -- ^ Method implementations
  } deriving Show

-- | Instance method implementation
-- e.g., (==) x y := x = y
data InstanceMethod = InstanceMethod
  { instMethodName   :: String          -- ^ Method name
  , instMethodParams :: [String]        -- ^ Parameter names
  , instMethodBody   :: Expr            -- ^ Method body
  } deriving Show

-- | Type constraint expression
-- e.g., Eq a, Ord a
data ConstraintExpr = ConstraintExpr
  { constraintClass :: String           -- ^ Class name
  , constraintTypes :: [TypeExpr]       -- ^ Type arguments
  } deriving (Show, Eq)

-- | Constructor for inductive data type
-- e.g., Less, S Nat, Node Tree Tree
data InductiveConstructor = InductiveConstructor
  { inductiveCtorName :: String      -- ^ Constructor name (e.g., "Less", "S", "Node")
  , inductiveCtorArgs :: [TypeExpr]  -- ^ Constructor argument types (e.g., [], [Nat], [Tree, Tree])
  } deriving (Show, Eq)

-- | Constructor for pattern inductive type
-- e.g., myNil, myCons a (MyList a)
data PatternConstructor = PatternConstructor
  { patternCtorName :: String      -- ^ Pattern constructor name (e.g., "myNil", "myCons")
  , patternCtorArgs :: [TypeExpr]  -- ^ Pattern constructor argument types (e.g., [], [a, MyList a])
  } deriving (Show, Eq)

data ConstantExpr
  = CharExpr Char
  | StringExpr Text
  | BoolExpr Bool
  | IntegerExpr Integer
  | FloatExpr Double
  | SomethingExpr
  | UndefinedExpr
  deriving Show

data Expr
  = ConstantExpr ConstantExpr
  | VarExpr String
  | FreshVarExpr
  | IndexedExpr Bool Expr [IndexExpr Expr]  -- True -> delete old index and append new one
  | SubrefsExpr Bool Expr Expr
  | SuprefsExpr Bool Expr Expr
  | UserrefsExpr Bool Expr Expr
  | TupleExpr [Expr]
  | CollectionExpr [Expr]
  | ConsExpr Expr Expr
  | JoinExpr Expr Expr
  | HashExpr [(Expr, Expr)]
  | VectorExpr [Expr]

  | LambdaExpr [Arg ArgPattern] Expr
  | LambdaExpr' [Arg VarWithIndices] Expr
  | TypedLambdaExpr [(String, TypeExpr)] TypeExpr Expr  -- ^ Lambda with typed parameters and return type
  | MemoizedLambdaExpr [String] Expr
  | TypedMemoizedLambdaExpr [TypedParam] TypeExpr Expr  -- ^ Memoized lambda with typed parameters
  | CambdaExpr String Expr
  | PatternFunctionExpr [String] Pattern

  | IfExpr Expr Expr Expr
  | LetExpr [BindingExpr] Expr
  | LetRecExpr [BindingExpr] Expr
  | WithSymbolsExpr [String] Expr

  | MatchExpr PMMode Expr Expr [MatchClause]
  | MatchAllExpr PMMode Expr Expr [MatchClause]
  | MatchLambdaExpr Expr [MatchClause]
  | MatchAllLambdaExpr Expr [MatchClause]

  | MatcherExpr [PatternDef]
  | AlgebraicDataMatcherExpr [(String, [Expr])]

  | QuoteExpr Expr
  | QuoteSymbolExpr Expr
  | WedgeApplyExpr Expr [Expr]

  | DoExpr [BindingExpr] Expr

  | PrefixExpr String Expr
  | InfixExpr Op Expr Expr
  | SectionExpr Op (Maybe Expr) (Maybe Expr) -- There cannot be 'SectionExpr op (Just _) (Just _)'

  | SeqExpr Expr Expr
  | ApplyExpr Expr [Expr]
  | AnonParamFuncExpr Integer Expr      -- e.g. 2#2, 3#$1, 2#($1 + $2)
  | AnonTupleParamFuncExpr Integer Expr -- e.g. (2)#2, (3)#$1, (2)#($1 + $2)
  | AnonListParamFuncExpr Integer Expr  -- e.g. [2]#2, [3]#$1, [2]#($1 + $2)
  | AnonParamExpr Integer               -- e.g. $1, $2

  | GenerateTensorExpr Expr Expr
  | TensorExpr Expr Expr
  | TensorContractExpr Expr
  | TensorMapExpr Expr Expr
  | TensorMap2Expr Expr Expr Expr
  | TransposeExpr Expr Expr
  | FlipIndicesExpr Expr

  | FunctionExpr [String]

  | TypeAnnotation Expr TypeExpr  -- ^ Expression with type annotation (expr : type)
  | SimplifyUsingExpr Expr String -- ^ Phase 7.6: `simplify <expr> using <ruleName>`. Skeleton: parser only — runtime semantics will appear when the rule-application engine lands.
  deriving Show

data VarWithIndices = VarWithIndices String [VarIndex]
  deriving (Show, Eq)

data Arg a
  = Arg a
  | InvertedArg a
  deriving Show

data ArgPattern
  = APWildCard
  | APPatVar VarWithIndices
  | APInductivePat String [Arg ArgPattern]
  | APTuplePat [Arg ArgPattern]
  | APEmptyPat
  | APConsPat (Arg ArgPattern) ArgPattern
  | APSnocPat ArgPattern (Arg ArgPattern)
  deriving Show

data VarIndex
  = VSubscript String
  | VSuperscript String
  | VMultiSubscript String Integer String   -- _(a_1)..._(a_n) -> VMultiSubscript "a" 1 "n"
  | VMultiSuperscript String Integer String -- ~(a_1)...~(a_n) -> VMultiSuperscript "a" 1 "n"
  | VGroupScripts [VarIndex]
  | VSymmScripts [VarIndex]
  | VAntiSymmScripts [VarIndex]
  deriving (Show, Eq)

data IndexExpr a
  = Subscript a
  | Superscript a
  | SupSubscript a
  | MultiSubscript a a
  | MultiSuperscript a a
  | Userscript a
  deriving (Show, Eq, Functor, Foldable, Traversable)

data PMMode = BFSMode | DFSMode
  deriving Show

data BindingExpr
  = Bind PrimitiveDataPattern Expr
  | BindWithIndices VarWithIndices Expr
  | BindWithType TypedVarWithIndices Expr  -- ^ Binding with type annotation (for where clauses)
  deriving Show

type MatchClause = (Pattern, Expr)

-- | Pattern definition in a matcher (with optional type class constraints)
data PatternDef = PatternDef
  { patDefPattern     :: PrimitivePatPattern
  , patDefMatcher     :: Expr
  , patDefClauses     :: [(PrimitiveDataPattern, Expr)]
  } deriving Show

data Pattern
  = WildCard
  | PatVar String
  | ValuePat Expr
  | PredPat Expr
  | IndexedPat Pattern [Expr]
  | LetPat [BindingExpr] Pattern
  | InfixPat Op Pattern Pattern -- Includes AndPat,OrPat,InductivePat(cons/join)
  | NotPat Pattern
  | AndPat Pattern Pattern
  | OrPat Pattern Pattern
  | ForallPat Pattern Pattern
  | TuplePat [Pattern]
  | InductivePat String [Pattern]
  | LoopPat String LoopRange Pattern Pattern
  | ContPat
  | PApplyPat Expr [Pattern]
  | VarPat String
  | InductiveOrPApplyPat String [Pattern]
  | SeqNilPat
  | SeqConsPat Pattern Pattern
  | LaterPatVar
  -- For symbolic computing
  | DApplyPat Pattern [Pattern]
  deriving Show

data LoopRange = LoopRange Expr Expr Pattern
  deriving Show

data PrimitivePatPattern
  = PPWildCard
  | PPPatVar
  | PPValuePat String  -- Variable name
  | PPInductivePat String [PrimitivePatPattern]
  | PPTuplePat [PrimitivePatPattern]
  deriving Show

data PDPatternBase var
  = PDWildCard
  | PDPatVar var
  | PDInductivePat String [PDPatternBase var]
  | PDTuplePat [PDPatternBase var]
  | PDEmptyPat
  | PDConsPat (PDPatternBase var) (PDPatternBase var)
  | PDSnocPat (PDPatternBase var) (PDPatternBase var)
  | PDConstantPat ConstantExpr
  -- MathValue primitive patterns
  | PDFracPat (PDPatternBase var) (PDPatternBase var)        -- Frac: MathValue -> PolyExpr, PolyExpr
  | PDPlusPat (PDPatternBase var)                           -- Plus: PolyExpr -> [TermExpr]
  | PDTermPat (PDPatternBase var) (PDPatternBase var)       -- Term: TermExpr -> Integer, [(SymbolExpr, Integer)]
  | PDSymbolPat (PDPatternBase var) (PDPatternBase var)     -- Symbol: SymbolExpr -> String, [IndexExpr]
  | PDApply1Pat (PDPatternBase var) (PDPatternBase var)     -- Apply1: SymbolExpr -> MathValue, MathValue
  | PDApply2Pat (PDPatternBase var) (PDPatternBase var) (PDPatternBase var) -- Apply2
  | PDApply3Pat (PDPatternBase var) (PDPatternBase var) (PDPatternBase var) (PDPatternBase var) -- Apply3
  | PDApply4Pat (PDPatternBase var) (PDPatternBase var) (PDPatternBase var) (PDPatternBase var) (PDPatternBase var) -- Apply4
  | PDQuotePat (PDPatternBase var)                          -- Quote: SymbolExpr -> MathValue
  | PDFunctionPat (PDPatternBase var) (PDPatternBase var) -- Function: SymbolExpr -> MathValue, [MathValue]
  | PDSubPat (PDPatternBase var)                            -- Sub: IndexExpr -> MathValue
  | PDSupPat (PDPatternBase var)                            -- Sup: IndexExpr -> MathValue
  | PDUserPat (PDPatternBase var)                           -- User: IndexExpr -> MathValue
  deriving (Functor, Foldable, Show)

type PrimitiveDataPattern = PDPatternBase String

data Op
  = Op { repr     :: String  -- syntastic representation
       , priority :: Int
       , assoc    :: Assoc
       , isWedge  :: Bool    -- True if operator is prefixed with '!'. Only used for expression infix.
       }
  deriving (Eq, Ord, Show)

data Assoc
  = InfixL
  | InfixR
  | InfixN
  | Prefix
  deriving (Eq, Ord)

instance Show Assoc where
  show InfixL = "infixl"
  show InfixR = "infixr"
  show InfixN = "infix"
  show Prefix = "prefix"

reservedExprOp :: [Op]
reservedExprOp =
  [ Op "!"    8 Prefix False -- Wedge and InvertedArg prefix
  , Op "-"    7 Prefix False -- Negate
  , Op "%"    7 InfixL False -- primitive function
  , Op "++"   5 InfixR False
  , Op "::"   5 InfixR False
  , Op "=="   4 InfixL False -- equality (from type class)
  , Op "/="   4 InfixL False -- inequality (from type class)
  , Op "="    4 InfixL False -- primitive function
  , Op "<="   4 InfixL False -- primitive function
  , Op ">="   4 InfixL False -- primitive function
  , Op "<"    4 InfixL False -- primitive function
  , Op ">"    4 InfixL False -- primitive function
  , Op "&&"   3 InfixR False -- logical and (from base)
  , Op "||"   2 InfixR False -- logical or (from base)
  , Op "$"    0 InfixR False -- right-associative lowest-priority (application)
  , Op "+"    6 InfixL False
  , Op "-"    6 InfixL False
  , Op "*"    7 InfixL False
  , Op "/"    7 InfixL False
  , Op "^"    8 InfixL False
  , Op "+'"   6 InfixL False
  , Op "-'"   6 InfixL False
  , Op "*'"   7 InfixL False
  , Op "/'"   7 InfixL False
  , Op "^'"   8 InfixL False
  , Op "∧"    7 InfixL False
  , Op "."    7 InfixL False
  , Op ".'"   7 InfixL False
  ]

reservedPatternOp :: [Op]
reservedPatternOp =
  [ Op "++" 5 InfixR False
  , Op "*:" 5 InfixL False
  , Op "+" 7 InfixR False
  , Op "-" 7 InfixL False  -- subtraction in rule LHS, e.g. `1 - $x`
  , Op "*" 8 InfixR False
  , Op "/" 8 InfixN False
  , Op "^" 9 InfixN False
  , Op "::" 6 InfixR False  -- required for desugaring collection pattern (priority 6 > ++ priority 5)
  , Op "&"  3 InfixR False
  , Op "|"  2 InfixR False
  ]

findOpFrom :: String -> [Op] -> Op
findOpFrom op table = fromJust $ find ((== op) . repr) table

makeApply :: String -> [Expr] -> Expr
makeApply func args = ApplyExpr (VarExpr func) args

stringToVarWithIndices :: String -> VarWithIndices
stringToVarWithIndices name = VarWithIndices name []

extractNameFromVarWithIndices :: VarWithIndices -> String
extractNameFromVarWithIndices (VarWithIndices name _) = name

--
-- Type expressions (for type annotations)
--

-- | Type expression in source code
data TypeExpr
  = TEInt                              -- ^ Integer (= MathValue)
  | TEMathValue                         -- ^ MathValue (= Integer)
  | TEFloat                            -- ^ Float
  | TEBool                             -- ^ Bool
  | TEChar                             -- ^ Char
  | TEString                           -- ^ String
  | TEVar String                       -- ^ Type variable, e.g., a
  | TEList TypeExpr                    -- ^ List type, e.g., [a]
  | TETuple [TypeExpr]                 -- ^ Tuple type, e.g., (a, b)
  | TEFun TypeExpr TypeExpr            -- ^ Function type, e.g., a -> b
  | TEMatcher TypeExpr                 -- ^ Matcher type
  | TEPattern TypeExpr                 -- ^ Pattern type, e.g., Pattern a
  | TEIO TypeExpr                      -- ^ IO type, e.g., IO ()
  | TETensor TypeExpr                  -- ^ Tensor type, e.g., Tensor a
  | TEVector TypeExpr                  -- ^ Vector type, e.g., Vector a (1D tensor)
  | TEMatrix TypeExpr                  -- ^ Matrix type, e.g., Matrix a (2D tensor)
  | TEDiffForm TypeExpr                -- ^ DiffForm type, e.g., DiffForm a (differential form, alias for Tensor)
  | TEApp TypeExpr [TypeExpr]          -- ^ Type application, e.g., List a
  | TEConstrained [ConstraintExpr] TypeExpr
                                      -- ^ Constrained type, e.g., Eq a => a
  -- New CAS types (Phase 2)
  | TEFactor                           -- ^ Factor type (atomic mathematical factor)
  | TETerm TypeExpr SymbolSetExpr      -- ^ Term type, e.g., Term Integer [x] (single monomial over the given atoms)
  | TEFrac TypeExpr                     -- ^ Frac type, e.g., Frac Integer
  | TEPoly TypeExpr SymbolSetExpr      -- ^ Poly type, e.g., Poly Integer [x, y]
  deriving (Show, Eq)

-- | Symbol set expression for polynomial types
data SymbolSetExpr
  = SSEClosed [TypeAtomExpr]            -- ^ Fixed symbol set, e.g., [x, y, sqrt 2]
  | SSEOpen                             -- ^ Open symbol set, [..]
  deriving (Show, Eq)

-- | A single atom inside a closed symbol set: either a plain identifier
-- (`x`, `i`), or a function applied to atom arguments (`sqrt 2`, `sin x`).
-- This is the AST level (parser output); the Type level uses a similar
-- structure (`TypeAtom` in Type.Types).
data TypeAtomExpr
  = TAEName String                      -- ^ Plain symbol/identifier
  | TAEApp String [TypeAtomExpr]        -- ^ Function applied to atom arguments
  | TAEInt Integer                      -- ^ Integer literal in atom position
  deriving (Show, Eq, Ord)

-- | Tensor shape expression
data TensorShapeExpr
  = TSLit [Integer]                    -- ^ Concrete shape, e.g., [2, 2]
  | TSVar String                       -- ^ Shape variable
  | TSMixed [ShapeDim]                 -- ^ Mixed shape, e.g., [n, m, 2]
  deriving (Show, Eq)

-- | Shape dimension (can be concrete or variable)
data ShapeDim
  = SDLit Integer                      -- ^ Concrete dimension, e.g., 2
  | SDVar String                       -- ^ Dimension variable, e.g., n
  deriving (Show, Eq)

-- | Tensor index expression
data TensorIndexExpr
  = TISub String                       -- ^ Subscript, e.g., _i
  | TISup String                       -- ^ Superscript, e.g., ~i
  | TIPlaceholderSub                   -- ^ Subscript placeholder, _#
  | TIPlaceholderSup                   -- ^ Superscript placeholder, ~#
  deriving (Show, Eq)

-- | Typed parameter pattern
data TypedParam
  = TPVar String TypeExpr                    -- ^ Simple variable with type: (x: a)
  | TPInvertedVar String TypeExpr            -- ^ Inverted variable with type: (!x: a)
  | TPTuple [TypedParam]                     -- ^ Tuple pattern: ((x: a), (y: b)) or (x: a, y: b)
  | TPWildcard TypeExpr                      -- ^ Wildcard with type: (_: a)
  | TPUntypedVar String                      -- ^ Untyped variable in tuple: x (inferred)
  | TPUntypedWildcard                        -- ^ Untyped wildcard: _
  deriving (Show, Eq)

-- | Variable with type annotation
data TypedVarWithIndices = TypedVarWithIndices
  { typedVarName        :: String
  , typedVarIndices     :: [VarIndex]
  , typedVarConstraints :: [ConstraintExpr]  -- ^ Type class constraints
  , typedVarParams      :: [TypedParam]      -- ^ Typed parameters (can include tuples)
  , typedVarRetType     :: TypeExpr          -- ^ Return type
  } deriving (Show, Eq)

