# Fix: Double Dictionary Application in Type Class Expansion

## Date
2026-01-29

## Problem

Type class constrained functions that call other constrained functions in their body were receiving dictionary parameters twice, causing runtime evaluation errors.

### Example Cases

1. **Recursive constrained function call**:
```egison
def myPlus {Num a} (X : a) (Y : a) : a := X + Y
def myPlus2 {Num a} (X : a) (Y : a) : a := myPlus X Y
```
Result: `myPlus2 1 2` returned a lambda function instead of `3`

2. **Constrained function calling tensor operations**:
```egison
def normalProduct {Num a} (X : Tensor a) (Y : Tensor a) : Tensor a := X . Y
```
Result: `normalProduct X Y` returned a lambda function

3. **Variable reference to constrained function**:
```egison
def V.* {Num a} : Tensor a -> Tensor a -> Tensor a := dotProduct
```
Result: `V.*` did not apply dictionary to `dotProduct`, returning unevaluated function

## Root Cause

Type class expansion happens in two phases:

1. **`expandTypeClassMethodsT`** (TypeClassExpand.hs:53-442)
   - Recursively processes all expressions
   - Applies dictionaries to constrained variables via `checkConstrainedVariable` (lines 406-436)

2. **`addDictionaryParametersT`** (TypeClassExpand.hs:936-1016)
   - Adds dictionary parameters to top-level definitions
   - Calls `replaceMethodCallsWithDictAccessT` (lines 1019-1160) to process body
   - Previously also applied dictionaries to constrained variables (lines 1064-1084)

Both phases were applying dictionaries to the same constrained variables, causing **double application**.

### Problematic Flow

For `def myPlus2 {Num a} (X : a) (Y : a) : a := myPlus X Y`:

1. `expandTypeClassMethodsT` processes `myPlus X Y`
   - Applies `dict_Num` to `myPlus`: `(myPlus dict_Num) X Y`

2. `addDictionaryParametersT` wraps in lambda with `dict_Num` parameter
   - `replaceMethodCallsWithDictAccessT` processes body
   - Applies `dict_Num` again: `((myPlus dict_Num) dict_Num) X Y`

Result: `myPlus` receives dictionary twice, creating incorrect closure.

## Solution

### Change 1: Remove redundant dictionary application in replaceMethodCallsWithDictAccessT

**File**: `hs-src/Language/Egison/Type/TypeClassExpand.hs:1064-1068`

**Before** (lines 1064-1084):
```haskell
Nothing -> do
  -- Not a method - check if it's a constrained variable that needs dictionary application
  if not (null exprConstraints) && not (null cs)
    then do
      let matchingConstraints = filter (...) exprConstraints
      if null matchingConstraints
        then return $ TIVarExpr methodName
        else do
          -- Apply matching dictionary parameters
          let dictParams = map constraintToDictParam matchingConstraints
              dictArgExprs = map (\p -> TIExpr ...) dictParams
              varExpr = TIExpr (Forall [] exprConstraints (TVar (TyVar "var"))) (TIVarExpr methodName)
          return $ TIApplyExpr varExpr dictArgExprs
    else return $ TIVarExpr methodName
```

**After**:
```haskell
Nothing -> do
  -- Not a method - just return the variable as-is
  -- Dictionary application for constrained variables is handled by expandTypeClassMethodsT
  return $ TIVarExpr methodName
```

**Rationale**: `expandTypeClassMethodsT` already handles dictionary application for constrained variables. Applying again here causes double application.

### Change 2: Handle variable references in addDictionaryParametersT

**File**: `hs-src/Language/Egison/Type/TypeClassExpand.hs:1008-1035`

**Problem**: When body is a simple variable reference (e.g., `def V.* := dotProduct`), dictionaries need to be applied.

**Before** (lines 1008-1016):
```haskell
-- Not a lambda: wrap in a lambda with dictionary parameters
_ -> do
  let dictParams = map constraintToDictParam cs
      dictVars = map stringToVar dictParams
  expr' <- replaceMethodCallsWithDictAccessT env cs expr
  let wrapperType = tiExprType expr
      newNode = TILambdaExpr Nothing dictVars expr'
      newScheme = Forall [] [] wrapperType
  return $ TIExpr newScheme newNode
```

**After** (lines 1008-1035):
```haskell
-- Not a lambda: wrap in a lambda with dictionary parameters
_ -> do
  let dictParams = map constraintToDictParam cs
      dictVars = map stringToVar dictParams
  -- Special handling for TIVarExpr: if it's a constrained variable, apply dictionaries
  expr' <- case tiExprNode expr of
    TIVarExpr varName -> do
      -- Check if this variable has constraints that match our constraints
      typeEnv <- getTypeEnv
      case lookupEnv (stringToVar varName) typeEnv of
        Just (Forall _ varConstraints _) | not (null varConstraints) -> do
          -- Check which constraints from varConstraints match parent constraints cs
          let (Forall _ exprConstraints exprType) = tiScheme expr
              matchingConstraints = filter (\(Constraint eName eType) ->
                    any (\(Constraint pName pType) ->
                      eName == pName && eType == pType) cs) exprConstraints
          if null matchingConstraints
            then replaceMethodCallsWithDictAccessT env cs expr
            else do
              -- Apply matching dictionary parameters
              let dictArgExprs = map (\p -> TIExpr (Forall [] [] (TVar (TyVar "dict"))) (TIVarExpr p))
                                     (map constraintToDictParam matchingConstraints)
                  varExpr = TIExpr (tiScheme expr) (TIVarExpr varName)
              return $ TIExpr (tiScheme expr) (TIApplyExpr varExpr dictArgExprs)
        _ -> replaceMethodCallsWithDictAccessT env cs expr
    _ -> replaceMethodCallsWithDictAccessT env cs expr
  let wrapperType = tiExprType expr
      newNode = TILambdaExpr Nothing dictVars expr'
      newScheme = Forall [] [] wrapperType
  return $ TIExpr newScheme newNode
```

**Rationale**: When body is `TIVarExpr` referencing a constrained function, we need to apply dictionary parameters at this stage. This handles cases like `def V.* := dotProduct` where:
- `V.*` has constraint `{Num a}`
- `dotProduct` also has constraint `{Num a}`
- Need to connect them: `\dict_Num -> dotProduct dict_Num`

**Key Difference from Change 1**: This applies dictionaries only when body is a **top-level variable reference** (not nested in application), which is never processed by `expandTypeClassMethodsT`.

## Type Class Expansion Flow

After the fix, dictionary application happens in appropriate phases:

1. **`addDictionaryParametersT`**: Adds dictionary parameters to function definitions
   - For lambda body: wraps with dict params
   - For variable reference body: applies dicts if needed (NEW)
   - For other expressions: wraps with dict params and processes via `replaceMethodCallsWithDictAccessT`

2. **`expandTypeClassMethodsT`**: Processes all expressions recursively
   - Applies dictionaries to constrained variable **calls**
   - Does NOT re-apply to variables already processed in step 1

3. **`replaceMethodCallsWithDictAccessT`**: Replaces type class method calls
   - Handles only type class methods (e.g., `+`, `.`)
   - Does NOT apply dicts to constrained user-defined functions (REMOVED)

## Test Cases

### Test 1: Recursive constrained function call
```egison
def myPlus {Num a} (X : a) (Y : a) : a := X + Y
def myPlus2 {Num a} (X : a) (Y : a) : a := myPlus X Y

myPlus2 1 2  -- Expected: 3, Result: 3 ✓
```

### Test 2: Constrained tensor operation
```egison
def normalProduct {Num a} (X : Tensor a) (Y : Tensor a) : Tensor a := X . Y

normalProduct [| 1, 2, 3 |] [| 4, 5, 6 |]
-- Expected: [| 4, 10, 18 |], Result: [| 4, 10, 18 |] ✓
```

### Test 3: Variable reference to constrained function
```egison
def dotProduct {Num a} (v1: Tensor a) (v2: Tensor a) : Tensor a :=
  withSymbols [i] v1~i . v2_i

def V.* {Num a} : Tensor a -> Tensor a -> Tensor a := dotProduct

V.* [| 1, 2 |] [| 3, 4 |]
-- Expected: element-wise product, Result: works correctly ✓
```

### Test 4: Real-world usage (riemann-curvature-tensor-of-S2.egi)
```egison
def g_i_j : Matrix MathExpr := generateTensor (\[x, y] -> V.* e_x_# e_y_#) [2, 2]
```
Result: Metric tensor computed correctly ✓

## Related Files

- `hs-src/Language/Egison/Type/TypeClassExpand.hs`: Main fix
  - Lines 1064-1068: Removed redundant dictionary application
  - Lines 1008-1035: Added special handling for variable references

- `hs-src/Language/Egison/Type/TypedDesugar.hs`: Type class expansion orchestration
  - Lines 45-53: Call order of transformation phases

## Notes

- The fix distinguishes between:
  1. **Variable calls** in expressions (handled by `expandTypeClassMethodsT`)
  2. **Variable references** as body (handled by `addDictionaryParametersT`)

- This ensures each constrained variable receives dictionaries exactly once, regardless of context.

- The constraint matching (`eName == pName && eType == pType`) ensures we only apply dictionaries for constraints that actually match between parent and child scopes.
