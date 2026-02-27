# Constraint-Aware Unification for Tensor Types

## Problem Discovery

When type class constrained functions (e.g., `+` with `Num` constraint) are applied to Tensor arguments, the current type inference incorrectly unifies type variables with Tensor types, resulting in unsatisfiable constraints.

### Example

```egison
-- Function: + :: {Num t0} t0 -> t0 -> t0
-- Arguments: Tensor MathExpr, Tensor MathExpr
-- Current result: {Num (Tensor MathExpr)} -- WRONG: Tensor MathExpr is not a Num instance
-- Expected result: {Num MathExpr}
```

## Current Behavior

### Type Inference Flow

1. **Function type from environment**: `Forall [t123] [Num t123] (t123 -> t123 -> t123)`
2. **Expected type from arguments**: `Tensor MathExpr -> Tensor MathExpr -> result`
3. **Unification** (line 2182 in Infer.hs):
   ```haskell
   unify (t123 -> t123 -> t123) (Tensor MathExpr -> Tensor MathExpr -> result)
   ```
4. **Result**: `{t123 -> Tensor MathExpr}` due to `Unify.hs` lines 98-99:
   ```haskell
   unify' (TTensor t1) t2 = unify t1 t2
   unify' t1 (TTensor t2) = unify t1 t2
   ```
5. **Constraint resolution**: `Num t123` becomes `Num (Tensor MathExpr)` - unsatisfiable!

## Root Cause

The `unify` function only performs structural unification without considering type class constraints. When unifying `Tensor a` with a type variable `t`, it always chooses `t = Tensor a`, even if the constraint `C t` cannot be satisfied by `Tensor a`.

## Solution Space

When unifying `Tensor a` with a constrained type variable `t` where constraint `C t` exists:

### Candidate 1: `t = Tensor a`
- Requires: `C (Tensor a)` instance
- Rare case: When Tensor type itself has the instance

### Candidate 2: `t = a`
- Requires: `C a` instance
- Common case: When element type has the instance
- **This is the correct choice for type class methods**

## Implementation Approach 1: Constraint-Aware Unification

Create a new unification function that considers type class constraints and selects unifiers that satisfy them.

### Design

```haskell
unifyWithConstraints :: ClassEnv -> [Constraint] -> Type -> Type -> Either UnifyError Subst
```

**Algorithm:**
1. Perform normal unification
2. When unifying `Tensor a` with constrained type variable `t`:
   - Check if `t` has any type class constraints
   - Try candidate 1: `t = Tensor a`, check if constraints are satisfiable
   - Try candidate 2: `t = a`, check if constraints are satisfiable
   - Select the satisfiable candidate (prefer `t = a` if both work)
   - Fail if neither works

### Benefits
- Catches constraint violations early in type inference
- Produces correct types automatically
- No need for post-processing or correction

### Location
- New function in `Language.Egison.Type.Unify`
- Used in `inferIApplicationWithContext` (line 2182)

## Implementation Approach 2: Post-Unification Correction (Alternative)

After normal unification, check constraints and modify substitutions if needed.

**Not recommended** because:
- More complex error handling
- Harder to track changes
- May need multiple iterations

## Implementation Approach 3: Deferred Resolution (Alternative)

Keep Tensor types during inference and resolve in TensorMapInsertion phase.

**Not recommended** because:
- Loses type safety early
- Makes error messages less clear
- Complicates later phases

## Recommended Solution

**Implement Approach 1**: Constraint-aware unification that considers type class constraints when choosing between `t = Tensor a` and `t = a`.

### Key Changes

1. Add `unifyWithConstraints` to `Language.Egison.Type.Unify`
2. Modify `inferIApplicationWithContext` to use the new function
3. Pass constraints and ClassEnv to the unification function

## Expected Outcome

After implementation:
```egison
-- Function: + :: {Num t0} t0 -> t0 -> t0
-- Arguments: Tensor MathExpr, Tensor MathExpr
-- Unification: t0 with Tensor MathExpr
-- Constraint check: Num (Tensor MathExpr)? No. Num MathExpr? Yes!
-- Result: t0 = MathExpr
-- Final constraint: {Num MathExpr} ✓
```

## Related Issues

- Functions remain scalar even when applied to Tensor arguments (as intended)
- TensorMap insertion happens in phase 8, not during type inference
- Original function types are preserved (e.g., `t0 -> t0 -> t0` not `Tensor t0 -> Tensor t0 -> Tensor t0`)
