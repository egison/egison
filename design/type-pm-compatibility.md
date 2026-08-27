# Egison Core Boundary

> **Status (2026-08-27).** This document defines the integration boundary
> between Egison's production type checker and the executable two-sorted core
> mechanized in `type-pm-mech3`. It is an implementation contract, not a claim
> that the complete Egison language has been translated to, or proved sound by,
> the mechanized core.

## 1. Integration policy

Egison keeps its full surface language and runtime semantics, but its type
system is a conservative extension of the mechanized core. Here,
“conservative extension” means that a program using core syntax, core types,
and core signatures and satisfying the core's static side conditions is typed
by the synchronized TypePM rules, with the same type and substitution; adding
Egison constructs does not change an existing core typing judgment.

Every Egison-specific typing rule therefore requires a positive extension
witness in the current input, such as an ordinary `TAny`, a CAS or tensor type,
a legacy CAS view, a production-only pattern form, or an explicitly diagnosed
relaxation of a core surface side condition such as coverage. Failure of core
type equality or matcher/slot checking is not such a witness and never selects
a second matcher solver. This makes the extra language forms extensions of the
core rather than alternate interpretations of core programs.

Here, “compatibility” refers only to explicitly non-core syntax or types that
are outside the maintained mechanized-core contract. It does not mean source
compatibility with `type-pm-mech` or `type-pm-mech2`, and it does not retain
their producer-flow or syntax-head inference algorithms as shims.

Crossing such an extension boundary must not silently be presented as a core
guarantee. General production extensions are reported by the optional flag

```text
--outside-egison-core-warnings
```

Two independently useful primitive-pattern diagnostics have dedicated flags:

```text
--pattern-hole-before-primitive-value-pattern-warnings
--nested-structured-primitive-pattern-pattern-warnings
```

Enabling any warning flag is diagnostic only: it must not change inferred
types, substitutions, elaborated terms, evaluation order, runtime values, or
failure behavior. Disabling them must preserve the existing production
behavior exactly. The former umbrella option is not retained as an alias: it
did not identify which boundary was being audited.

The outside-core warning path is deliberately separate from strict type
errors. A core construct rejected by the synchronized core path remains an
error; it cannot be rescued by enabling a warning-producing rule. The warning
path records an explicitly witnessed Egison extension through which the
production system proceeds. A later, independent error may still reject the
enclosing expression.

## 2. Synchronized two-sorted core path

The synchronized path treats matcher capability and matcher target as
different sorts throughout inference:

```text
Matcher     kappa tau
MatcherSlot kappa tau
```

An ordinary type substitution may refine `tau`, but it does not manufacture or
strengthen `kappa`. A capability substitution acts only on capability
positions. The production implementation must retain this separation in
types, schemes, free-variable computation, instantiation, generalization,
constraint solving, and zonking.

Here, “synchronized” refers to this capability/target and matcher-slot
fragment. On the TypePM grammar, the production entry point calls this relation
directly and does not retry with a wider matcher relation. The Haskell checker
also has separately selected rules for CAS representations, tensors, type
classes, ordinary gradual `TAny`, and other Egison features. A successful use
of one of those extension rules is not, by itself, evidence of a TypePM
derivation. The mechanized executable checker remains the Lean implementation;
the Haskell integration records known extra forms through the boundary below.

### 2.1 A/R-constrained ordinary variables and recursive values

Ordinary unification variables carry one of two usage classes without creating
a second grammar of types. An argument-class variable (A variable) may be
replaced by any `TypeOK` type, including `MatcherSlot`. A result-class variable
(R variable) may be replaced only by a `ResultOK` type. `MatcherSlot` itself is
not `ResultOK`; a function is `ResultOK` when its domain is `TypeOK` and its
codomain is `ResultOK`. Consequently, slots may occur only in function-argument
positions, including argument positions of a returned function, and never as a
returned value.

When A and R occurrences are unified, the A occurrence is strengthened to the
R class. Substitution, generalization, instantiation, public annotation
checking, and principal-type comparison retain that class. Thus an identity
function can have one shared R variable in both its domain and codomain instead
of losing the fact that the shared type must be result-admissible.

A recursive binder is assigned one fresh monomorphic R placeholder before its
body is inferred. The completed body type is made result-admissible and unified
with that placeholder. The only syntax restriction is that each actually
cyclic definition has a lambda or matcher literal at its root. The cycle
analysis enforces only this root restriction; it does not classify producer
names or contribute matcher capability evidence.

At a next-matcher slot, an unresolved A variable is committed to
`MatcherSlot`, while an unresolved R variable must be committed to `Matcher`.
The latter value is checked against the slot, but contributes neither
independent clause-shape evidence nor a `CapTargetOK` assumption. This decision
depends only on the current A/R class, so direct recursion, a local alias, and
an ordinary identity-function application follow the same rule.

### 2.2 Root capability/target alignment

Every matcher-producing expression is aligned as one capability/target pair.
The root capability and root target are resolved under the same prevailing
solver state, but by their respective solvers. A successful root result must
therefore satisfy both of the following:

1. the capability is justified by constructor or tuple evidence, an actual
   producer/slot assumption, or the ground `Any` case; and
2. the ordinary target is the target obtained by applying the same final
   substitution to the shared matcher target.

Target specialization alone is never evidence for a structured capability.
Ordinary gradual `TAny`, a target annotation, or a match-site demand must not
invent a capability head.

### 2.3 Capability-origin ledger and nested core solver

Nested `Matcher` and `MatcherSlot` occurrences are solved by the same
two-sorted rules as root occurrences. `InferState` records every capability
variable as `Rigid`, `RenameOnly`, or `StructuralFlexible`. Unlisted ambient
variables are rigid. A generic scheme instance is rename-only: it may be
alpha-renamed to another non-structural variable, but it must not acquire a
constructor such as `Collection`. Fresh consumer and constructor-local
variables are structural until their scope is complete.

Constructor and primitive schemes use a dedicated structural instantiation
path. After the application constraints have been solved, only structural
capability leaves that remain visible in the exported type are frozen to
rename-only. A constructor capability consumed entirely inside an application
therefore stays locally flexible, while a partially applied or bare
constructor cannot export structural flexibility.

Egison also exposes consumer positions as `MatcherSlot` arguments of ordinary
library combinators such as `list` and `maybe`. On a syntactically direct
application, only scheme binders occurring in such slots receive the same
local structural treatment; all other binders remain rename-only, and the
surviving result leaves are frozen. Taking the function as a value does not
use this path. Likewise, a named pattern-function application allocates its
dual binders as local structural pattern demands for that match cut, while
ordinary standalone dual-scheme instantiation remains rename-only. These are
production representations of consumer-demand allocation, not permission to
strengthen an exported producer.

The nested paired-type solver must recurse through products, functions,
collections, data constructors, tensors, and embedded matcher types while
consulting the same origin ledger for every capability component. Tensor
application retains Egison's traversal rule; the capability-origin check does
not change that evaluation-order extension.

The core solver fails closed. If a nested core constraint cannot be handled by
its origin-aware two-sorted rules, inference fails rather than weakening the
constraint to ordinary `TAny`, capability `Any`, or a fresh unconstrained slot.
There is no failure-triggered retry. A separate extension rule can apply only
when its own non-core syntax or type is already present in the constraint.

An origin violation is never such a warning event. Any substitution that
strengthens a rigid or rename-only variable is rejected as a type error before
it can be committed to the global substitution.

### 2.4 One-way `Matcher` to `MatcherSlot` coercion

Coercion is producer-to-consumer and is not symmetric unification:

```text
Matcher kappa_p tau_p  ->  MatcherSlot kappa_c tau_c
```

At the start of the check, the bindable capability domain is fixed to flexible
variables owned only by the consumer. Producer variables, and variables shared
by producer and consumer, remain rigid even if they later occur in a consumer
position during recursive decomposition. Repeated consumer variables must be
solved consistently, and the occurs check remains active.

Capability `Any` is a ground constructor, not an unsolved metavariable. Its
wildcard behavior is limited to a literal `Any` node in the original consumer
shape of this one-way judgment. Producer `Any` is rigid, as is every `Any` in
symmetric capability unification and exact evidence merging. If a consumer
variable is first bound to producer `Any`, a later occurrence of that variable
must compare strictly with the saved `Any`; applying the substitution must not
turn it into a new wildcard. Thus matching `Prod[Any, K]` against the consumer
`Prod[kappa, kappa]` fails unless `K` is also `Any`. The original-node
provenance and shared binding environment are retained across product-slot
aggregation, nested `Matcher`/`MatcherSlot` types, and multi-parameter
one-way matching.

The capability check runs first. Its substitution is applied to constraints
and both targets before the ordinary target equality is solved. The resulting
substitutions are composed in that order. Tuple coercion is componentwise and
preserves the same fixed producer/consumer boundary in every nested component.

This coercion is available only at an explicit slot-use boundary. Generic type
equality rejects `Matcher`/`MatcherSlot` crossing and raw tuple/matcher
conveniences, including at nested positions. Egison does not retry a rejected
core equality merely because the two types have the same matcher head. A raw
ordinary `TAny` at the explicit slot boundary has its own gradual extension
rule; that positive type witness is not matcher capability evidence and does
not change core equality.

### 2.5 Shared matcher target and fresh hole capabilities

A matcher literal has one shared ordinary target across all clauses. Each
primitive-pattern hole allocates a fresh capability variable and pairs it with
the target determined by that hole's constructor-field position. Clause
inference preserves source order for holes and captured bindings.

All clauses are checked against the shared target and one cumulative solver
state. Constructor and tuple nodes project their child evidence into the
matcher root; a bare root hole is the catch-all exception and does not provide
root shape evidence. Fresh hole capabilities are resolved only by actual
next-matcher components and constructor-signature projection, never by the
shared target alone.

Result projection and field admissibility are separate operations. Generic
constructor projection transports only evidence that reaches a variable in the
constructor result. The certified actual-clause path additionally checks every
non-unseen hole producer against the declared field's capability-visible
skeleton, including nested observable heads. Thus a value of type
`Matcher Any [Integer]` is valid, but it cannot fill the hole of a general
`box $` clause whose declared field is `[Integer]`. Wildcard and captured-value
refinements contribute unseen evidence and impose no next-matcher obligation.

An undetermined A variable committed to `MatcherSlot` at a next-matcher
boundary is consumer-owned. PP-Con may therefore solve that slot's capability
meta to a fresh instance of the declared field skeleton (observable heads
retained, leaves fresh). An existing matcher value is a producer instead:
field validation may inspect its existing capability but never strengthen it
from the field target. An undetermined R variable is committed to `Matcher` so
it can fill the slot, but that commitment supplies unseen shape evidence and no
capability/target assumption. This is what prevents a recursive use from
proving the structure of the matcher currently being defined.
The nested structured primitive-pattern fallback in Section 4.2 remains a
whole-clause uncertified path: it keeps generic result projection, but skips
both this slot-skeleton alignment and closed-field validation. The dedicated
nested-structure warning option controls reporting only, not which inference
path is taken.

### 2.6 First-result match fallback

The first-result forms `match` and `matchDFS` may carry an `else` fallback.
Ordinary arms are inferred in source order. The fallback is inferred last in
the surrounding context, outside every arm binding, and its result is unified
with the one result type shared by the ordinary arms. At runtime it is
evaluated in the original environment only after every ordinary arm has
produced an empty result. It is not a wildcard arm and performs no matcher
operation of its own. The enumerating forms `matchAll` and `matchAllDFS` do not
have this fallback.

### 2.7 Primitive-pattern capture order

Egison core visits a `PrimitivePatPattern` depth first and from left to right.
A primitive value pattern `#$name` is core-admissible only if no pattern hole
`$` has been visited earlier in that order. Wildcards do not change the state;
constructor and tuple children thread the state through their children in
source order. For example:

```text
cons #$head $       core-admissible
cons $ #$tail       outside Egison core
($, #$value)        outside Egison core
join $ (cons #$v $) outside Egison core
```

Consequently, convenient production clauses such as `$ ++ #$px :: $` and
`($, #$n) :: $` are outside the formal core whenever their parsed tree places
the first `$` before the capture. Production may keep them; a core-facing
matcher instead declares a pattern constructor with the required fields in the
opposite order.

The restriction removes the operational case in which primitive value-pattern
evaluation can observe bindings contributed by a hole to its left. Equivalent
matcher behavior can be expressed by declaring a pattern constructor whose
field order matches the required evaluation order.

Production Egison intentionally continues to accept the broader syntax for
convenience. It reports the boundary at matcher-definition time when
`--pattern-hole-before-primitive-value-pattern-warnings` is enabled. This
syntax-only warning is distinct from `checkVpScope`: an actual user value
pattern that refers to a binding made to its left in the same matcher atom is
still a hard type error.

### 2.8 Allocated/protected producer ledger

The production solver maintains explicit sets for capability variables
allocated during inference and for variables protected from later
consumer-side solving. Scheme capability images are protected immediately;
inference-owned variables that survive in a finalized matcher capability are
protected at finalization.

The ledger has the following invariants:

- a producer variable is allocated before any target equality can expose it in
  a consumer position;
- a protected producer variable is never added to a one-way substitution
  domain later in decomposition;
- fresh hole and scheme-instance variables have distinct allocation
  identities, even when subsequent target unification makes their displayed
  types equal;
- zonking does not change allocation identity; and
- nested solver calls inherit the protected set of their enclosing producer.

This information must be carried explicitly. The final capability is
intersected with the allocation set only to select which surviving
inference-owned variables become protected; source capability variables are
therefore not accidentally claimed by the matcher producer.

## 3. Egison extension rules

An extension rule is a production rule for a form outside the TypePM admissible
fragment and therefore outside the executable core inference theorem. It is selected by an explicit
non-core form or a diagnosed relaxation of a surface side condition, not by a
failed type equality or matcher/slot judgment. Outside-core warnings observe
the rule; they do not replace it, reject it, or reinterpret it.

Warnings should be emitted once per relevant source occurrence where
practical, name the accepted extension, and include the closest available
source expression. Generated code should be attributed to its originating
surface construct when that provenance is available. Warning collection must
not mutate the inference environment or participate in solver decisions.

The intentional extension paths are therefore limited and independently
controlled:

| Production extension | Positive witness and purpose | Reporting or control |
|---|---|---|
| Raw ordinary `TAny` at an explicit slot | The ordinary `TAny` node explicitly requests Egison's gradual unknown rule | `--outside-egison-core-warnings` |
| Numeric, CAS, and tensor representation equality at a rigid annotation boundary | An extension representation type, or a numeric type together with its production constraint, explicitly selects the relation | `--outside-egison-core-warnings` |
| Legacy CAS pattern views | A named legacy view selects this path; the target-indexed virtual signature and runtime preservation certificate do not yet exist | `--outside-egison-core-warnings` |
| Header-only or expression-headed pattern-function application, and residual non-core pattern forms | The corresponding production-only syntax or unresolved header is the witness; no finalized paired capability/target contract or validated surface-to-core translation is available | `--outside-egison-core-warnings` |
| Broader primitive-pattern ordering or nesting | The broader primitive-pattern AST is the witness; production syntax is intentionally wider than the maintained core contract | Two dedicated primitive-pattern warning flags |
| Partial matcher coverage | Production treats coverage as an optional diagnostic rather than an acceptance premise | `--matcher-consistency-warnings` |
| Evaluation after a type error | The default evaluator retains the historical untyped fallback | Disabled by `--type-check-strict` |
| Generated CAS quotient representation cast | Quotient projection and representation code currently crosses a nominal boundary with a reserved unsafe identity cast | Reserved for generated code by convention; no core certificate |

## 4. Warning inventory

Except for the two primitive-pattern categories in Sections 4.1 and 4.2, the
following cases are reported when `--outside-egison-core-warnings` is enabled.

### 4.1 Pattern hole before a primitive value pattern

A matcher clause whose depth-first, left-to-right primitive-pattern traversal
encounters `$` before a later `#$name` is reported when
`--pattern-hole-before-primitive-value-pattern-warnings` is enabled. The
diagnostic is attached to the matcher definition and renders the complete
primitive-pattern tree. Production accepts and evaluates the clause unchanged;
the warning states that the clause is outside Egison core.

This category is independent of nesting. Thus `($, #$value)` produces only
this warning, while `join $ (cons #$value $)` produces this warning and the
nested-structure warning below when both flags are enabled. Reversing the
relevant order, as in `(#$value, $)`, does not produce this warning.

### 4.2 Nested structured primitive-pattern patterns

A nested constructor or tuple inside another constructor or tuple is reported,
for example:

```text
<join $ <cons #$val $>>
```

or its current surface equivalent:

```text
join $ (cons #$val $)
```

This category is reported only when
`--nested-structured-primitive-pattern-pattern-warnings` is enabled. The
diagnostic is based on the `PrimitivePatPattern` AST, not textual matching.
This avoids warnings for comments and ordinary user patterns and correctly
handles operator precedence.

Lean's core syntax, typing rules, operational semantics, and executable
inference can represent nested structured primitive-pattern patterns. The
warning classifies these production occurrences outside the maintained
Egison-core integration contract; constructing and proving a complete
Egison-to-core translation is not required. The diagnostic must not state that
Lean cannot express or infer the pattern.

### 4.3 Primitive-pattern binder duplication or overlap

A fallback is reported when primitive-pattern captures are not pairwise
distinct, or when a primitive-pattern capture overlaps a primitive-data-pattern
arm binder in a scope for which the core requires disjoint contexts. This
includes repeated `#$name` captures and any production shadowing behavior that
would otherwise choose one of two bindings by environment order.

Once the production checker enforces the same distinctness and disjointness
conditions as the core, these cases should become ordinary type errors and no
longer use this explicit extension rule.

### 4.4 Undeclared primitive-pattern constructor fallback

The core requires every primitive-pattern constructor to be found in the
frozen pattern-constructor signature. Production's generic inference for an
undeclared `PPInductivePat` is an extension rule selected by that undeclared
constructor and is reported. The
warning names the constructor and states that its result and field types were
not obtained from the frozen signature.

Removing generic undeclared-constructor inference and rejecting the construct
as an ordinary type error retires this warning category.

### 4.5 Legacy CAS views

The target-indexed virtual signature needed to justify legacy CAS views is not
part of the synchronized core bridge. Any special path that accepts views such
as `MathValue`, `PolyExpr`, `TermExpr`, `SymbolExpr`, or `IndexExpr` without the
ordinary frozen constructor/capability correspondence is reported.

The diagnostic must identify the legacy view boundary. It must not treat the
view as constructor evidence for unrelated ordinary capabilities.

### 4.6 Pattern-function fallback boundary

A successfully checked pattern-function definition is stored as one canonical
`DualScheme`. The scheme contains the capability and target of every argument
and of the result, with capability binders and ordinary type binders kept in
separate sorts. The expression-facing function scheme is only a target
projection computed from this canonical scheme and shares its quantified
binders. Both binder lists are set-like: definition-side generalization
constructs duplicate-free lists, and instantiation rejects a malformed scheme
with duplicate binders, matching the mechanized input invariant.

Definition-side generalization counts each flexible capability variable across
all argument/result duals, including matcher capabilities nested in their
ordinary targets. A non-ambient variable occurring exactly once carries no
correlation and is canonicalized to ground `Any`. A variable occurring two or
more times remains quantified so its argument/argument or argument/result
sharing is preserved. Ambient variables remain free. Local pattern variables,
wildcards, and value/predicate leaves still receive fresh capabilities during
inference; only the completed `DualScheme` crosses this canonicalization
boundary.

A finalized named application instantiates both binder lists in one freshening
step and applies the same paired substitution to every argument and the result.
It then checks the result target, each argument target, and each argument
capability. This `DualScheme` generalization/instantiation and PAT-APP component
is on the synchronized direct path and is not reported merely because it belongs
to a pattern function. This statement does not place the complete definition
body or its embedded expressions on that path; they remain subject to the other
extension boundaries in this inventory and to the scope boundary in Section 7.

If the definition body itself contains a residual non-core pattern form from
Section 4.7, that body boundary is reported when the definition is checked.
The inferred `DualScheme` is still retained, but later finalized named
applications do not repeat—or silently substitute for—the definition-site
diagnostic.

Only the following pattern-function applications cross this compatibility
boundary:

- a header-only forward or mutually recursive reference whose body has
  not yet produced a finalized `DualScheme`; and
- an explicit expression-headed `IPApplyPat`. This path always infers its head
  through the ordinary lexical environment, even when that head is a variable
  whose spelling also names a finalized top-level pattern function; only the
  resolved named surface form selects canonical PAT-APP dispatch.

Those paths retain Egison's target-only application checking and are reported.
A header permits name resolution and target checking, but it is not capability
evidence. Production must not reconstruct a capability scheme from that header
or from an already-zonked target type. A direct or nested self-call is not a
fallback: PATFUN-DEF rejects it under the mechanized core's nonrecursion side
condition.

When a later load unit redeclares a pattern-function name, its header masks the
older finalized scheme before any item in that unit is checked. Only successful
checking installs the replacement scheme; permissive fallback leaves the new
runtime body header-only. Duplicate declarations of one pattern-function name
inside the same expanded load unit are rejected, because choosing different
declarations in the static and runtime environments would invalidate the
stored contract.

### 4.7 Non-core pattern forms

After verified surface elaboration, any surviving pattern form without a core
term and typing-rule translation is reported. Examples include production-only
predicate, indexed, let, negation, universal, loop, continuation, sequential,
later-variable, and symbolic application forms.

A surface form that is completely lowered by a validated, type-preserving
elaboration should not warn merely because it appeared in the source. The
warning applies when the production checker or evaluator handles a residual
non-core form directly.

The pattern result context is also part of this boundary. TypePM contexts are
ordered and duplicate-free. Egison cases that retain duplicate binders or
accept two or-pattern branches with the same names in different orders are
reported as extension-context fallbacks.

Constructor-shaped user patterns are core-facing only when their constructor
is present in the frozen pattern signature. Falling back to a value
constructor or to generic constructor inference is reported.

### 4.8 Explicit type-extension boundaries

There is no general extended matcher solver. Matcher equality, nested matcher
equality, top-level matcher annotations, and explicit slot checks use the
synchronized TypePM relations directly. Rejecting one of those judgments does
not select another relation.

Two ordinary-type extensions are selected independently by positive type
evidence:

- a raw ordinary `TAny` may fill an explicit `MatcherSlot` through Egison's
  gradual rule; and
- an extension representation type (numeric, CAS, tensor, or a named
  production representation), or a numeric type under a production numeric
  constraint, may use its representation rule at a rigid annotation boundary.

Both cases are reported with the relevant types. Successful use of the core
solver does not warn. The first rule applies only at the explicit slot-use
boundary and is not matcher capability evidence.

Capability-origin violations, generic `Matcher`/`MatcherSlot` crossing,
same-head nested matcher mismatches, and implicit tuple-to-slot decomposition
remain hard errors. In particular, the ground capability `Any` of a value
`Matcher Any tau` cannot witness a structured capability, and no extension
rule may replace a capability failure with capability `Any` or an
unconstrained variable.

Recursive matcher uses do not select an extension merely because they pass
through an alias or ordinary function application. They use the A/R rule in
Section 2.1 and the type-derived next-matcher check.

## 5. Coverage uses a separate option

Coverage is a well-formedness premise of the mechanized core, while the
production language deliberately retains it as a separate opt-in diagnostic.
Coverage diagnostics remain controlled by:

```text
--matcher-consistency-warnings
```

They must not be enabled, disabled, reclassified, or used as capability
evidence by the outside-core warning flags. Auditing a production program
against the complete core acceptance conditions therefore requires
`--matcher-consistency-warnings` together with the relevant outside-core flags.
A matcher can exercise only synchronized inference operations while
remaining intentionally partial under the production policy, and it can be
coverage-complete while using an out-of-core Egison extension.

## 6. Runtime compatibility outside inference warnings

Two compatibility mechanisms are intentionally separate from the
outside-core inference warnings.

By default, a top-level type error is printed and the expression may continue
through untyped desugaring and evaluation. `--type-check-strict` stops before
that expression enters the runtime environment. Strict mode therefore removes
this permissive evaluation fallback, but it does not disable the independently
defined Egison extension relations above or turn the run into a certified
whole-program mode.

Generated CAS quotient declarations use the reserved primitive
`casQuotientCast : forall a b. a -> b` for representation projection and
reconstruction. Its runtime implementation is an identity operation. This is
an explicit unsafe bridge for generated quotient code, not a TypePM
derivation, a D5-CAS certificate, or a general implicit coercion rule. The
current type environment does not carry a provenance marker proving that a
call was generated, so this primitive remains a whole-program certification
escape hatch.

## 7. Scope of the soundness claim

For a program in the core admissible fragment, the implementation contract is
exact: production dispatch reaches the synchronized typing relations directly,
emits no extension warning, and preserves their inferred type and
substitution. At the constraint level, regression tests compare the public
production unification entry point with the synchronized core entry point on
accepted and rejected core constraints. This prevents an Egison extension from
silently rescuing a core type-equality failure.

A warning-free run is useful evidence that none of the instrumented production
extensions was observed. It is not, by itself, a proof that the complete Egison
program is sound, nor does it imply that every production inference result is
covered by `TypePM.infer_success_sound`.

Proving a translation or correspondence theorem between the complete Haskell
implementation and the Lean implementation is an explicit non-goal. The
production checker also contains parsing, elaboration, type classes, CAS,
tensors, and runtime integration that are intentionally outside the
mechanized core; formalizing that complete stack would be a separate project.

The maintained boundary is therefore an engineering contract:

1. `type-pm-mech3` is the normative specification and proof artifact for the
   core type system;
2. the Haskell core path implements the corresponding rules without a
   failure-triggered alternate matcher solver;
3. regression tests preserve acceptance, rejection, and substitutions at the
   synchronized Haskell entry points; and
4. production-only rules remain explicitly separated and observable through
   the extension inventory and diagnostics.

These tests and documents support synchronization, not a formal Haskell–Lean
correspondence claim. Outside-core warnings expose the implementation boundary;
they are not proof certificates.
