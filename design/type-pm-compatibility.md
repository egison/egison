# Egison Core Boundary

> **Status (2026-08-05).** This document defines the integration boundary
> between Egison's production type checker and the executable two-sorted core
> mechanized in `type-pm-mech`. It is an implementation contract, not a claim
> that the complete Egison language has been translated to, or proved sound by,
> the mechanized core.

## 1. Integration policy

Egison keeps its full surface language and runtime semantics. The production
checker uses the mechanized core design wherever a source construct has a
validated core interpretation. Egison-specific extensions that do not yet have
a validated bridge continue through their existing typing and evaluation
paths.

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
errors. A construct rejected by the synchronized core path remains an error
when the production checker already treats it as an error. The warning path is
for an Egison extension or compatibility fallback through which the production
system proceeds. A later, independent error may still reject the enclosing
expression.

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
fragment. The Haskell ordinary-type solver also contains production rules for
CAS representations, tensors, type classes, `Any`, and other Egison features.
A successful call to that whole production solver is therefore not, by
itself, evidence of a TypePM derivation. The mechanized executable checker
remains the Lean implementation; the Haskell integration records known
departures through the boundary described below.

### 2.1 Root capability/target alignment

Every matcher-producing expression is aligned as one capability/target pair.
The root capability and root target are resolved under the same prevailing
solver state, but by their respective solvers. A successful root result must
therefore satisfy both of the following:

1. the capability is justified by constructor or tuple evidence, an actual
   producer/slot assumption, or the unconstraining `none` case; and
2. the ordinary target is the target obtained by applying the same final
   substitution to the shared matcher target.

Target specialization alone is never evidence for a structured capability.
`Any`, a target annotation, a match-site demand, or a recursive result
annotation must not invent a capability head.

### 2.2 Nested rigid core solver

Nested `Matcher` and `MatcherSlot` occurrences are solved by the same
two-sorted rules as root occurrences. In particular, producer capability
variables remain rigid while the surrounding ordinary type is unified. The
nested solver must recurse through products, functions, collections, data
constructors, and embedded matcher types without switching to symmetric
ordinary-type unification for a capability component.

The core solver fails closed. If a nested constraint cannot be handled by its
rigid two-sorted rules, the synchronized path fails rather than weakening the
constraint to `Any`, `none`, or a fresh unconstrained slot. Production may then
use an explicitly identified Egison fallback, but that crossing is a
compatibility-warning event.

### 2.3 One-way `Matcher` to `MatcherSlot` coercion

Coercion is producer-to-consumer and is not symmetric unification:

```text
Matcher kappa_p tau_p  ->  MatcherSlot kappa_c tau_c
```

At the start of the check, the bindable capability domain is fixed to flexible
variables owned only by the consumer. Producer variables, and variables shared
by producer and consumer, remain rigid even if they later occur in a consumer
position during recursive decomposition. Repeated consumer variables must be
solved consistently, and the occurs check remains active.

The capability check runs first. Its substitution is applied to constraints
and both targets before the ordinary target equality is solved. The resulting
substitutions are composed in that order. Tuple coercion is componentwise and
preserves the same fixed producer/consumer boundary in every nested component.

This coercion is available only at an explicit slot-use boundary. Generic
type equality rejects `Matcher`/`MatcherSlot` crossing and raw tuple/matcher
conveniences. The production extension solver may retain those historical
conveniences after reporting a compatibility fallback.

### 2.4 Shared matcher target and fresh hole capabilities

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
`Matcher none [Integer]` is valid, but it cannot fill the hole of a general
`box $` clause whose declared field is `[Integer]`. Wildcard and captured-value
refinements contribute unseen evidence and impose no next-matcher obligation.

An undetermined next-matcher parameter committed to `MatcherSlot` by Step 3a'
is consumer-owned. PP-Con may therefore solve that slot's capability meta to a
fresh instance of the declared field skeleton (observable heads retained,
leaves fresh). An `HCMatcher` value is a producer instead: field validation may
inspect its existing capability but never strengthen it from the field target.
The nested structured primitive-pattern fallback in Section 4.2 remains a
whole-clause uncertified path: it keeps generic result projection, but skips
both this slot-skeleton alignment and closed-field validation. The dedicated
nested-structure warning option controls reporting only, not which inference
path is taken.

### 2.5 Primitive-pattern capture order

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

### 2.6 Allocated/protected producer ledger

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

## 3. Egison extension fallbacks

An extension fallback is a production path whose acceptance has not yet been
connected to the executable core inference theorem by a validated translation.
The fallback may continue to use Egison's existing type checker and evaluator.
Outside-core warnings observe the fallback; they do not replace it, reject
it, or reinterpret it.

Warnings should be emitted once per relevant source occurrence where
practical, name the accepted fallback, and include the closest available
source expression. Generated code should be attributed to its originating
surface construct when that provenance is available. Warning collection must
not mutate the inference environment or participate in solver decisions.

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
warning exists because the production Egison-to-core bridge for these
occurrences has not yet been validated end to end. It must not state that Lean
cannot express or infer the pattern.

### 4.3 Primitive-pattern binder duplication or overlap

A fallback is reported when primitive-pattern captures are not pairwise
distinct, or when a primitive-pattern capture overlaps a primitive-data-pattern
arm binder in a scope for which the core requires disjoint contexts. This
includes repeated `#$name` captures and any production shadowing behavior that
would otherwise choose one of two bindings by environment order.

Once the production checker enforces the same distinctness and disjointness
conditions as the core, these cases should become ordinary type errors and no
longer use a compatibility fallback.

### 4.4 Undeclared primitive-pattern constructor fallback

The core requires every primitive-pattern constructor to be found in the
frozen pattern-constructor signature. Production's generic inference for an
undeclared `PPInductivePat` is an extension fallback and is reported. The
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

A finalized named application instantiates both binder lists in one freshening
step and applies the same paired substitution to every argument and the result.
It then checks the result target, each argument target, and each argument
capability. This `DualScheme` generalization/instantiation and PAT-APP component
is on the synchronized direct path and is not reported merely because it belongs
to a pattern function. This statement does not place the complete definition
body or its embedded expressions on that path; they remain subject to the other
extension boundaries in this inventory and to the bridge obligations in
Section 6.

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

### 4.8 Protected or nested core-solver fallback

A warning is reported whenever the synchronized solver delegates a protected
producer constraint or a nested matcher constraint to a more permissive
production solver. This includes:

- symmetric capability unification where one-way matching was required;
- adding a protected producer variable to a consumer substitution domain;
- using a raw `Any` value, including a raw tuple component, as evidence for a
  `MatcherSlot` head;
- replacing a failed nested matcher constraint with `Any`, `none`, or a fresh
  unconstrained variable;
- deriving ownership from a final zonked type because the allocation ledger is
  unavailable; and
- changing an enclosing ordinary-type or capability metavariable at an
  explicitly identified numeric, CAS, tensor, or related annotation bridge.

The warning should record the fallback reason and the relevant producer and
consumer types. Successful use of the rigid core solver does not warn.

Recursive or transformed producer flow for which production cannot construct
a conservative capability remains a hard type error. It is not reclassified
as a warning merely because the missing proof obligation is outside the core;
warnings are used only where Egison has an actual continuation path.

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
coverage-complete while using an unbridged Egison extension.

## 6. Scope of the soundness claim

A warning-free run is useful evidence that none of the instrumented production
fallbacks was observed. It is not, by itself, a proof that the complete Egison
program is sound, nor does it imply that every production inference result is
covered by `TypePM.infer_success_sound`.

That claim additionally requires a completed translation bridge with, at
minimum:

1. a total translation for every warning-free source declaration, expression,
   pattern, matcher clause, primitive-pattern pattern, and primitive data
   pattern;
2. a frozen-signature and context correspondence theorem;
3. preservation of capability/target schemes, allocation ownership, and
   substitutions across the translation;
4. correspondence between production inference success and executable core
   inference success; and
5. an evaluation or elaboration correspondence sufficient to connect the core
   theorem to the behavior claimed for Egison.

Until those obligations are discharged, the precise claim is limited to the
mechanized core itself and to individual production components whose bridges
have been separately validated. Outside-core warnings expose the remaining
boundary; they do not close it.
