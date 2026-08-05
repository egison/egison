# Type-PM Compatibility Boundary

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
guarantee. The optional flag

```text
--type-pm-compatibility-warnings
```

reports those crossings. Enabling the flag is diagnostic only: it must not
change inferred types, substitutions, elaborated terms, evaluation order,
runtime values, or failure behavior. Disabling it must preserve the existing
production behavior exactly.

The compatibility warning path is deliberately separate from strict type
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

### 2.5 Allocated/protected producer ledger

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
Compatibility warnings observe the fallback; they do not replace it, reject
it, or reinterpret it.

Warnings should be emitted once per relevant source occurrence where
practical, name the accepted fallback, and include the closest available
source expression. Generated code should be attributed to its originating
surface construct when that provenance is available. Warning collection must
not mutate the inference environment or participate in solver decisions.

## 4. Compatibility warning inventory

The following cases are reported when
`--type-pm-compatibility-warnings` is enabled.

### 4.1 Nested structured primitive-pattern patterns

A nested constructor or tuple inside another constructor or tuple is reported,
for example:

```text
<join $ <cons #$val $>>
```

or its current surface equivalent:

```text
join $ (cons #$val $)
```

The diagnostic is based on the `PrimitivePatPattern` AST, not textual matching.
This avoids warnings for comments and ordinary user patterns and correctly
handles operator precedence.

Lean's core syntax, typing rules, operational semantics, and executable
inference can represent nested structured primitive-pattern patterns. The
warning exists because the production Egison-to-core bridge for these
occurrences has not yet been validated end to end. It must not state that Lean
cannot express or infer the pattern.

### 4.2 Primitive-pattern binder duplication or overlap

A fallback is reported when primitive-pattern captures are not pairwise
distinct, or when a primitive-pattern capture overlaps a primitive-data-pattern
arm binder in a scope for which the core requires disjoint contexts. This
includes repeated `#$name` captures and any production shadowing behavior that
would otherwise choose one of two bindings by environment order.

Once the production checker enforces the same distinctness and disjointness
conditions as the core, these cases should become ordinary type errors and no
longer use a compatibility fallback.

### 4.3 Undeclared primitive-pattern constructor fallback

The core requires every primitive-pattern constructor to be found in the
frozen pattern-constructor signature. Production's generic inference for an
undeclared `PPInductivePat` is an extension fallback and is reported. The
warning names the constructor and states that its result and field types were
not obtained from the frozen signature.

Removing generic undeclared-constructor inference and rejecting the construct
as an ordinary type error retires this warning category.

### 4.4 Legacy CAS views

The target-indexed virtual signature needed to justify legacy CAS views is not
part of the synchronized core bridge. Any special path that accepts views such
as `MathValue`, `PolyExpr`, `TermExpr`, `SymbolExpr`, or `IndexExpr` without the
ordinary frozen constructor/capability correspondence is reported.

The diagnostic must identify the legacy view boundary. It must not treat the
view as constructor evidence for unrelated ordinary capabilities.

### 4.5 Pattern functions awaiting the `DualScheme` bridge

A pattern-function definition or application is reported when its production
scheme has not been translated through the `DualScheme` bridge. Completion of
that bridge requires capability and target binders to be instantiated,
generalized, rigidly checked, and substituted as separate sorts, while
preserving the structural index of every parameter and result.

Ordinary pattern-function typing may continue unchanged while this warning is
active. A target-only scheme or a scheme reconstructed from an already-zonked
ordinary type is not sufficient evidence that the bridge is complete.

### 4.6 Non-core pattern forms

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

### 4.7 Protected or nested core-solver fallback

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
evidence by `--type-pm-compatibility-warnings`. Auditing a production program
against the complete core acceptance conditions therefore requires both
options. A matcher can exercise only synchronized inference operations while
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
have been separately validated. Compatibility warnings expose the remaining
boundary; they do not close it.
