# Egison Tests

The cabal test suite is `test` (test/Test.hs). It evaluates Egison
programs and checks their `assertEqual`s, in three groups:

* **Language tests** — `test/syntax.egi`, `test/primitive.egi`.
* **Library unit tests** — every `test/lib/**/*.egi` is discovered
  automatically, so a new suite dropped there runs without editing
  `Test.hs`. Files that must not run are listed in `skippedLibTests`
  with the reason, and the skips are printed at startup.
* **Sample programs** — a curated list in `sampleTests`, each
  registered for the language feature it exercises.

Each test file is evaluated in **one batch together with the core and
math libraries**, mirroring the interpreter's initial load. This
matters for the CAS tests: `declare rule auto` / `declare ideal` in a
test file rewrite `mathNormalize` for the whole batch, so a separate
library batch would keep the rules from ever firing.

## How to run

```
$ cabal test
```

To run selected files, pass `--select-tests` through to
test-framework (test labels are the file paths):

```
$ cabal test --test-options='--select-tests=test/syntax.egi'
```

When checking CAS changes, also grep the log for stray diagnostics
(the suite is expected to be clean):

```
$ gtimeout 900 cabal test 2>&1 | tee /tmp/cabal-test.log
$ grep -E "Warning:|Type error:|Evaluation error:|Unbound variable" /tmp/cabal-test.log
```

## Checks for the paper on type inference for pattern matching

The test suite `paper-artifact` (`test/PaperArtifact.hs`) reproduces the
interpreter results reported in the appendices of the paper:

* the four collection matchers of `lib/core/collection.egi` are inferred
  without their type annotations, with the same types as the annotated
  definitions, and `test/lib/core/collection.egi` passes with and without
  the annotations;
* the paper's list and set matcher listings are inferred without type
  annotations and pass their cases; the listings are read from the
  paper-source directory given by `PAPER_SOURCES`, and this check is
  skipped when the variable is not set;
* the Egison side of the comparison with Haskell embeddings, and the paper
  examples of `test/lib/core/type-pm-examples.egi` under strict checking.

```
$ PAPER_SOURCES=/absolute/path/to/paper-source cabal test paper-artifact
```

The Sweet Egison side of the comparison is the separate package
`test/sweet-egison-comparison`, so that this package does not depend on
Sweet Egison:

```
$ cd test/sweet-egison-comparison && cabal test
```

## Not wired

`test/CLITest.hs` (command-line option tests) predates the move to
cabal: it shells out via `stack exec` and its `test/fixture/` files no
longer exist. It is not part of any cabal test-suite; reviving it
means porting the invocations to `cabal run egison --` and recreating
the fixtures.
