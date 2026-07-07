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

## Not wired

`test/CLITest.hs` (command-line option tests) predates the move to
cabal: it shells out via `stack exec` and its `test/fixture/` files no
longer exist. It is not part of any cabal test-suite; reviving it
means porting the invocations to `cabal run egison --` and recreating
the fixtures.
