# Egison Tests

Currently, there are 2 test suites:
* `test` executes several egison programs and checks if the results are expected.
* `test-cli` tests command line options.

## How to Run Tests

To run all tests, simply execute the following.
(We recommend _not_ to use `--fast` option, as some tests can take very long time without compiler optimizations.)
```
$ stack test
```

To run selected test suites, you can add `egison:[test suite name]`.
```
$ stack test egison:test
$ stack test egison:test-cli
```

For the `test` suite, you can use `--test-arguments` (or `--ta` for short) to specify which program to test.
```
$ stack test egison:test --test-arguments=--select-tests="test/syntax.egi"
$ stack test egison:test --ta=--select-tests="test/syntax.egi"
```
