#!/usr/bin/env bash

TEST_DIR=nons-test/test
find $TEST_DIR -type f -name "*.egi" | xargs -L 1 -t stack exec egison -- -t -N
