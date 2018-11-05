#!/usr/bin/env bash

TEST_DIR=nons-test/test
find $TEST_DIR -type f -name "*.egi" | xargs -L 1 -t stack exec egison -- -t -N
stack exec egison -- -t -N nons-test/loadpi.egi
stack exec egison -- -t -N nons-test/new-tensor.egi
