#!/usr/bin/env bash

TEST_DIR=test
# find $TEST_DIR -type f | xargs -L 1 -t stack exec egison -- -t -N
echo "syntax"
stack exec egison -- -t -N dev/test/syntax.egi
echo "primitive"
stack exec egison -- -t -N dev/test/primitive.egi
echo "base"
stack exec egison -- -t -N dev/test/lib/core/base.egi
echo "collection"
stack exec egison -- -t -N dev/test/lib/core/collection.egi
echo "number"
stack exec egison -- -t -N dev/test/lib/core/number.egi
echo "order"
stack exec egison -- -t -N dev/test/lib/core/order.egi
echo "string"
stack exec egison -- -t -N dev/test/lib/core/string.egi
echo "algebra"
stack exec egison -- -t -N dev/test/lib/math/algebra.egi
echo "analysis"
stack exec egison -- -t -N dev/test/lib/math/analysis.egi
echo "arithmetic"
stack exec egison -- -t -N dev/test/lib/math/arithmetic.egi
