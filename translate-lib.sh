#!/bin/sh

for f in lib/??*/??*
do
    [ -d ${f} ] && continue
    [ "${f}" = "lib/core/assoc.segi" ] && continue
    [ "${f}" = "lib/core/base.segi" ] && continue
    [ "${f}" = "lib/core/collection.segi" ] && continue
    [ "${f}" = "lib/core/io.segi" ] && continue
    [ "${f}" = "lib/core/maybe.segi" ] && continue
    [ "${f}" = "lib/core/number.segi" ] && continue
    [ "${f}" = "lib/core/order.segi" ] && continue
    [ "${f}" = "lib/core/random.segi" ] && continue
    [ "${f}" = "lib/core/string.segi" ] && continue
    echo ${f}
    stack exec -- egison-translate ${f} > "nons-"${f%.segi}.egi
done

for f in lib/??*/??*/??*
do
    [ "${f}" = "lib/math/algebra/equations.segi" ] && continue
    [ "${f}" = "lib/math/algebra/inverse.segi" ] && continue
    [ "${f}" = "lib/math/algebra/matrix.segi" ] && continue
    [ "${f}" = "lib/math/algebra/root.segi" ] && continue
    [ "${f}" = "lib/math/algebra/tensor.segi" ] && continue
    [ "${f}" = "lib/math/algebra/vector.segi" ] && continue
    [ "${f}" = "lib/math/analysis/derivative.segi" ] && continue
    [ "${f}" = "lib/math/analysis/integral.segi" ] && continue
    # [ "${f}" = "lib/math/common/arithmetic.segi" ] && continue
    [ "${f}" = "lib/math/common/constants.segi" ] && continue
    [ "${f}" = "lib/math/common/functions.segi" ] && continue
    [ "${f}" = "lib/math/geometry/3d-euclidean-space.segi" ] && continue
    [ "${f}" = "lib/math/geometry/4d-euclidean-space.segi" ] && continue
    [ "${f}" = "lib/math/geometry/differential-form.segi" ] && continue
    [ "${f}" = "lib/math/geometry/minkowski-space.segi" ] && continue

    echo ${f}
    stack exec -- egison-translate ${f} > "nons-"${f%.segi}.egi
done
