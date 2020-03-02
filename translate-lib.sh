#!/bin/sh

for f in lib/??*/??*
do
    [ -d ${f} ] && continue
    [ "${f}" = "lib/core/sexpr.segi" ] && continue
    [ "${f}" = "lib/core/collection.segi" ] && continue
    [ "${f}" = "lib/core/base.segi" ] && continue
    [ "${f}" = "lib/core/maybe.segi" ] && continue
    echo ${f}
    stack exec -- egison-translate ${f} > "nons-"${f%.segi}.egi
done

for f in lib/??*/??*/??*
do
    echo ${f}
    stack exec -- egison-translate ${f} > "nons-"${f%.segi}.egi
done
