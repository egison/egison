#!/bin/sh

for f in lib/??*/??*
do
    [ -d ${f} ] && continue
    echo ${f}
    stack exec -- egison-translate ${f} > "nons-"${f}
done

for f in lib/??*/??*/??*
do
    echo ${f}
    stack exec -- egison-translate ${f} > "nons-"${f}
done
