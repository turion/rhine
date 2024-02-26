#! /usr/bin/env bash

set -e

for cabalfile in $(ls */*.cabal)
do
    pushd $(dirname $cabalfile)
    cabal check
    cabal-gild --mode=check --input=$(basename $cabalfile)
    popd
done
