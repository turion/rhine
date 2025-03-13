#! /usr/bin/env bash

set -e

for cabalfile in $(ls */*.cabal)
do
    pushd $(dirname $cabalfile)
    cabal check
    cabal-gild --io=$(basename $cabalfile)
    popd
done
git diff --exit-code
