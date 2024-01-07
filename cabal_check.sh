#! /usr/bin/env bash

set -e

for cabalfile in $(ls */*.cabal)
do
    pushd $(dirname $cabalfile)
    cabal check
    popd
done
