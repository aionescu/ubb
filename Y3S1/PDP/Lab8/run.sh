#!/usr/bin/env bash
set -euo pipefail

[ -f dsm.out ] || ./build.sh

NP=$1
shift

mpirun -np "$NP" --mca opal_warn_on_missing_libcuda 0 dsm.out $@
