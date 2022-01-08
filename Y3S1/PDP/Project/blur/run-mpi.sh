#!/usr/bin/env bash
set -euo pipefail

[ -f blur-mpi.out ] || ./build-mpi.sh
mpirun --mca opal_warn_on_missing_libcuda 0 blur-mpi.out $@
