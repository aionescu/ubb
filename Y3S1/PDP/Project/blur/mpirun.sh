#!/usr/bin/env bash
set -euo pipefail

mpic++ -std=c++17 -Wall -Wshadow -pthread -O3 -D USE_MPI src/*.cpp

mpirun --mca opal_warn_on_missing_libcuda 0 a.out $@
rm -f a.out
