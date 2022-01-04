#!/usr/bin/env bash
set -euo pipefail

mpic++ -std=c++17 -Wall -Wextra -Wpedantic -Wshadow $1
shift

mpirun --mca opal_warn_on_missing_libcuda 0 a.out $@
rm -rf a.out
