#!/usr/bin/env bash
set -euo pipefail

mpic++ -std=c++17 -Wall -Wshadow -pthread -O3 -D USE_MPI -o blur-mpi.out src/*.cpp
