#!/usr/bin/env bash
set -euo pipefail

g++ -std=c++17 -Wall -Wshadow -pthread -O3 -o blur-mt.out src/*.cpp
mpic++ -std=c++17 -Wall -Wshadow -pthread -O3 -D USE_MPI -o blur-mpi.out src/*.cpp
