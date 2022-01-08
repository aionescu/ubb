#!/usr/bin/env bash
set -euo pipefail

g++ -std=c++17 -Wall -Wshadow -pthread -O3 src/*.cpp
./a.out $@

rm -f a.out
