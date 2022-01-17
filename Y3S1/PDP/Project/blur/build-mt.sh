#!/usr/bin/env bash
set -euo pipefail

g++ -std=c++17 -Wall -Wpedantic -Wshadow -pthread -O3 -o blur-mt.out src/*.cc
