#!/usr/bin/env bash
set -euo pipefail

mpic++ -std=c++17 -Wall -Wextra -Wpedantic -Wshadow -g -no-pie -fno-pie -o dsm.out src/*.cc
