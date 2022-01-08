#!/usr/bin/env bash
set -euo pipefail

[ -f blur-mt.out ] || ./build-mt.sh
./blur-mt.out $@
