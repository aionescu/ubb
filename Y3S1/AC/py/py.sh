#!/usr/bin/env bash
set -euo pipefail


LOG_PATH="/home/oldpug/Desktop/Study/Runs.csv"
mkdir -p $(dirname $LOG_PATH)

date +%s%3N >> "$LOG_PATH"
python3 $@
