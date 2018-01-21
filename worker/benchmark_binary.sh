#!/bin/bash

PATH_TO_BINARY="$1"
NUM_RUNS="$2"
TASKSET_MASK="$3"
shift
shift
shift
ARGS="$@"

set -euo pipefail

function extract_time {
  grep "real" \
    | perl -e 'while(<>){ /(\d+)m((\d+\.)?\d+)s/; print ($1 * 60.0 + $2); }'
}

for i in $(seq 1 "$NUM_RUNS"); do
  { time taskset "$TASKSET_MASK" "$PATH_TO_BINARY" $ARGS 2>&1 1>/dev/null ; } 2>out_time.txt
  cat out_time.txt | extract_time
  echo ""
done
