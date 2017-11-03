#!/bin/bash

PATH_TO_BINARY="$1"
NUM_RUNS="$2"

function extract_time {
  grep "real" \
    | perl -e 'while(<>){ /(\d+)m((\d+\.)?\d+)s/; print ($1 * 60.0 + $2); }'
}

for i in $(seq 1 "$NUM_RUNS"); do
  TIME_OUTPUT=$((time (cat w/random1M.bin | "$PATH_TO_BINARY" >/dev/null 2>/dev/null)) 2>&1)

  # I don't know why this additional echo is needed -- but since it works i
  # don't question the lord of shell scripts.
  echo "$TIME_OUTPUT" | extract_time
  echo ""
done
