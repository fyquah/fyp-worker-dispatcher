#!/bin/bash

PATH_TO_BINARY="$1"
TASKSET_MASK="$2"
shift
shift
ARGS="$@"

# It is guranteed that the binary is located at the [rundir] of the worker.
cd $(dirname "$0")

set -euo pipefail

# Recording only the "important" stats
perf stat -x, \
  -e branches \
  -e branch-misses \
  -e L1-icache-load-misses \
  -e branch-load-misses \
  -e branch-loads \
  -e iTLB-load-misses \
  -e iTLB-loads \
  -e cpu-cycles \
  -e instructions \
  -- taskset "$TASKSET_MASK" "$PATH_TO_BINARY" $ARGS 2>&1 1>/dev/null

echo "*"

# Record practically all events
# Hardware counters that are removed are:
#    node-store-misses => Modern caches highly local, this is almost always 0
#    node-load-misses  => ``
#    LLC-load-misses   => Too high variance, hard to maintain over diff runs
#    LLC-store-misses  => ``

# TODO(fyq14): What is the difference between [cache-misses] and
#              [LLC-cache-misses] ?

perf stat -r 2 -x, \
  -e branches \
  -e branch-misses \
  -e L1-icache-load-misses \
  -e branch-load-misses \
  -e branch-loads \
  -e iTLB-load-misses \
  -e iTLB-loads \
  -e cpu-cycles \
  -e instructions \
  -e L1-dcache-load-misses \
  -e L1-dcache-loads \
  -e L1-dcache-stores \
  -e LLC-loads \
  -e LLC-stores \
  -e cache-misses \
  -e bus-cycles \
  -e cache-references \
  -e ref-cycles \
  -e dTLB-load-misses \
  -e dTLB-loads \
  -e dTLB-store-misses \
  -e dTLB-stores \
  -e node-loads \
  -e node-stores \
  -- taskset "$TASKSET_MASK" "$PATH_TO_BINARY" $ARGS 2>&1 1>/dev/null

echo "*"

# call graph (we don't care about kernel events, just to save space ...)

OCAML_GC_STATS=/dev/stderr \
  perf record -q -F 1000 -e cycles:u --call-graph=dwarf -o $HOME/perf.data \
  -- taskset "$TASKSET_MASK" "$PATH_TO_BINARY" $ARGS 2>&1 1>/dev/null
