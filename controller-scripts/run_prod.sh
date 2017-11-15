#!/bin/bash

set -euo pipefail

echo "$1"

if [ "$1" = "" ]; then
  echo "Must provide algorithm"
  exit 1
fi

source ~/.bash_profile
echo $PATH

cd ~/prod/
RUNDIR=~/prod/rundir/$(ocamlnow)

mkdir -p $RUNDIR/worker/

cp config.sexp $RUNDIR/config.sexp
cp benchmark_binary.sh $RUNDIR/worker/benchmark_binary.sh
cp get_gc_stats.sh $RUNDIR/worker/get_gc_stats.sh
CONTROLLER="$(pwd)/controller.exe"

cd "$RUNDIR"

"$CONTROLLER" "$1" \
  -config config.sexp \
  -rundir "$RUNDIR" \
  -exp-dir ~/prod/experiments/normal/floats-in-functor \
  -bin-name b \
  -args "500000000 1000.0"
