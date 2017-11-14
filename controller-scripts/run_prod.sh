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
CONTROLLER="$(pwd)/controller.exe"

cd "$RUNDIR"

"$CONTROLLER" "$1" \
  -config config.sexp \
  -rundir "$RUNDIR" \
  -exp-dir ~/prod/experiments/async/cohttp \
  -bin-name cohttp_async_bench \
  -args "20000"
