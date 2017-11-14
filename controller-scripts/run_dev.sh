#!/bin/bash

set -euo pipefail

echo "$1"

if [ "$1" = "" ]; then
  echo "Must provide algorithm"
  exit 1
fi

source ~/.bash_profile
echo $PATH

mkdir -p ~/dev/rundir
mkdir -p ~/dev/rundir/worker/
cd ~/dev/rundir

cp ../dev_config.sexp ./
cp ../benchmark_binary.sh ./worker/benchmark_binary.sh
RUNDIR=$(pwd)

../controller.exe "$1" \
  -config dev_config.sexp \
  -rundir "$RUNDIR" \
  -exp-dir ../experiments/async/cohttp \
  -bin-name cohttp_async_bench \
  -args "10000" \
  -debug
