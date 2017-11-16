#!/bin/bash

set -euo pipefail

export OPAM_ROOT=$HOME/fyp/opam-root/
eval `opam config env`
opam switch 4.05.0+fyp

echo "Algorithm = $1"

if [ "$1" = "" ]; then
  echo "Must provide algorithm"
  exit 1
fi

echo $PATH

RUNDIR=~/prod/rundir/$(ocamlnow)

mkdir -p tmp/dev/

cp config.sexp $RUNDIR/config.sexp

nohup jbuilder exec controller -- \
  "$1" \
  -config "$RUNDIR/config.sexp" \
  -rundir "$RUNDIR" \
  -exp-dir ~/prod/experiments/normal/almabench \
  -bin-name almabench \
  -args "" \
  1>stdout.log 2>stderr.log &

PID=$!

echo $PID >tmp/dev/controller.pid
echo $RUNDIR >tmp/dev/controller.rundir

echo "PID = $PID"
echo "RUNDIR = $RUNDIR"
