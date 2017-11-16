#!/bin/bash

set -euo pipefail

export OPAMROOT=$HOME/fyp/opam-root/
eval `opam config env`
opam switch 4.05.0+fyp

echo "Default OCAMLOPT = `which ocamlopt`"
echo "Algorithm = $1"

if [ "$1" = "" ]; then
  echo "Must provide controller optimization algorithm"
  exit 1
fi

echo $PATH

RUNDIR=~/fyp/dev/rundir/

mkdir -p tmp/dev/

cp config.sexp $RUNDIR/config.sexp

nohup jbuilder exec controller -- \
  "$1" \
  -config "$RUNDIR/config.sexp" \
  -rundir "$RUNDIR" \
  -exp-dir ~/fyp/prod/experiments/normal/almabench \
  -bin-name almabench \
  -args "" \
  1>$RUNDIR/stdout.log 2>$RUNDIR/stderr.log &

PID=$!

echo $PID >tmp/dev/controller.pid
echo $RUNDIR >tmp/dev/controller.rundir

echo "PID = $PID"
echo "RUNDIR = $RUNDIR"
