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

RUNDIR=~/fyp/dev/rundir
EXPERIMENTS_DIR=$RUNDIR/experiments

mkdir -p $RUNDIR

if [ ! -d "$EXPERIMENTS_DIR" ]; then
  git clone ~/fyp/experiments "$EXPERIMENTS_DIR"
fi

bash -c "cd $EXPERIMENTS_DIR && git pull"

cp config.sexp $RUNDIR/config.sexp

jbuilder exec controller -- \
  "$1" \
  -config "$RUNDIR/config.sexp" \
  -rundir "$RUNDIR" \
  -exp-dir "$EXPERIMENTS_DIR/normal/almabench" \
  -bin-name almabench \
  -args ""
