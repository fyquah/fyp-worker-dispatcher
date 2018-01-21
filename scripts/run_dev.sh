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
EXPERIMENTS_REPO=$RUNDIR/experiments

mkdir -p $RUNDIR

if [ ! -d "$EXPERIMENTS_REPO" ]; then
  git clone ~/fyp/experiments "$EXPERIMENTS_REPO"
fi

bash -c "cd $EXPERIMENTS_REPO && git pull"

cp dev_config.sexp $RUNDIR/config.sexp

if [[ ! -v ADDITIONAL_CONTROLLER_ARGS ]]; then
  ADDITIONAL_CONTROLLER_ARGS=""
fi

set -x

jbuilder exec controller -- \
  "$1" run \
  -config "$RUNDIR/config.sexp" \
  -rundir "$RUNDIR" \
  -exp-dir "$EXPERIMENTS_REPO/$EXPERIMENT_DIR" \
  -bin-name "$EXPERIMENT_BIN_NAME" \
  -args "$EXPERIMENT_BIN_ARGS" \
  $ADDITIONAL_CONTROLLER_ARGS
