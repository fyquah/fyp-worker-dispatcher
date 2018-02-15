#!/bin/bash

echo "Switching off connection to college network , switching on \
  local connection"
nmcli conn down "eduroam"
nmcli conn down "Imperial-WPA"

sudo /sbin/ifup enp2s0
sleep 1.0

# we set "safe mode" after the nmcli commands, which can (expectedly) fail
# with non fatal errors.
set -euo pipefail

# Don't run production cycle if code doesn't compile --- duh.
make controller

export OPAMROOT=$HOME/fyp/opam-root/
eval `opam config env`
opam switch 4.05.0+fyp

echo "Default OCAMLOPT = `which ocamlopt`"
echo "Algorithm = $1"

if [ "$1" = "" ]; then
  echo "Must provide controller optimization algorithm"
  exit 1
fi

RUNDIR=~/fyp/prod/rundir/$(ocamlnow | sed -e 's/\:/_/g')
EXPERIMENTS_REPO=$RUNDIR/experiments

mkdir -p $RUNDIR

if [ ! -d "$EXPERIMENTS_REPO" ]; then
  git clone ~/fyp/experiments "$EXPERIMENTS_REPO"
fi

bash -c "cd $EXPERIMENTS_REPO && git pull"

cp prod_config.sexp $RUNDIR/config.sexp

if [[ ! -v ADDITIONAL_CONTROLLER_ARGS ]]; then
  ADDITIONAL_CONTROLLER_ARGS=""
fi

mkdir -p tmp

nohup jbuilder exec controller -- \
  "$1" run \
  -config "$RUNDIR/config.sexp" \
  -rundir "$RUNDIR" \
  -exp-dir "$EXPERIMENTS_REPO/$EXPERIMENT_DIR" \
  -bin-name "$EXPERIMENT_BIN_NAME" \
  -args "$EXPERIMENT_BIN_ARGS" \
  $ADDITIONAL_CONTROLLER_ARGS \
  1>$RUNDIR/stdout.log 2>$RUNDIR/stderr.log &

PID="$!"

echo "$PID" >tmp/controller.pid
echo "$RUNDIR" >tmp/controller.rundir

echo "Piping controller stdout and stderr to $RUNDIR/stdout.log and $RUNDIR/stderr.log"
