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
opam switch 4.05.0+fyp
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

if [ -z ${CONFIG_FILE_OVERRIDE+x} ]; then
  cp prod-configs/all.sexp $RUNDIR/config.sexp
else
  echo "INFO: OVERRIDE CONFIG FILE WITH $CONFIG_FILE_OVERRIDE"
  cp "$CONFIG_FILE_OVERRIDE" $RUNDIR/config.sexp
fi


if [[ ! -v ADDITIONAL_CONTROLLER_ARGS ]]; then
  ADDITIONAL_CONTROLLER_ARGS=""
fi

if [ -z ${EXPERIMENT_TMP_DIR+x} ]; then
  EXPERIMENT_TMP_DIR=tmp
else
  echo "INFO: OVERRIDE TMP DIR WITH $EXPERIMENT_TMP_DIR"
fi

if [ -z ${EXPERIMENT_OCAMLPARAM+x} ]; then
  EXPERIMENT_OCAMLPARAM=_
else
  echo "INFO: OVERRIDE OCAMLPARAM WITH $EXPERIMENT_OCAMLPARAM"
fi

mkdir -p $EXPERIMENT_TMP_DIR

set -x
OPAMROOT=~/.opam OCAMLPARAM="$EXPERIMENT_OCAMLPARAM" nohup _build/install/default/bin/controller \
  "$1" run \
  -config "$RUNDIR/config.sexp" \
  -rundir "$RUNDIR" \
  -exp-dir "$EXPERIMENTS_REPO/$EXPERIMENT_DIR" \
  -bin-name "$EXPERIMENT_BIN_NAME" \
  -args "$EXPERIMENT_BIN_ARGS" \
  -module-paths "$EXPERIMENT_MODULE_PATHS" \
  $ADDITIONAL_CONTROLLER_ARGS \
  1>$RUNDIR/stdout.log 2>$RUNDIR/stderr.log &
PID="$!"
set +x

echo "$PID" >$EXPERIMENT_TMP_DIR/controller.pid
echo "$RUNDIR" >$EXPERIMENT_TMP_DIR/controller.rundir

echo "Piping controller stdout and stderr to $RUNDIR/stdout.log and $RUNDIR/stderr.log"
