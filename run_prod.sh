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

export OPAMROOT=$HOME/fyp/opam-root/
eval `opam config env`
opam switch 4.05.0+fyp

echo "Default OCAMLOPT = `which ocamlopt`"
echo "Algorithm = $1"

if [ "$1" = "" ]; then
  echo "Must provide controller optimization algorithm"
  exit 1
fi

# Don't run production cycle if code doesn't compile --- duh.
make controller

RUNDIR=~/fyp/prod/rundir/$(ocamlnow)
EXPERIMENTS_DIR=$RUNDIR/experiments

mkdir -p $RUNDIR

if [ ! -d "$EXPERIMENTS_DIR" ]; then
  git clone ~/fyp/experiments "$EXPERIMENTS_DIR"
fi

bash -c "cd $EXPERIMENTS_DIR && git pull"

cp config.sexp $RUNDIR/config.sexp

nohup jbuilder exec controller -- \
  "$1" \
  -config "$RUNDIR/config.sexp" \
  -rundir "$RUNDIR" \
  -exp-dir "$EXPERIMENTS_DIR/normal/almabench" \
  -bin-name almabench \
  -args "" \
  1>$RUNDIR/stdout.log 2>$RUNDIR/stderr.log &

PID="$!"

echo "$PID" >tmp/controller.pid
echo "$RUNDIR" >tmp/controller.rundir

echo "Piping controller stdout and stderr to $RUNDIR/stdout.log and $RUNDIR/stderr.log"
