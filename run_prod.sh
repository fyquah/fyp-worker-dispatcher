#!/bin/bash

nmcli conn down "eduroam"
nmcli conn down "Imperial-WPA"

sudo /sbin/ifup enp2s0
sleep 1.0

RUNDIR=../prod/controller-rundir/$(ocamlnow)
echo "RUNDIR = $RUNDIR"

jbuilder exec -- controller \
  -config deploy_config.sexp \
  -rundir "$RUNDIR" \
  -exp-dir ../experiments/async/cohttp \
  -bin-name cohttp_async_bench \
  -bin-args "20000" \
  1>"$RUNDIR/stdout.log"
