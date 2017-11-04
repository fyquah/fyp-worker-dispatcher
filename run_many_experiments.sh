#!/bin/bash

nmcli conn down "eduroam"
nmcli conn down "Imperial-WPA"

sudo /sbin/ifup enp2s0
sleep 1.0

for i in $(seq 2 12); do
  RUNDIR=../prod/controller-rundir/$i

  mkdir -p "$RUNDIR"
  nohup jbuilder exec -- controller \
    -config deploy_config.sexp \
    -rundir "$RUNDIR" \
    1>"$RUNDIR/stdout.log" \
    2>"$RUNDIR/stderr.log" &

  PID="$!"
  echo "Experiment run $i with PID = $PID"
  wait "$PID"
done
