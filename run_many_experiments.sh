#!/bin/bash

nmcli conn down "eduroam"
nmcli conn down "Imperial-WPA"

sudo /sbin/ifup enp2s0
sleep 1.0

set -euo pipefail

# List of experiments to run should be 
wait_for_completion() {
  echo "[BATCH EXECUTOR] Waiting for completion on `cat tmp/controller.pid`"
  while true; do
    STATUS="$(kill -0 `cat tmp/controller.pid` 2>/dev/null || echo -n "completed")"
    if [ "$STATUS" == "completed" ]; then
      break
    fi
    sleep 5.0
  done
  echo "[BATCH EXECUTOR] `cat tmp/controller.pid` done!"
}

BATCH_EXECUTOR_LOG=tmp/batch_executor.log

while true; do
  for script in $@; do
    echo "[BATCH EXECUTOR] RUNNING $script!"
  
    # call bash to that it executes in a subprocess
    bash "$script" prod
    # ## This is a test bed
    # echo "$!" >tmp/controller.pid
    # echo "$script" >tmp/controller.rundir
    # ## end of testbed
  
    RUNDIR=$(cat tmp/controller.rundir)
    echo "$script,$RUNDIR" >$BATCH_EXECUTOR_LOG
  
    wait_for_completion
  done
done
