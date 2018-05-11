#!/bin/bash

nmcli conn down "eduroam"
nmcli conn down "Imperial-WPA"

sudo /sbin/ifup enp2s0
sleep 1.0

set -euo pipefail

# List of experiments to run should be 
wait_for_completion() {
  PROCESS_PID=$1
  echo "[BATCH EXECUTOR] Waiting for completion on $PROCESS_PID"
  while true; do
    STATUS="$(kill -0 "$PROCESS_PID" 2>/dev/null || echo -n "completed")"
    if [ "$STATUS" == "completed" ]; then
      break
    fi
    sleep 5.0
  done
  echo "[BATCH EXECUTOR] `cat tmp/controller.pid` done!"
}

mkdir -p important-logs/
BATCH_EXECUTOR_LOG=important-logs/batch_executor.log

TASK_FILE=$1

while true; do
  cat $TASK_FILE | while read line; do
    echo "[BATCH EXECUTOR] DISPATCHING {$line}!"
  
    # call bash to that it executes in a subprocess
    for worker in $(seq 0 2); do
      EXPERIMENT_TMP_DIR=tmp-$worker \
        CONFIG_FILE_OVERRIDE=./prod-configs/only-worker-$worker.sexp \
        bash $line prod
      RUNDIR=$(cat tmp-$worker/controller.rundir)
      echo "$line,$RUNDIR" >>$BATCH_EXECUTOR_LOG
    done

    echo "Going to wait for $(cat tmp-0/controller.pid) $(cat tmp-1/controller.pid) $(cat tmp-2/controller.pid)"
    wait_for_completion $(cat tmp-0/controller.pid)
    wait_for_completion $(cat tmp-1/controller.pid)
    wait_for_completion $(cat tmp-2/controller.pid)
  done
done
