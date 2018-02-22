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

for script in $@; do
  if [ ! -f "$script" ]; then
    echo "$script doesn't exists!"
    exit 1
  fi

  echo "Checked $script"
done

while true; do
  for script in $@; do
    echo "[BATCH EXECUTOR] DISPATCHING $script!"
  
    # call bash to that it executes in a subprocess
    for worker in $(seq 0 2); do
      EXPERIMENT_TMP_DIR=tmp-$worker \
        CONFIG_FILE_OVERRIDE=./prod-configs/only-worker-$worker.sexp \
        bash "$script" prod
      RUNDIR=$(cat tmp-$worker/controller.rundir)
      echo "$script,$RUNDIR" >>$BATCH_EXECUTOR_LOG
    done

    echo "Going to wait for $(cat tmp-0/controller.pid) , $(cat tmp-1/controller.pid) and $(cat tmp-2/controller.pid)"
    wait_for_completion $(cat tmp-0/controller.pid)
    wait_for_completion $(cat tmp-1/controller.pid)
    wait_for_completion $(cat tmp-2/controller.pid)
  done
done
