#!/bin/bash

nmcli conn down "eduroam"
nmcli conn down "Imperial-WPA"

sudo /sbin/ifup enp2s0
sleep 1.0

# we set "safe mode" after the nmcli commands, which can (expectedly) fail
# with non fatal errors.
set -euo pipefail

scp _build/default/controller/main.exe \
  fyp--controller@0.0.0.0:~/prod/controller.exe
scp controller-scripts/run_prod.sh \
  fyp--controller@0.0.0.0:~/prod/run.sh
scp prod_config.sexp fyp--controller@0.0.0.0:~/prod/config.sexp
scp worker/benchmark_binary.sh fyp--controller@0.0.0.0:~/prod/benchmark_binary.sh

echo "Updating experiments repo"
ssh fyp--controller@0.0.0.0 "cd /home/fyp--controller/prod/experiments; pwd && git pull"

echo "Running controller"
ssh fyp--controller@0.0.0.0 \
  "cd /home/fyp--controller/prod/; pwd; ./run.sh \"$1\""

