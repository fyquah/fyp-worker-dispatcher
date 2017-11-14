#!/bin/bash

set -euo pipefail

scp _build/default/controller/main.exe \
  fyp--controller@0.0.0.0:~/dev/controller.exe
scp run_dev_on_controller_user.sh \
  fyp--controller@0.0.0.0:~/dev/run.sh
scp dev_config.sexp fyp--controller@0.0.0.0:~/dev/dev_config.sexp
scp worker/benchmark_binary.sh fyp--controller@0.0.0.0:~/dev/benchmark_binary.sh

echo "Updating experiments repo"
ssh fyp--controller@0.0.0.0 "cd /home/fyp--controller/dev/experiments; pwd && git pull"

echo "Running controller"
ssh fyp--controller@0.0.0.0 "cd /home/fyp--controller/dev/; pwd; ./run.sh \"$1\""
