#!/bin/bash

set -euo pipefail

# NOT reporting inlining decisions is REALLY REALLY important, otherwise, compilation
# will end up consuming excessive amounts of RAM.
export OCAMLOPT_TIMEOUT="1m"
export FYP_COMPILATION_TIMEOUT="10m"
export PATH="$HOME/fyp/worker-dispatcher/hacks:$PATH"

export OCAMLOPT_TIMEOUT="1m"
make build/plugin_random.cmxs

for exp in $(python ~/fyp/worker-dispatcher/scripts/query_experiment_params.py --all-old); do
  for i in $(seq 0 100); do
    cd ~/fyp/worker-dispatcher/tools/fyp_compiler_plugins
    cp build/plugin_random.cmxs build/plugin_random_$(((i * 3) + 0)).cmxs
    cp build/plugin_random.cmxs build/plugin_random_$(((i * 3) + 1)).cmxs
    cp build/plugin_random.cmxs build/plugin_random_$(((i * 3) + 2)).cmxs

    cd ../../
    CONFIG_FILE_OVERRIDE=prod-configs/only-worker-0.sexp \
      ./scripts/run-plugin-benchmarks plugin_random_$(((i * 3) + 0)) $exp &
    sleep 2.0

    CONFIG_FILE_OVERRIDE=prod-configs/only-worker-1.sexp \
      ./scripts/run-plugin-benchmarks plugin_random_$(((i * 3) + 1)) $exp &
    sleep 2.0

    CONFIG_FILE_OVERRIDE=prod-configs/only-worker-2.sexp \
      ./scripts/run-plugin-benchmarks plugin_random_$(((i * 3) + 2)) $exp &
    sleep 2.0
    echo "Waiting"
    wait
  done
done
