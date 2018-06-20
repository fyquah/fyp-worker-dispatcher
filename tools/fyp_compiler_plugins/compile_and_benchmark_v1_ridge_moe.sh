#!/bin/bash

set -euo pipefail

PLUGIN_NAME=plugin_v1_neural_network_ridge_moe

cd $(dirname $0)

make clean
mkdir -p training-logs/$PLUGIN_NAME
export TRAINING_LOG_DIR="$(pwd)/training-logs/$PLUGIN_NAME/"
./train_plugin_v1_ridge_moe_neural_network.sh &>/dev/stdout | tee training-logs/$PLUGIN_NAME/out.log

make build/plugin_v1_neural_network.cmxs
mv build/plugin_v1_neural_network.cmxs build/plugin_v1_neural_network_ridge_moe.cmxs
cd ../../
./scripts/run-plugin-benchmarks plugin_v1_neural_network_ridge_moe
