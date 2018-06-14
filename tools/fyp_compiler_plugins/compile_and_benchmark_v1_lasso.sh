#!/bin/bash

set -euo pipefail

REWARD_CLS=$1
PLUGIN_NAME=plugin_v1_neural_network_lasso_$REWARD_CLS

cd $(dirname $0)

make clean
mkdir -p training-logs/$PLUGIN_NAME
export TRAINING_LOG_DIR="$(pwd)/training-logs/$PLUGIN_NAME/"
./train_plugin_v1_lasso_neural_network.sh "$REWARD_CLS" &>/dev/stdout | tee training-logs/$PLUGIN_NAME/out.log

make build/plugin_v1_neural_network.cmxs
mv build/plugin_v1_neural_network.cmxs build/plugin_v1_neural_network_lasso_$REWARD_CLS.cmxs
cd ../../
./scripts/run-plugin-benchmarks plugin_v1_neural_network_lasso_$REWARD_CLS
