#!/bin/bash

set -euo pipefail

REWARD_CLS=$1
SIGNIFIANCE_DIFF=$2
PLUGIN_NAME=plugin_v1_neural_network_ridge_"$REWARD_CLS"_$SIGNIFIANCE_DIFF

cd $(dirname $0)

make clean
mkdir -p training-logs/
./train_plugin_v1_ridge_neural_network.sh "$REWARD_CLS" "$SIGNIFIANCE_DIFF" &>/dev/stdout | tee training-logs/$PLUGIN_NAME.log
make build/plugin_v1_neural_network.cmxs
mv build/plugin_v1_neural_network.cmxs build/$PLUGIN_NAME.cmxs
cd ../../
./scripts/run-plugin-benchmarks $PLUGIN_NAME
