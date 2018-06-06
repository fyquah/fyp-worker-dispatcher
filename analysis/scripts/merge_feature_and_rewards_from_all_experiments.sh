#!/bin/bash

set -euo pipefail
set -x

cd $(dirname $0)/../../

echo "VERSION = $REWARD_VERSION"
TMP=$(mktemp)

for exp in $(python scripts/query_experiment_params.py --all-old); do
  echo "(Unsegmented (
    ((name $exp)
     (features_file w/$exp)
     (rewards_file w/$REWARD_VERSION/$exp/rewards.sexp))))" > $TMP

  ./_build/default/tools/local_reward_model.exe plots dump-v1-data \
    -feature-version V1 \
    -spec $TMP >w/$REWARD_VERSION/$exp/feature_reward_pair.sexp

  ./_build/default/tools/local_reward_model.exe plots dump-rewards \
    w/$REWARD_VERSION/$exp/rewards.sexp \
    >w/$REWARD_VERSION/$exp/rewards_dump.sexp \

  cp w/$REWARD_VERSION/$exp/feature_reward_pair.sexp \
    ./analysis/report_plots/reward_assignment/data/$exp/

  cp w/$REWARD_VERSION/$exp/rewards_dump.sexp \
    ./analysis/report_plots/reward_assignment/data/$exp/
done
