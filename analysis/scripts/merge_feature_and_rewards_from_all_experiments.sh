#!/bin/bash

set -euo pipefail
set -x

cd $(dirname $0)/../../

echo "REWARD VERSION = $REWARD_VERSION"
echo "FEATURE VERSION = $FEATURE_VERSION"
TMP=$(mktemp)

for exp in $(python scripts/query_experiment_params.py --all-old); do
  echo "(Unsegmented (
    ((name $exp)
     (features_file w/$exp)
     (rewards_file w/reward-$REWARD_VERSION/$exp/rewards.sexp))))" > $TMP

  mkdir -p w/reward-$REWARD_VERSION-feature-$FEATURE_VERSION/$exp/
  mkdir -p w/reward-$REWARD_VERSION/$exp/

  ./_build/default/tools/local_reward_model.exe plots dump-data \
    -feature-version $FEATURE_VERSION \
    -spec $TMP \
    >w/reward-$REWARD_VERSION-feature-$FEATURE_VERSION/$exp/feature_reward_pair.sexp

  ./_build/default/tools/local_reward_model.exe plots dump-rewards \
    w/reward-$REWARD_VERSION/$exp/rewards.sexp \
    >w/reward-$REWARD_VERSION/$exp/rewards_dump.sexp \

  cp w/reward-$REWARD_VERSION-feature-$FEATURE_VERSION/$exp/feature_reward_pair.sexp \
    ./analysis/report_plots/reward_assignment/data/$exp/

  cp w/reward-$REWARD_VERSION/$exp/rewards_dump.sexp \
    ./analysis/report_plots/reward_assignment/data/$exp/
done
