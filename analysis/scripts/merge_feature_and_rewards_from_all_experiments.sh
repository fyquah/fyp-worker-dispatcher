#!/bin/bash

set -euo pipefail
set -x

cd $(dirname $0)/../../

echo "REWARD VERSION = $REWARD_VERSION"
echo "REWARD MODEL = $REWARD_MODEL"
echo "FEATURE VERSION = $FEATURE_VERSION"
FEATURE_VERSION_LOWER=$(echo -n "$FEATURE_VERSION" | tr '[:upper:]' '[:lower:]')
TMP=$(mktemp)

for exp in $(python scripts/query_experiment_params.py --all-old); do
  echo "(Unsegmented (
    ((name $exp)
     (features_file w/$exp)
     (rewards_file w/reward-$REWARD_VERSION/$REWARD_MODEL/$exp/rewards.sexp))))" > $TMP

  mkdir -p w/reward-$REWARD_VERSION-feature-$FEATURE_VERSION/$REWARD_MODEL/$exp/
  mkdir -p w/reward-$REWARD_VERSION/$REWARD_MODEL/$exp/

  ./_build/default/tools/local_reward_model.exe plots dump-data \
    -feature-version $FEATURE_VERSION \
    -spec $TMP \
    >w/reward-$REWARD_VERSION-feature-$FEATURE_VERSION/$REWARD_MODEL/$exp/feature_reward_pair_$FEATURE_VERSION_LOWER.sexp

  ./_build/default/tools/local_reward_model.exe plots dump-rewards \
    w/reward-$REWARD_VERSION/$REWARD_MODEL/$exp/rewards.sexp \
    >w/reward-$REWARD_VERSION/$REWARD_MODEL/$exp/rewards_dump.sexp \

  mkdir -p ./analysis/report_plots/reward_assignment/data/$REWARD_MODEL/$exp/

  cp w/reward-$REWARD_VERSION-feature-$FEATURE_VERSION/$REWARD_MODEL/$exp/feature_reward_pair_$FEATURE_VERSION_LOWER.sexp \
    ./analysis/report_plots/reward_assignment/data/$REWARD_MODEL/$exp/

  cp w/reward-$REWARD_VERSION/$REWARD_MODEL/$exp/rewards_dump.sexp \
    ./analysis/report_plots/reward_assignment/data/$REWARD_MODEL/$exp/
done
