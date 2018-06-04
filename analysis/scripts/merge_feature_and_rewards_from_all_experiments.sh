#!/bin/bash

set -euo pipefail
set -x

cd $(dirname $0)/../../

echo "VERSION = $VERSION"
TMP=$(mktemp)

for exp in $(python scripts/query_experiment_params.py --all-old); do
  echo "(Unsegmented (
    ((name $exp)
     (features_file w/$exp)
     (rewards_file w/$VERSION/$exp/rewards.sexp))))" > $TMP

  ./_build/default/tools/local_reward_model.exe plots dump-rewards \
    -feature-version V1 \
    -spec $TMP >w/$VERSION/$exp/feature_reward_pair.sexp
done
