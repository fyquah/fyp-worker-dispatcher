#!/bin/bash

set -euo pipefail

echo "VERSION = $VERSION"

cd $(dirname $0)/..

for exp in $(python ../scripts/query_experiment_params.py --all-old); do
  mkdir -p ../w/$VERSION/$exp
  python ./print_linear_general_reward.py \
    --experiment-dir out-$VERSION/$exp/linear-general-reward-without-normalisation/decay-1.000000-ridge-0.050000-benefit-tanh_speedup_over_mean \
    --problem-dir out-$VERSION/$exp/ \
    --dump-rewards >../w/$VERSION/$exp/rewards.sexp
    # --experiment-dir out-$VERSION/$exp/linear-general-reward-without-normalisation/$(cat out-$VERSION/best-hyperparams.sexp | sexp-query  "(field $exp)") \
done
