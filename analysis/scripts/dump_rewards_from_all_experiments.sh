#!/bin/bash

set -euo pipefail

echo "VERSION = $VERSION"

cd $(dirname $0)/..

set -x

for exp in $(python ../scripts/query_experiment_params.py --all-old); do
  echo "dumping to $(realpath ../w/reward-$VERSION/$exp/rewards.sexp)"
  mkdir -p ../w/reward-$VERSION/$exp
  python ./print_linear_general_reward.py \
    --experiment-dir out-$VERSION/$exp/linear-general-reward-without-normalisation/$(cat out-$VERSION/best-hyperparams.sexp | sexp-query  "(field $exp)") \
    --problem-dir out-$VERSION/$exp/ \
    --dump-rewards >../w/reward-$VERSION/$exp/rewards.sexp
    # --experiment-dir out-$VERSION/$exp/linear-general-reward-without-normalisation/decay-1.000000-ridge-0.050000-benefit-tanh_speedup_over_mean \
done
