#!/bin/bash

set -euo pipefail

echo "VERSION = $VERSION"
echo "MODEL= $MODEL"

cd $(dirname $0)/..


for exp in $(python ../scripts/query_experiment_params.py --all-old); do
  mkdir -p ../w/reward-$VERSION/$MODEL/$exp
  echo "dumping to $(realpath ../w/reward-$VERSION/$MODEL/$exp/rewards.sexp)"
  python ./print_linear_general_reward.py \
    --experiment-dir out-$VERSION/$exp/lasso/decay-1.000000-benefit-tanh_speedup_over_baseline-lasso-factor-auto \
    --problem-dir out-$VERSION/$exp/ \
    --dump-rewards \
    >../w/reward-$VERSION/$MODEL/$exp/rewards.sexp

    # --experiment-dir out-$VERSION/$exp/linear-general-reward-without-normalisation/decay-1.000000-ridge-0.050000-benefit-tanh_speedup_over_baseline \

    # --experiment-dir out-$VERSION/$exp/$MODEL/$(cat out-$VERSION/$MODEL/best-hyperparams.sexp | sexp-query  "(field $exp)") \
    # --experiment-dir out-$VERSION/$exp/lasso/decay-1.000000-benefit-tanh_speedup_over_baseline-lasso-factor-auto \
    # --experiment-dir out-$VERSION/$exp/linear-general-reward-without-normalisation/decay-1.000000-ridge-0.050000-benefit-tanh_speedup_over_mean \
done
