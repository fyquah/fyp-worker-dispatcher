#!/bin/bash

set -euo pipefail

cd $(dirname $0)/..
for exp in $(python ../scripts/query_experiment_params.py --all); do
  mkdir -p ../w/$exp
  python ./print_linear_general_reward.py \
    --experiment-dir out/$exp/linear-general-reward-without-normalisation/decay-0.300000-ridge-0.003000-benefit-sigmoid_speedup_over_mean/ \
    --problem-dir out/$exp/ \
    --dump-rewards >../w/$exp/rewards.sexp
done
