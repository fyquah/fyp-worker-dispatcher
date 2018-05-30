#!/bin/bash

set -euo pipefail

PARALLEL_FLAGS="--jobs 4"

# learns everything in all non ill-defined experiments.
TASK_FILE=$(mktemp)
PROBLEMS="almabench  bdd  fft  floats-in-functor  hamming  kahan-sum  kb  lens  quicksort sequence-cps sequence lexifi"

for problem in $PROBLEMS; do
  # echo "./print_linear_general_reward.sh out/$problem &>tmp/$problem-learn-linear-general-reward.log" >>$TASK_FILE
  echo "./print_linear_general_reward_without_normalisation.sh out-$VERSION/$problem &>tmp/$problem-print-linear-general-reward-without-normalisation.log" >>$TASK_FILE
done

echo "Task file: $TASK_FILE" >/dev/stderr

set -x
parallel $PARALLEL_FLAGS -t <$TASK_FILE
