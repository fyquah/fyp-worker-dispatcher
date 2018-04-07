#!/bin/bash

set -euo pipefail

# learns everything in all non ill-defined experiments.
TASK_FILE=$(mktemp)
PROBLEMS="almabench bdd fft floats-in-functor hamming kahan-sum kb lens quicksort"

for problem in $PROBLEMS; do
  echo "./learn_linear_general_reward_without_normalisation.sh out/$problem &>tmp/$problem-learn-linear-general-reward-without-normalisation.log" >>$TASK_FILE
done

echo "Task file: $TASK_FILE" >/dev/stderr

parallel -t <$TASK_FILE
