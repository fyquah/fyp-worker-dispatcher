#!/bin/bash

set -euo pipefail

# learns everything in all non ill-defined experiments.
TASK_FILE=$(mktemp)
PROBLEMS="almabench bdd fft floats-in-functor hamming kahan-sum kb lens lexifi quicksort sequence sequence-cps"

for problem in $PROBLEMS; do
  ./learn_lasso_with_alpha.sh out-$VERSION/$problem &>tmp/$VERSION-$problem-learn-all-lasso-with-alpha.log
done
