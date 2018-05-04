#!/bin/bash

set -euo pipefail

TESTING_EXPERIMENT=$1

echo_exp(){
  local exp=$1
  echo "  ((name $exp)"
  echo "   (features_file w/$exp/features.bin)"
  echo "   (rewards_file w/$exp/rewards.sexp))"
}

cd $(dirname $0)
echo "(Segmented ("
echo " (training ("
for exp in $(python ../scripts/query_experiment_params.py --all); do
  if [ "$exp" != "$TESTING_EXPERIMENT" ]; then
    echo_exp $exp
  fi
done
echo " ))"
echo " (test ("
echo_exp $TESTING_EXPERIMENT
echo " ))"
echo "))"
