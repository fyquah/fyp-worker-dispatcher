#!/bin/bash
cd $(dirname $0)
for exp in $(python ../scripts/query_experiment_params.py --all); do
  sed TEMPLATE.sexp -e "s/EXP/$exp/g" >./$exp.sexp
done

echo "(" >ALL.sexp
for exp in $(python ../scripts/query_experiment_params.py --all); do
  echo " ((name $exp)" >>ALL.sexp
  echo "  (features_file w/$exp/features.bin)" >>ALL.sexp
  echo "  (rewards_file w/$exp/rewards.sexp))" >>ALL.sexp
done
echo ")" >>ALL.sexp
