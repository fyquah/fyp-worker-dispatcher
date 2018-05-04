#!/bin/bash
cd $(dirname $0)
for exp in $(python ../scripts/query_experiment_params.py --all); do
  sed TEMPLATE.sexp -e "s/EXP/$exp/g" >./$exp.sexp
done
