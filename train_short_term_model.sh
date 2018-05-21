#!/bin/bash


set -euo pipefail

id=$1

mkdir -p tmp/training-logs-$id
mkdir -p tanalysis-specs

for exp in $(python ./scripts/query_experiment_params.py --all); do
  ./analysis-specs/gen_out_of_sample.sh $exp >analysis-specs/ALL_but_$exp.sexp
done

for exp in $(python ./scripts/query_experiment_params.py --all); do
  echo "./_build/default/tools/local_reward_model.exe short-term-model -epochs 3000 -spec ./analysis-specs/ALL_but_$exp.sexp &>tmp/training-logs-$id/ALL_but_$exp.log -hyperparams tmp/hyperparams-$id.sexp"
done | parallel --jobs 3 --verbose
