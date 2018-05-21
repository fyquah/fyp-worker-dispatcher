#!/bin/bash


id=$1
set -euo pipefail

mkdir -p important-logs/decision-model/$id
mkdir -p analysis-specs

for exp in $(python ./scripts/query_experiment_params.py --all); do
  ./analysis-specs/gen_out_of_sample.sh $exp >analysis-specs/ALL_but_$exp.sexp
done

for exp in $(python ./scripts/query_experiment_params.py --all); do
  echo "./_build/default/tools/local_reward_model.exe decision-model -epochs 1000 -spec ./analysis-specs/ALL_but_$exp.sexp &>./important-logs/decision-model/$id/ALL_but_$exp.log -hyperparams ./tmp/hyperparams-$id.sexp"
done | parallel --jobs 3 --verbose
