#!/bin/bash


set -euo pipefail

id=$1


mkdir -p ./important-logs/training/recognition-model/$id

for exp in $(python ./scripts/query_experiment_params.py --all); do
  ./analysis-specs/gen_out_of_sample.sh $exp >analysis-specs/ALL_but_$exp.sexp
done

for exp in $(python ./scripts/query_experiment_params.py --all); do
  echo "./_build/default/tools/local_reward_model.exe familiarity-model -feature-version V1 -epochs 1000 -spec ./analysis-specs/ALL_but_$exp.sexp &>./important-logs/training/recognition-model/$id/ALL_but_$exp.log -hyperparams tmp/hyperparams-$id.sexp"
done | parallel --jobs 3 --verbose
