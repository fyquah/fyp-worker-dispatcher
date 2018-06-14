#!/bin/bash

set -euo pipefail

echo "VERSION = $VERSION"
echo "OUT_MODEL= $OUT_MODEL"
echo "MODEL = $MODEL"

if [ -z ${1+x} ]; then
  echo "Possible selection criterion: [star], [general] or <your-hand-chosen-model-here>";
  echo "   - lasso hand chosen: decay-1.000000-benefit-tanh_speedup_over_baseline-lasso-factor-auto"
  echo "   - ridge hand chosen: decay-0.400000-ridge-0.050000-benefit-log_speedup_over_mean"
  exit 1
else
  echo "selection choice: $1"
fi

selection=$1

cd $(dirname $0)/..


for exp in $(python ../scripts/query_experiment_params.py --all-old); do

  case "$selection" in
    star)
      experiment_dir=out-$VERSION/$exp/$MODEL/$(cat out-$VERSION/$MODEL-best-hyperparams.sexp | sexp-query  "(field $exp)")
      ;;
     
    general)
      experiment_dir=out-$VERSION/$exp/$MODEL/$(cat out-$VERSION/$MODEL-general-hyperparams.txt)
      ;;
     
    *)
      experiment_dir=out-$VERSION/$exp/$MODEL/$selection
      ;;
  esac

  mkdir -p ../w/reward-$VERSION/$OUT_MODEL/$exp
  echo ">>>>>>>>>> Experiment $exp <<<<<<<<<<"
  echo "- dumping to $(realpath ../w/reward-$VERSION/$OUT_MODEL/$exp/rewards.sexp)"
  echo "- Reading from $experiment_dir"

  python ./print_ridge.py \
    --experiment-dir  $experiment_dir \
    --problem-dir out-$VERSION/$exp/ \
    --dump-rewards \
    >../w/reward-$VERSION/$OUT_MODEL/$exp/rewards.sexp
done
