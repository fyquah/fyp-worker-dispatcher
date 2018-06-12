set -euo pipefail

BENCHMARK_OUT_DIR=$1
if [ -z ${DRY_RUN+x} ]; then
  DRY_RUN=""
fi

if [ "$DRY_RUN" = "" ]; then
  export ADDITIONAL_FLAGS=""
else
  echo "Running this script in dry run (no effect)"
  export ADDITIONAL_FLAGS="--dry-run"
fi

## Thorough parameters
# DECAY_FACTORS="0.3 0.5 0.9 0.93 0.95 0.99 0.993 0.995 0.999"

## Simple parameters
DECAY_FACTORS="1.0"  # lasso factor is determined by cross validation. It is, given almost for sure, that smaller lambda values are benefitial
BENEFIT_FUNCTIONS="linear_speedup_over_mean log_speedup_over_mean tanh_speedup_over_mean linear_speedup_over_baseline log_speedup_over_baseline tanh_speedup_over_baseline"

### (1) Hyperparameter sweep
TASK_FILE=$(mktemp)
echo "Writing tasks to $TASK_FILE"

cat $TASK_FILE | while read args; do
  echo "python learn_lasso_with_alpha.py $args"
done | parallel -j6 --verbose
