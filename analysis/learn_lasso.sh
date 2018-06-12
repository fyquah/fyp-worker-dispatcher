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
DECAY_FACTORS="0.4 0.9 0.99 1.0"
BENEFIT_FUNCTIONS="linear_speedup_over_mean log_speedup_over_mean tanh_speedup_over_mean linear_speedup_over_baseline log_speedup_over_baseline tanh_speedup_over_baseline"
# BENEFIT_FUNCTIONS="tanh_speedup_over_baseline"

### (1) Hyperparameter sweep
TASK_FILE=$(mktemp)
echo "Writing tasks to $TASK_FILE"

# The loop must be ordered as follows so that the LRU cache actually takes
# effect.
for decay_factor in $DECAY_FACTORS; do
  for benefit_function in $BENEFIT_FUNCTIONS; do
    echo "--skip-normalisation $BENCHMARK_OUT_DIR --decay-factor $decay_factor --benefit-function $benefit_function" >>$TASK_FILE
  done
done

# for benefit_function in $BENEFIT_FUNCTIONS; do
#   echo "--skip-normalisation $BENCHMARK_OUT_DIR --decay-factor 1.0 --benefit-function $benefit_function" >>$TASK_FILE
# done
# 
# echo "--skip-normalisation $BENCHMARK_OUT_DIR --decay-factor 0.3 --benefit-function sigmoid_speedup_over_mean" >>$TASK_FILE
# echo "--skip-normalisation $BENCHMARK_OUT_DIR --decay-factor 0.3 --benefit-function linear_speedup_over_mean" >>$TASK_FILE
# echo "--skip-normalisation $BENCHMARK_OUT_DIR --decay-factor 0.3 --benefit-function tanh_speedup_over_baseline" >>$TASK_FILE

cat $TASK_FILE | while read args; do
  python learn_lasso.py $args
done
