set -euo pipefail

BENCHMARK_OUT_DIR=$1
if [ -z ${DRY_RUN+x} ]; then
  DRY_RUN=""
fi

if [ "$DRY_RUN" = "" ]; then
  ADDITIONAL_FLAGS=""
else
  echo "Running this script in dry run (no effect)"
  ADDITIONAL_FLAGS="--dry-run"
fi

## Thorough parameters
# DECAY_FACTORS="0.3 0.5 0.9 0.93 0.95 0.99 0.993 0.995 0.999"
# RIDGE_FACTORS="0.003 0.005 0.009 0.03 0.05 0.09 0.3 0.5 0.9"

## Simple parameters
DECAY_FACTORS="0.3 0.7 0.93 0.97 0.993 0.997"
RIDGE_FACTORS="0.003 0.007 0.03 0.07 0.3 0.7"
BENEFIT_FUNCTIONS="sigmoid_speedup_over_mean linear_speedup_over_mean log_speedup_over_mean"

### (1) Hyperparameter sweep
TASK_FILE=$(mktemp)
echo "Writing tasks to $TASK_FILE"

# The loop must be ordered as follows so that the caching actually takes
# effect. As the problem is a function of decay factor alone, it makes sense
# to iterate through all decay_factors initially.
for ridge_factor in $RIDGE_FACTORS; do
  for benefit_function in $BENEFIT_FUNCTIONS; do
    for decay_factor in $DECAY_FACTORS; do
        echo "$BENCHMARK_OUT_DIR --decay-factor $decay_factor --ridge-factor $ridge_factor  --benefit-function $benefit_function" >>$TASK_FILE
    done
  done
done
python2 batch_execute.py learn_linear_general_reward $ADDITIONAL_FLAGS --num-threads 7 <$TASK_FILE


### (2) print optimal decisions for every hyperparams configuration
echo -n "">$TASK_FILE
EXP_BASE_DIR=./out/kb/linear-general-reward

for subdir in $(ls out/kb/linear-general-reward/); do
    echo "--experiment-dir $EXP_BASE_DIR/$subdir/ --problem-dir ./out/kb/ --optimal-decision --output $EXP_BASE_DIR/$subdir/kb-optimal-expanded.sexp" >>$TASK_FILE
done
python2 batch_execute.py print_linear_general_reward $ADDITIONAL_FLAGS --num-threads 7 <$TASK_FILE

  
### (3) Changing the optimal expanded tree into inlining decisions.
###       It is okay to use GNU parallel for this, as there is no state-sharing
###       across different executions.
for subdir in $(ls out/kb/linear-general-reward/); do
  echo "../_build/default/tools/tree_tools.exe v1 expanded-to-decisions \
    $EXP_BASE_DIR/$subdir/kb-optimal-expanded.sexp \
    -output $EXP_BASE_DIR/$subdir/kb-optimal.sexp"
done | parallel $ADDITIONAL_FLAGS
