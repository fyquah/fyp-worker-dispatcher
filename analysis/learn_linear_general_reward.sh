set -euo pipefail

BENCHMARK_OUT_DIR=$1

## Thorough parameters
DECAY_FACTORS="0.3 0.5 0.9 0.93 0.95 0.99 0.993 0.995 0.999"
RIDGE_FACTORS="0.003 0.005 0.009 0.03 0.05 0.09 0.3 0.5 0.9"

## Simple parameters
# DECAY_FACTORS="0.5 0.9 0.95 0.99 0.999"
# RIDGE_FACTORS="0.03 0.07 0.3 0.7"

# Hyperparameter sweep
for decay_factor in $DECAY_FACTORS; do
  for ridge_factor in $RIDGE_FACTORS; do
    python learn_linear_general_reward.py  \
      --decay-factor $decay_factor \
      --ridge-factor $ridge_factor \
      "$BENCHMARK_OUT_DIR"
  done
done

EXP_BASE_DIR=./out/kb/linear-general-reward

# print optimal decisions for every hyperparams configuration
for subdir in $(ls out/kb/linear-general-reward/); do

  python print_linear_general_reward.py  \
    --experiment-dir $EXP_BASE_DIR/$subdir/ \
    --problem-dir ./out/kb/ \
    --optimal-decision \
    >$EXP_BASE_DIR/$subdir/kb-optimal-expanded.sexp
  
  ../_build/default/tools/tree_tools.exe v1 expanded-to-decisions \
    $EXP_BASE_DIR/$subdir/kb-optimal-expanded.sexp \
    -output $EXP_BASE_DIR/$subdir/kb-optimal.sexp
done
