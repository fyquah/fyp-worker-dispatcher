#!/bin/bash

PROBLEM_BASE_DIR="$1"
TASK_FILE=$(mktemp)

### (2) print optimal decisions for every hyperparams configuration
echo -n "">$TASK_FILE
EXP_BASE_DIR=$PROBLEM_BASE_DIR/lasso-with-alpha

for subdir in $(ls $EXP_BASE_DIR/); do
  if [ -f "$EXP_BASE_DIR/$subdir/optimal-expanded.sexp" ]; then
    echo "$EXP_BASE_DIR/$subdir/optimal-expanded.sexp exists -- skipping"
  else
    echo "python2 print_lasso_with_alpha.py --skip-normalisation --experiment-dir $EXP_BASE_DIR/$subdir/ --problem-dir $PROBLEM_BASE_DIR --optimal-decision --output $EXP_BASE_DIR/$subdir/optimal-expanded.sexp" >>$TASK_FILE
  fi
done

parallel -j6 --verbose <$TASK_FILE

### (3) Changing the optimal expanded tree into inlining decisions.
###       It is okay to use GNU parallel for this, as there is no state-sharing
###       across different executions.
for subdir in $(ls $EXP_BASE_DIR/); do
  echo "../_build/default/tools/tree_tools.exe v1 expanded-to-decisions $EXP_BASE_DIR/$subdir/optimal-expanded.sexp -round 0 -output $EXP_BASE_DIR/$subdir/optimal.sexp"
done | parallel -j6 --verbose
