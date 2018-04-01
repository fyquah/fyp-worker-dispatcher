#!/bin/bash

PROBLEM_BASE_DIR="$1"
TASK_FILE=$(mktemp)

### (2) print optimal decisions for every hyperparams configuration
# echo -n "">$TASK_FILE
EXP_BASE_DIR=$PROBLEM_BASE_DIR/linear-general-reward
# 
for subdir in $(ls $EXP_BASE_DIR/); do
    echo "--experiment-dir $EXP_BASE_DIR/$subdir/ --problem-dir $PROBLEM_BASE_DIR --optimal-decision --output $EXP_BASE_DIR/$subdir/optimal-expanded.sexp" >>$TASK_FILE
done
python2 batch_execute.py print_linear_general_reward $ADDITIONAL_FLAGS <$TASK_FILE

### (3) Changing the optimal expanded tree into inlining decisions.
###       It is okay to use GNU parallel for this, as there is no state-sharing
###       across different executions.
for subdir in $(ls $EXP_BASE_DIR/); do
  echo "../_build/default/tools/tree_tools.exe v1 expanded-to-decisions \
    $EXP_BASE_DIR/$subdir/optimal-expanded.sexp \
    -output $EXP_BASE_DIR/$subdir/optimal.sexp"
done
