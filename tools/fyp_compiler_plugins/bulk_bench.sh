#!/bin/bash

set -euo pipefail

# NOT reporting inlining decisions is REALLY REALLY important, otherwise, compilation
# will end up consuming excessive amounts of RAM.
export PATH="$HOME/fyp/worker-dispatcher/hacks:$PATH"
export OCAMLOPT_TIMEOUT="1m"

./compile_and_benchmark_v1_ridge_moe.sh
exit 0

./compile_and_benchmark_v1_lasso_moe.sh
./compile_and_benchmark_nothing.sh
./compile_and_benchmark_v1_lasso.sh hand   
./compile_and_benchmark_v1_lasso.sh general
./compile_and_benchmark_v1_lasso.sh star   

BOUNDS="0.00005 0.0001 0.0005 0.001 0.005 0.05 0.01"

for b in $BOUNDS; do
   ./compile_and_benchmark_v1_ridge.sh hand    $b
   ./compile_and_benchmark_v1_ridge.sh general $b
   ./compile_and_benchmark_v1_ridge.sh star    $b
done
