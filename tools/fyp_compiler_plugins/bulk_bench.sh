#!/bin/bash

set -euo pipefail

export EXPERIMENT_OCAMLPARAM="_,inline-max-unroll=15"

./compile_and_benchmark_v1_lasso.sh hand   
./compile_and_benchmark_v1_lasso.sh general
./compile_and_benchmark_v1_lasso.sh star   

BOUNDS="0.00005 0.0001 0.0005 0.001 0.005 0.05 0.01"

for b in $BOUNDS; do
   ./compile_and_benchmark_v1_ridge.sh hand    $b
   ./compile_and_benchmark_v1_ridge.sh general $b
   ./compile_and_benchmark_v1_ridge.sh star    $b
done
