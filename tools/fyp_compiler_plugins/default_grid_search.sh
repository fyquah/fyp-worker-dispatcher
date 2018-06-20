#!/bin/bash

set -euo pipefail

# NOT reporting inlining decisions is REALLY REALLY important, otherwise, compilation
# will end up consuming excessive amounts of RAM.
export OCAMLOPT_TIMEOUT="1m"

INLINE_MAX_DEPTH="5"
INLINE_MAX_UNROLLS="1 5 15"
INLINE_AGRESSIVENESS="10 25 50"

make build/plugin_nothing.cmxs

for exp in $(python ~/fyp/worker-dispatcher/scripts/query_experiment_params.py --all); do
  for depth in $INLINE_MAX_DEPTH; do
    for unroll in $INLINE_MAX_UNROLLS; do
      for agressive in $INLINE_AGRESSIVENESS; do
        cd ~/fyp/worker-dispatcher/tools/fyp_compiler_plugins/
        plugin_name=plugin_grid_search_"$depth"_"$unroll"_"$agressive"
    
        cp build/plugin_nothing.cmxs build/$plugin_name.cmxs
    
        cd ../../
    
        inline_0=$(echo "$agressive * 1.0" | bc -l)
        inline_1=$(echo "$agressive * 2.5" | bc -l)
        inline_2=$(echo "$agressive * 5.0" | bc -l)
    
        THIS_OCAMLPARAM="_,unbox-closures=1,inline-max-depth=$depth,inline-max-unroll=$unroll,inline=0=$inline_0,inline=1=$inline_1,inline=2=$inline_2"
        EXPERIMENT_OCAMLPARAM="$THIS_OCAMLPARAM" ./scripts/run-plugin-benchmarks $plugin_name $exp
      done
    done
  done
done
