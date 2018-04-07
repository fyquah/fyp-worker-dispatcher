#!/bin/bash

for exp in $(python ../scripts/query_experiment_params.py --all); do
  echo "time python extract_node_rewards_from_processed_data.py --output-dir out/$exp --experiment-name $exp &>tmp/extract-$exp.log"
done | parallel $PARALLEL_FLAGS
