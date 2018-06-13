#!/bin/bash

cd $(dirname $0)
cd ..
for exp in $(python ../scripts/query_experiment_params.py --all-old); do
  echo "find /media/usb/home/fyquah/fyp/prod/processed-data/$exp/ -name 'queries-v0.bin' | ../_build/default/tools/data_cleaner.exe concat-queries -output ../w/$exp/queries-v0.bin -filelist /dev/stdin -tag $exp"
done | parallel --jobs 6 --verbose
