#!/bin/bash

cd $(dirname $0)
cd ..
for exp in $(python ../scripts/query_experiment_params.py --all); do
  echo "find /media/usb/home/fyquah/fyp/prod/processed-data/$exp/ -name 'features-v0.sexp' | ../_build/default/tools/data_cleaner.exe concat-features -output ../w/$exp/features.bin"
done | parallel --jobs 6 --verbose