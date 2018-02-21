#!/bin/bash

DIRECTORIES=$(cat tmp/batch_executor.log.2 | grep $1 | cut -f2 -d',')

for d in $DIRECTORIES; do
  jbuilder exec -- controller simulated-annealing plot v1-execution-stats $d
done
