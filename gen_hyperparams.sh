#!/bin/bash

set -euo pipefail

i=$1

for l2_reg in 0.001 0.01 0.1; do
  for dropout_keep_prob in 0.5 1.0; do
    outfile="tmp/hyperparams-$i.sexp"
    echo "">$outfile
    echo "((l2_reg $l2_reg)" >>$outfile
    echo " (dropout_keep_prob $dropout_keep_prob))" >>$outfile
    let i=i+1
    echo "Generated $outfile"
  done
done
