#!/bin/bash

for log_file in $(find ./important-logs/training -name "*.log"); do
  pdf_file=$(python2 -c "import os; print os.path.splitext('$log_file')[0]").pdf
  if [ -f "$pdf_file" ]; then
    echo "$pdf_file exists. Ignoring." >/dev/stderr
  else
    echo "python2 analysis/plot_training_log.py --pdf $pdf_file $log_file"
  fi
done | parallel --jobs 4 --verbose
