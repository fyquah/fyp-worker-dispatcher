mkdir experiment-logs

set -euo pipefail

PLUGIN_NAME="$1"

echo "FEATURE VERSION = $FEATURE_VERSION"
echo "PLUGIN_NAME = $PLUGIN_NAME"

for B in 0.0 0.0005 0.001 0.002 0.003 0.004 0.005 0.008 0.009; do

  echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
  echo "         B = $B"
  echo ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"

  cd ~/fyp/worker-dispatcher/
  LOGFILE=~/fyp/worker-dispatcher/experiment-logs/$B.txt

  echo "Simple ANN" >$LOGFILE
  echo "$FEATURE_VERSION Features" >>$LOGFILE
  echo "V0-reproduce Rewards" >>$LOGFILE

  cd tools/fyp_compiler_plugins/
  ./train_$PLUGIN_NAME.sh $B --feature-version $FEATURE_VERSION >>$LOGFILE
  make build/$PLUGIN_NAME.cmxs

  cd ~/fyp/worker-dispatcher/
  rm results/*/plugins/$PLUGIN_NAME.csv || echo "hello"
  ./scripts/run-plugin-benchmarks $PLUGIN_NAME

  cd analysis/
  PYTHONPATH=. python report_plots/machine_learning/print_exec_stats_table.py \
    $(echo -n "$PLUGIN_NAME" | sed -e 's/plugin_//g') >>$LOGFILE
done
