set -euo pipefail

cd ../../analysis
python2 train_ridge_single_neural_network.py \
  --model-choice $1 \
  --decision-model-file ../tools/fyp_compiler_plugins/build/decision_model_v1_neural_network.ml \
  --significance-diff $2
