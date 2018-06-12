set -euo pipefail

cd ../../analysis
python2 train_lasso_single_neural_network.py \
  --decision-model-file ../tools/fyp_compiler_plugins/build/decision_model_v1_neural_network.ml
