set -euo pipefail

cd ../../analysis
python2 train_clustered_neural_network.py $1 \
  --decision-model-file ../tools/fyp_compiler_plugins/build/decision_model_v0_clustered_neural_network.ml \
  --familiarity-model-file ../tools/fyp_compiler_plugins/build/familiarity_model_v0_clustered_neural_network.ml
