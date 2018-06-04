cd ../../analysis
python2 train_logistic_regression.py 0.02 \
  --decision-model-file ../tools/fyp_compiler_plugin/build/decision_model_v0_logistic_regression.ml
  --familiarity-model-file ../tools/fyp_compiler_plugin/build/familiarity_model_v0_logistic_regression.ml
