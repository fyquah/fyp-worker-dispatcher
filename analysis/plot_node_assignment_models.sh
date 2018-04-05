mkdir -p plots/reward-assignment-model

python plot_model_general_performance.py \
  --model linear-general-reward-without-normalisation \
  --decay-factor 0.300000 --ridge-factor 0.003000 \
  --benefit-function log_speedup_over_mean \
  --title "Selected Models' Performance on Benchmarks (log speedup)" \
  --pdf plots/reward-assignment-model/best_hyperparams_per_experiment_choose_log_speedup.pdf

python plot_model_general_performance.py \
  --model linear-general-reward-without-normalisation \
  --decay-factor 0.300000 --ridge-factor 0.003000 \
  --benefit-function linear_speedup_over_mean \
  --title "Selected Models' Performance on Benchmarks (linear speedup)" \
  --pdf plots/reward-assignment-model/best_hyperparams_per_experiment_choose_linear_speedup.pdf

python plot_model_general_performance.py \
  --model linear-general-reward-without-normalisation \
  --decay-factor 0.300000 --ridge-factor 0.003000 \
  --benefit-function sigmoid_speedup_over_mean \
  --title "Selected Models' Performance on Benchmarks (sigmoid speedup)" \
  --pdf plots/reward-assignment-model/best_hyperparams_per_experiment_choose_sigmoid_speedup.pdf

python plot_model_general_performance.py \
  --model linear-general-reward-without-normalisation \
  --title "Projected Models' Performance on Benchmarks" \
  --pdf plots/reward-assignment-model/best_hyperparams_per_experiment.pdf
