DECAY_FACTORS="0.400000 0.900000 1.000000"  # lasso factor is determined by cross validation. It is, given almost for sure, that smaller lambda values are benefitial
BENEFIT_FUNCTIONS="linear_speedup_over_mean log_speedup_over_mean tanh_speedup_over_mean linear_speedup_over_baseline log_speedup_over_baseline tanh_speedup_over_baseline"

for decay_factor in $DECAY_FACTORS; do
  for benefit_function in $BENEFIT_FUNCTIONS; do
    PYTHONPATH=. python report_plots/reward_assignment/plot_lasso_lambda.py lexifi $decay_factor $benefit_function
  done 
done

PYTHONPATH=. python report_plots/reward_assignment/plot_lasso_lambda.py bdd 1.000000 tanh_speedup_over_mean
