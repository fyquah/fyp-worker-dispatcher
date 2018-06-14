#!/bin/bash

PYTHONPATH=. python report_plots/reward_assignment/plot_reward_histogram.py ridge-general
PYTHONPATH=. python report_plots/reward_assignment/plot_reward_histogram.py ridge-hand
PYTHONPATH=. python report_plots/reward_assignment/plot_reward_histogram.py ridge-star
PYTHONPATH=. python report_plots/reward_assignment/plot_reward_histogram.py lasso-general
PYTHONPATH=. python report_plots/reward_assignment/plot_reward_histogram.py lasso-hand
PYTHONPATH=. python report_plots/reward_assignment/plot_reward_histogram.py lasso-star
