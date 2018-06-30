All the python programs (excluding some very very simply scripts, which can
live in ROOT/scripts/) used in the project lives in this directory. The
main "theoretical" components that lives in this part of the world are:

- call site reward assignment
- inlining policy machine learning
- plotting scripts

There is a lot of shell scripts and arbitrary scripts. The scripts were
not structured in nice directories (tl; dr - research quality).

# Directory Structure from Data Generation

As the project was carried out on specific hard disks, the rundirs is
expected to live in one of (which one exactly doesn't matter):

- `/media/usb/home/fyquah/fyp/prod/rundir/`
- `/media/usb2/home/fyquah/fyp/prod/rundir/`
- `/media/usb3/prod/rundir/`

# Call Site Reward Assignment

The main data structure used here is the `expanded-tree`, as described in
the report.

The primary data structures are provided by

- `inlining_tree.py` - Albeit it's name, it actually provides the expanded
   tree as described in the report. The name is as such because I found out
   about expanded trees after writing this part.

To reproduce this stage of the project's results:

```bash
# Note: Every script uses all core in the machine, so DO NOT parallelise
#       these 3 scripts.

# Data extraction
./extract_all.sh

# Fitting by linear regression. These scripts performs automatic
#   hyperparameter search.
./learn_all_lasso.sh
./learn_all_ridge.sh

# 
./print_all_lasso.sh
./print_all_ridge.sh
```

## Data Extraction

The first stage of the pipeline for reward assignment is to extract all
the inlining trees from every experiment.

- `extract_data_from_experiments.py` - extracts expanded tree into a set of
  adjacency lists, and the execution time of programs.
- `extract_all.sh` and `extract_all_commands.txt` - parallelise data
  extraction automatically throghout data benchmarks.


## Dumping Rewards from Node Reward Assignment Model


```bash
exp="almabench"
python ./print_linear_general_reward.py \
  --experiment-dir out/$exp/linear-general-reward-without-normalisation/decay-0.300000-ridge-0.003000-benefit-sigmoid_speedup_over_mean/ \
  --problem-dir out/$exp/ --dump-rewards >$exp-rewards.sexp
```

# Learning an Inlining Policy

## Feature Extraction




## Directory Structure

Files used to collect data from our large messy set of execution statistics:

- `extract_data_from_experiments.py` - Extract data from the archive files
  that I have collected in the 3 machines sitting in 505. The data is
  assumed to reside in /media/usb or /media/usb2 or /media/usb<n> or ...


In general, most experiments have two files:

- `learn_<exp_name>.py` - optimises things
- `print_<exp_name>.py` - print what the model has figured out, inlining
  decisions etc.

The modelling results will be storred in `modelling-results` with the
following data stucture:

```bash
out/
  almabench/
    properties.pkl
    edges_lists.pkl
    execution_times.npz
    node_labels.npz
    linear_general_reward/
      somewhat-meaningful-id/
        hyperparams.pkl
```


## Modelling Algorithms & Experiments

- `linear_specialised_reward` - the model I first talk to DT about.
  Contributions are added lienarly throughout the paths in the tree.
- `linear_general_reward` - similar to above, except using a shared
  contribution value for every experiment rather than using something that
  is specialised for every experimental run.
