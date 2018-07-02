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

tl; dr : To reproduce this stage of the project's results, run the following:

```bash
cd path/to/fyp/analysis

# Note: Every script uses all core in the machine, so DO NOT parallelise
#       these 3 scripts.

# Data extraction
VERSION="out-v0-reproduce-relabel" ./extract_all.sh

# Fitting by linear regression. These scripts performs automatic
#   hyperparameter search.
VERSION="out-v0-reproduce-relabel" /learn_all_lasso.sh
VERSION="out-v0-reproduce-relabel" /learn_all_ridge.sh

# 
VERSION="out-v0-reproduce-relabel" /print_all_lasso.sh
VERSION="out-v0-reproduce-relabel" /print_all_ridge.sh
```

## Data Extraction

The first stage of the pipeline for reward assignment is to extract all
the inlining trees from every experiment. To extract all data, run:

```bash
VERSION="out-v0-reproduce-relabel" ./extract_all.sh
```

The script simply runs extraction in parallel, invoking the following files:

- `extract_data_from_experiments.py` - extracts expanded tree into a set of
  adjacency lists, and the execution time of programs.
- `extract_all.sh` and `extract_all_commands.txt` - parallelise data
  extraction automatically throghout data benchmarks.

Running each extraction script using `extract_data_from_experiment.py` creates
a directory called out-$VERSION-reproduce-relabel/<experiment-name>,
which contains the adjacency lists of inlining trees and execution times
(stored in pickle format).

## Assign Numerical Rewards to Nodes in Expanded Tree

There is no real notion for selecting gamma (deacy factor), choice of
preprocessing function and lambda (regularisation factor, in the case of
ridge regression). There are shell scripts that automatically select
"sensible" values to perform an exhaustive search on.

To assign numerical rewards using both schemes, run the following script:

```bash
VERSION="out-v0-reproduce-relabel" ./learn_all_lasso.sh
VERSION="out-v0-reproduce-relabel" ./learn_all_ridge.sh
```

- `learn_lasso.py` and `learn_ridge.py` construct the problem as a linear
  MSE problem (with matrices) and write the learnt reward values into
  relevant output directories.
- `learn_all_lasso.sh` and `learn_all_ridge.sh`  Runs the python scripts
  with the appropriate command line arguments to specify the directory
  that contains problem definitions.

The results from reward assignment are then written to
`out-$VERSION/exp-name/<lasso | ridge>/hyperparameter-name/rewards.npy`.

## Print Predictive Modelling Inlining Decisions

```bash
VERSION="out-v0-reproduce-relabel" ./print_all_lasso.sh
VERSION="out-v0-reproduce-relabel" ./print_all_ridge.sh
```

These shell script invokes `print_lasso.sh` and `print_ridge.sh` on all
experiments. The 2 said shell scripts in turn invokes `print_lasso.py` and
`print_ridge.py` and an `tree-tools v1 expanded-to-decisions` which
converts the optimal expanded tree and a decision set.

The results are written to
`out-$VERSION/exp-name/<lasso | ridge>/hyperparameter-name/optimal-expanded.sexp`
and `out-$VERSION/exp-name/<lasso | ridge>/hyperparameter-name/optimal.sexp`,
containing the "optimal" expanded tree and complete decision set, as defined
by the optimisation procedure.

## Benchmarking Inlining Decisions

To benchmark the predicting modeling decisions, and assuming you have made
the necessary setups, run:

```bash
cd ../  # Go back to root directory of this repo.
./scripts/run-all-benchmarks ridge-v0-reproduce-relabel
./scripts/run-all-benchmarks lasso-v0-reproduce-relabel
```

This will take awhile ....

## Selecting `h_general` and `H_star`

```bash
python select_best_model_hyperparams.py --model ridge-v0-reproduce-relabel/ \
         --prefix ridge --output-dir out-v0-reproduce-relabel/
python select_best_model_hyperparams.py --model lasso-v0-reproduce-relabel/ \
         --prefix lasso --output-dir out-v0-reproduce-relabel/
```

You can ignore the latex output printed to stdout, they were used for
writing of this thesis. The selected hyperparameters are written to
`out-v0-reproduce-relabel/*-best-hyperparams.sexp` and
`out-v0-reproduce-relabel/*-general-hyperparams.txt` respectively. They
are used for deriving labels used in the inlining policy.

## Misc.

The report mentioned the study of the effects of the regularisation factor
in lasso regression, ceteris peribus. The scripts used to study that are

```
learn_lasso_with_alpha.py
learn_lasso_with_alpha.sh
learn_all_lasso_with_alpha.sh  # probably not very useful, you don't want to
                               # study for all experiments.
print_lasso_with_alpha.sh
```

They can be used similarly to the instructions above.

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
