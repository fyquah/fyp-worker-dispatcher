All the python programs (excluding some very very simply scripts, which can
live in ROOT/scripts/) used in the project lives in this directory. The
main "theoretical" components that lives in this part of the world are:

- call site reward assignment
- inlining policy machine learning
- plotting scripts

There is a lot of shell scripts and arbitrary scripts. The scripts were
not structured in organised directories.

## Directory Structure from Data Generation

As the project was carried out on specific hard disks, the rundirs is
expected to live in one of (which one exactly doesn't matter):

- `/media/usb/home/fyquah/fyp/prod/rundir/`
- `/media/usb2/home/fyquah/fyp/prod/rundir/`
- `/media/usb3/prod/rundir/`

### Call Site Reward Assignment

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
VERSION="out-v0-reproduce-relabel" ./learn_all_lasso.sh
VERSION="out-v0-reproduce-relabel" ./learn_all_ridge.sh

# Print the "optimal" inlining decisions
VERSION="out-v0-reproduce-relabel" ./print_all_lasso.sh
VERSION="out-v0-reproduce-relabel" ./print_all_ridge.sh
```

### Data Extraction

The first stage of the pipeline for reward assignment is to extract all
the inlining trees from every experiment. To extract all data, run:

```bash
# Run this only if you have /media/usb mounted, and want to go from raw data.
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

One of the early mistakes made in the project was not to have a stable
labelling algorithm with a proof. Some of the data used in the project were
not stably proven. `scripts/clean-with-path-patching` converts from the
unproven labels to the proven in the inlining decision files, and is
automatically invoked during feature extraction. The resultant "cleaned"
and "fixed" inlining decisions are stored in
`/media/usb/home/fyquah/processed-data/`


### Assign Numerical Rewards to Nodes in Expanded Tree

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

### Print Predictive Modelling Inlining Decisions

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

### Benchmarking Inlining Decisions

To benchmark the predicting modeling decisions, and assuming you have made
the necessary setups, run:

```bash
cd ../  # Go back to root directory of this repo.
./scripts/run-all-benchmarks ridge-v0-reproduce-relabel
./scripts/run-all-benchmarks lasso-v0-reproduce-relabel
```

This will take awhile ....

To visualise the results, see [a later section](#Printing-Benchmark-Results)

### Selecting `h_general` and `H_star`

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

### Misc.

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

## Learning an Inlining Policy

This is the final stage of the optimisation pipeline, that is to turn
rewards into a model that decides when to inline a function.

The following bash snippets assume that you are in the `analysis` directory,
similar to the previous section.

tl; dr - To reproduce everything from raw data and reward assignments for
this section, run the following code snippet:

```bash
# Run the following two only if you want to go from raw data. They can take
# a long long time (up to 12 hours on a reasonably powerful 8-core machine)
./scripts/extract-features-from-all-experiments  # assumes /media/usb/ is mounted.
./scripts/dump_features_from_all_experiments.sh  # assumes /media/usb/ is mounted.

./scripts/gen-merged-features-and-rewards
cd ../tools/fyp_compiler_plugins/

# Benchmarks all the models / inlining policies that were discussed in the
# report.
./bulk_bench.sh
```

### Extracting Inlining Query

The following two scripts take the "cleaned" inlining decisions (described
above) and run them with the compilation flags that generates the set of
inlining decisions that were taken in the first round of Flambda inlining.
To do that, run:

```bash
# Run the following two only if you want to go from raw data. They can take
# a long long time (up to 12 hours on a reasonably powerful 8-core machine)
./scripts/extract-features-from-all-experiments  # assumes /media/usb/ is mounted.
./scripts/dump_features_from_all_experiments.sh  # assumes /media/usb/ is mounted.
```

The first script and stores them in `/media/usb/home/fyquah/processed-data/`
in a file called `queries-v0.bin`.

The second script takes all the queries extracted, concatenates them and
stores them in `../w/<exp-name>/queries-v0.bin`


### Generating Feature-Reward Pairs

There were several different kinds of reward assignment schemes, namely
`H_star`, `h_general` and `h_hand` from both ridge and lasso regression.
The following script generate the combined feature-reward pairs.

```bash
./scripts/gen-merged-features-and-rewards
```

The script invokes `scripts/dump_rewards_from_all_experiments.sh` and
`scripts/merge_feature_and_rewards_from_all_experiments`. The former dumps
the learnt reward vector into
`../w/reward-v0-reproduce-relabel/<lasso | ridge>/<exp-name>/rewards.sexp`.
The latter runs the feature-reward merge procedure (by mathcing labels) and
stores the results in
`report_plots/reward_assignment/data/$REWARD_MODEL/$exp/feature_reward_pair_v3.sexp`.

After invoking the said two scripts, the script invokes `feature_loader.py`,
which concats all the features for a given reward-assignment model together.

_Note: there has been 4 iterations of feature selection. The version used
here (and the report) is V3. The parameter can be configured by modifying
`scripts/gen-merged-features-and-rewards`_

### Learning and Benchmarking

As the dataset is not extremely big (only around 4.3k data points), training
is pretty fast (runs under 20 seconds). For that reason, there was no
infrastructure setup to execute machine learning.

To benchmark all models disucssed, run the following (from `analysis`
subdirectory).

```bash
cd ../tools/fyp_compiler_plugins/
./bulk_bench.sh
```

`bulk_bench.sh` benchmarks the lasso regression models (3)
and the regression models whilist logarithimically varying the
uncertainty threshold / bound (3 * 8) and lasso CMoE and ridge CMoE. In
addition to this, the script also benchmarks a "nothing" plugin, that
compiles the program without overidding inlining decisions.

## Printing Benchmark Results


