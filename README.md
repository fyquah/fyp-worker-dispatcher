# FYP: Inlining ML with ML

*Why is this repo called worker-dispatcher? We will never know, but FWIW,
you can treat this repo as the "main" repo for the project.*

Presentation slides: [http://www.fyquah.me/fyp/presentation.pdf](http://www.fyquah.me/fyp/presentation.pdf)

Pipeline summary: [http://www.fyquah.me/fyp/summary.pdf](http://www.fyquah.me/fyp/summary.pdf)


This repository contains most of the code I have used when working on my
final year project. Some other relevant repositories are:

- [OCaml fork](https://github.com/fyquah95/fyp-ocaml) patched with the necessary support
- [Benchmark repository](https://github.com/fyquah95/fyp-experiments)
- [An opam repo](https://github.com/fyquah95/fyp-opam-repo-dev) containing the opam repo
- [This repository](https://github.com/fyquah95/fyp-worker-dispatcher) which contains most of the engineering and analysis work.

To reproduce the results in the thesis (from scratch), you need to:

1. Configure the master machine
2. Configure the benchmark environments
3. Run data generation
4. Run reward assignment (See the [analysis section](analysis/README.md))
5. Train and benchmark inlining policies
   (See the [analysis section](analysis/README.md))

Most of the "interesting" analysis goes on in (4) and (5), which are
discussed in [analysis/README.md](analysis/README.md). Hence, if you are
only interested in that section of the project, it is much easier (and
takes much less time)

1. Use [this Vagrantfile]() to configure the development environment.
2. Run reward assignment (See the [analysis section](analysis/README.md))
3. Train and benchmark inlining policies (See the [analysis section](analysis/README.md))

To make it easier to reproduce the results due to (4) and (5) (which is more
interesting, anyway), simply skip to
(analysis/README.md) and follow the instructions there.

## Project Structure

```bash
$HOME/fyp/
  experiments/
  ocaml/
  worker-dispatcher/  # THIS REPO
    analysis-specs/
    analysis/
    codegen/
    controller/  # Source code that runs data generation with simulated annealing / random walk lives here
    experiment-scripts/  # scripts to aid running data generation. These scripts
                         # are meant to be invoked by other scripts, not humans
    fyp_compiler_lib/  # source code for compiler libraries
    hacks/  # scripts used in benchmarking. This is a hacky solution to override
            # ocamlopt.opt with something custom-made
    important-logs/
    metadata-filelist/  # Stores interesting list of files.
    optimization/  # Source code for simulated annealing over asynchronous
                   # processes lives here
    pca-data/  # N
    prod-configs/  # Config files for executing long-running processes
                   # (such as simulated annealing-based data generation)
    protocol/  # Source code for several data structures and configuration
               #  s-expressions live here
    tools/   # Code for tooling lives here
      fyp_compiler_plugins/  # Code for inlining policies live here
    results/  # Execution time from running training and test benchmarks
    scripts/  # All arbitrary scripts live here.
    test/     # test for various parts of the code.

    tf_config.pb  # Tensorflow config that makes sure that TF doesn't use
                  # up the entire GPU.
    dev_config.sexp  # Config files for executing long-running dev processes
    training_tasks.txt  # List of task for running data-generation on training
                        # benchmarks
    Makefile
    worker_dispatcher.opam
```


## 1. Configuring the Master Machine

The master machine in this work used a debian jessie machine

Install apt packages that we definitely require:

```bash
sudo apt-get install zip build-essential git tmux curl
curl -sSL https://gist.githubusercontent.com/fyquah95/2621159524ec7341e0be/raw/51980b660d1b59f19b4a6c582f21a32c159a3c2f/.tmux.conf | sed -e 's/zsh/bash/g' >~/.tmux.conf

# These are required by owl
sudo apt-get install libgsl0-dev liblapacke-dev libopenblas-dev pkg-config libplplot-dev libshp-dev m4
```

Clone all the relevant repos:

```bash
ssh-keygen

##### Then, Add ssh key to github #####

mkdir -p ~/fyp
git clone git@github.com:fyquah95/fyp-worker-dispatcher.git worker-dispatcher
git clone git@github.com:fyquah95/fyp-ocaml.git ocaml
git clone git@github.com:fyquah95/fyp-experiments.git experiments
git clone git@github.com:fyquah95/fyp-opam-repo-dev.git opam-repo-dev
git clone git@github.com:fyquah95/tensorflow-ocaml.git tensorflow-ocaml
```

Install opam

```bash
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
```

Setup the FYP compilation environment

```bash
cd ~/fyp/worker-dispatcher
./scripts/configure-fyp-env
```

Install the packages we need to compile worker dispatcher and friends:

```bash
opam install core=v0.9.1  # Version here is important, our code doesn't compile with
                          # the newer core versions
opam install jbuilder async async_shell async_unix menhir owl
```

"installing" ocamlnow

```bash
echo 'export PATH="$PATH:$HOME/bin/"' >> ~/.bashrc
mkdir -p ~/bin/
cp _build/default/tools/now.exe ~/bin/ocamlnow
source ~/.bashrc
```

Finally, add your ssh keys to all the workers. In our case, our workers

```bash
ssh-keygen
SSH_PUB_key=$(cat ~/.ssh/id_rsa.pub)
ssh fyquah@worker0 "echo '$SSH_PUB_key=' >>~/.ssh/authorized_keys"
ssh fyquah@worker1 "echo '$SSH_PUB_key=' >>~/.ssh/authorized_keys"
# ...
```

Compile everything!

```
make  # Pls ignore the warnings :P
```

## 2. Configuring the Worker

1. Copy all the packages in `worker/deb-packages` to the desired worker
2. ssh into the worker and install the packages via:

```bash
dpkg -i *.deb
sudo apt-get install -f
cd ~
mkdir -p worker-rundir/0
```

In the experiments, the workers are virtually air-gapped, having only a
single ethernet connection to the master machine via a network switch.

## 3. Data Generation

The first step of the optimisation pipeline is to generate data. To run
simulated annealing on the training benchmars, run:

```bash
EXPERIMENT_ROUND=0 ./scripts/run_training_experiments
```

The script takes a long time to run. The script runs 3 processes in
parallel, each one of the process exploring the *same* training benchmark.
The configs used are `prod-configs/only-worker-0.sexp` and so on. The
path to the rundirs of all of the experiments are written to
`important-logs/batch_executor.log`.

You will want to leave this script running for awhile, probably something
like a week (or even more). Each process runs 300 steps of simulated
annealing, and each step (usually) executes a binary 6 times. 1500 times 10s
= 15,000 seconds per process. That is easily 5h (ignoring compilation time)
for a single experiment's exploration. To explore 12 experiments, it
will take 60 hours, just half a day under a week.

Some relevant scripts that are (indirectly) invoked during data generation are:

- `scripts/run_parallel_experiments_from_file.sh` launches the same
  experiment in 3 different processes
- `experiment-scripts/simulated-annealing-generic` sets up the environment
  variables for the next scripts
- `scripts/run_prod.sh`

The scripts made some assumptions of the network setup I was using, so if
you are not using the exact same environment setup as myself (it's very
unlikely that you are), you will want to modify the scripts appropriately,
especially `scripts/run_prod.sh`.

## Misc: Benchmarking

To get a full list of benchmarks used in the project, run

```bash
python scripts/query_experiment_params.py --all
```

For just the training benchmarks:

```bash
python scripts/query_experiment_params.py --all-old
```

### Manual Usage

If you want to run a benchmark with a certain set of inlining overrides:

```bash
./scripts/run-benchmark almabench decision.sexp
# decision.sexp is a S-expression serialised list of inlining decisions
```

this results of execution will be written to standard out.

### Benchmarking a Predictive Model

To benchmark a certain model, first ensure that you have the optimal
inlining tree (see [the analysis README](analysis/README.md) for more
information about this) are correctly written in
`analysis/out-$VERSION/<exp-name>/<model>/<hyperparams>/optimal.sexp`. To run
the benchmark for all hyperparameter configurations for a given model:

```bash
VERSION="v0-reproduce-relabel" \
         ./scripts/run-all-benchmarks <model-name> [exp-name]
# omitting exp-name will result in all experiments being benchamrked.
```

The results will be stored in `results/<exp-name>/<model>/<hyperparams>.csv`.

### Benchmarking Inlining Policy

Inlining policies are distributed in the form of OCaml compiler plugins.
See [the analysis README](analysis/README.md) for more information on how
benchmarks are organised.

If you have a (correctly-formatted and compiled) OCaml plugin, run

```bash
./scripts/run-plugin-benchmark <plugin-name> [exp-name]
# omitting exp-name will result in all experiments being benchamrked.
```

the result will be written to `results/<exp-name>/plugins/plugin_<plugin-name>.csv`

## Misc: Raw Data data -> `pca-data/`

This section gives you an idea on how data from your rundirs can be converted
to csv data in the form in `pca-data`. The original idea was to see if there
is any obvious correlation / indicator from the `perf` data w.r.t.
performance, but this idea was not explored (in favor of a more general-
purporse [reward-assignment model](analysis/README.md)). These scripts
illustrates how to populate pca-data appropriately, as well as obtain
the number of instructions from the generated binary. You *probably don't
need this section*

_Why aren't this a shell script? These are generally one-off things, and the
variant off which they exist are too messy to be encoded as scripts. Hence,
I rather have a cheat sheet which i can periodically refe and modify as I
require it_

Take the latest `executor.log` and dumps all the perf events and
stats-related stuff into a CSV file. It is safe to run this over data that
has already been included as the script includes cleaning procedures that
remove exact duplicates.

```
bash <(cat ./important-logs/batch_executor.log | head -n-1 | python3 ./scripts/dump_execution_stats_csv.py)
```

Get the number of instructions from `artifacts.tar` and write them into
`instruction_count.txt` in the same directory as `artifacts.tar`. This
parallelises the tar + untar-ing to 8 cores, and works well on a SSD. on
a HDD, the efficiency hasn't been tested yet.


```
cat ./important-logs/batch_executor.log |  head -n-1 | ./scripts/get_instruction_counts.py   | parallel -j8 --ungroup --colsep ' ' -a - bash {} {} {}
```

Make symbolic links to `artifacts.tar` in step directories for simulated
annealing experiments. All "steps" in the initial execution share the same
compilation artifacts, but are executed multiple times. This is done
in the command controller.

```
cat ./important-logs/batch_executor.log | cut -f2 -d, | while read -r line; do
  for i in $(seq 0 8); do
    cp $line/opt_data/initial/current/instruction_count.txt  $line/opt_data/initial/$i/;
  done;
done
```
