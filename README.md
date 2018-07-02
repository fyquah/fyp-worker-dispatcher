# FYP: Inlining ML with ML

Presentation slides: [http://www.fyquah.me/fyp/presentation.pdf](http://www.fyquah.me/fyp/presentation.pdf)

Pipeline summary: [http://www.fyquah.me/fyp/summary.pdf](http://www.fyquah.me/fyp/summary.pdf)


This repository contains most of the code I have used when working on my
final year project. Some other relevant repositories are:

- [OCaml fork](https://github.com/fyquah95/fyp-ocaml) patched with the necessary support
- [Benchmark repository](https://github.com/fyquah95/fyp-experiments)
- [An opam repo](https://github.com/fyquah95/fyp-opam-repo-dev) containing the opam repo
- [This repository](https://github.com/fyquah95/fyp-worker-dispatcher) which contains most of the engineering and analysis work.

To reproduce the results in the thesis, you need to:

1. Configure the master machine
2. Configure the benchmark environments
3. Run data generation
4. Run reward assignment
5. Train and benchmark inlining policies

Most of the "interesting" analysis goes on in (4) and (5), which are
discussed in [analysis/README.md](analysis/README.md). (1), (2) and (3) are
required to setup benchmarks.

To make it easier to reproduce the results due to (4) and (5) (which is more
interesting, anyway), simply skip to
[analysis/README.md](analysis/README.md) and follow the instructions there.

## Directory Structure


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
```

In the experiments, the workers are virtually air-gapped, having only a
single ethernet connection to the master machine via a network switch.

## 3. Data Generation

The first step of the optimisation pipeline is to generate data.

## Misc: Some scripts to Corellate GC Stats etc. with Exec Times

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
