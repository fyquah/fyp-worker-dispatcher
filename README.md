## Configuring a new machine to run FYP things

Install apt packages that we definitely require:

```bash
sudo apt-get install zip build-essential git

# These are required by owl
sudo apt-get install libgsl0-dev liblapacke-dev libopenblas-dev pkg-config libplplot-dev libshp-dev m4
```

Clone all the relevant repos:

```bash
mkdir -p ~/fyp
git clone git@github.com:fyquah95/fyp-worker-dispatcher.git worker-dispatcher
git clone git@github.com:fyquah95/fyp-ocaml.git ocaml
git clone git@github.com:fyquah95/fyp-experiments.git experiments
git clone git@github.com:fyquah95/fyp-opam-repo-dev.git opam-repo-dev
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

## Cheatsheet

_Why aren't this a shell script? These are generally one-off things, and the
variant off which they exist are too messy to be encoded as scripts. Hence,
I rather have a cheat sheet which i can periodically refe and modify as I
require it_

Take the latest `executor.log` and dumps all the perf events and
stats-related stuff into a CSV file. It is safe to run this over data that
has already been included as the script includes cleaning procedures that
remove exact duplicates.

```
bash <(cat ./tmp/batch_executor.log | head -n-1 | python3 ./scripts/dump_execution_stats_csv.py)
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
compilation artifacts, but are executed multiple times.

```
cat ./tmp/batch_executor.log | cut -f2 -d, | while read -r line; do
  for i in $(seq 0 8); do
    cp $line/opt_data/initial/current/instruction_count.txt  $line/opt_data/initial/$i/;
  done;
done
```
