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
