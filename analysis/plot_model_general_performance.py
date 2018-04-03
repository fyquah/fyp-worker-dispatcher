import os
import csv

import matplotlib.pyplot as plt
import numpy as np

import py_common


def parse_time(s):
    assert isinstance(s, str)
    if s[-1] == "s":
        return float(s[:-1])
    else:
        assert False


def geometric_mean(times):
    assert len(times) >= 1
    acc = 1.0
    for t in times:
        acc *= t
    return acc ** (1.0 / len(times))


def main():
    plot_data = []
    for benchmark in py_common.EXPERIMENT_TO_PARAMETERS.keys():
        initial_exec_times = []
        best_time = None
        with open("../pca-data/%s.csv" % benchmark, "rb") as f:
            for line in csv.reader(f):
                t = parse_time(line[-1] + "s")
                if "initial" in line[0]:
                    initial_exec_times.append(t)
                if best_time is None:
                    best_time = t
                else:
                    best_time = min(best_time, t)
        initial_exec_time = geometric_mean(initial_exec_times)

        bench_dir = (
            "../results/%s/linear-general-reward-without-normalisation/"
            % benchmark)
        csv_files = os.listdir(bench_dir)
        proposed_time = None
        for filename in csv_files:
            with open(os.path.join(bench_dir, filename), "rb") as f:
                times = []
                for line in csv.reader(f):
                    times.append(parse_time(line[4]))
                if len(times) >= 1:
                    time = geometric_mean(times)
                    if proposed_time is None:
                        proposed_time = time
                    else:
                        proposed_time = min(proposed_time, time)
        if proposed_time is not None:
            plot_data.append((benchmark, (proposed_time - best_time) / (initial_exec_time - best_time)))
    x = [a[0] for a in plot_data]
    y = [a[1] for a in plot_data]
    plt.title("Individually-tuned Models on Individual Benchmarks")
    plt.axhline(y=1.0, color='r', linestyle='--')
    plt.axhline(y=0.0, color='g', linestyle='--')
    plt.bar(x, y)
    plt.show()

if __name__ == "__main__":
    main()
