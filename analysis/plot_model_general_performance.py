import argparse
import collections
import csv
import os
import re

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

parser = argparse.ArgumentParser()
parser.add_argument("--decay-factor", type=str, help="filename")
parser.add_argument("--ridge-factor", type=str, help="filename")
parser.add_argument("--benefit-function", type=str, help="")

def almost_equal(a, b):
    return np.abs(float(a) - float(b)) < 0.000001

def matches_filter(filename, args):
    m = re.search("decay-([\.0-9]+)-ridge-([\.0-9]+)-benefit-([a-zA-Z_]+)", filename)
    decay_factor = m.group(1)
    ridge_factor = m.group(2)
    benefit_function = m.group(3)
    assert decay_factor is not None 
    assert ridge_factor is not None 
    assert benefit_function is not None 
    return ((args.decay_factor is None or almost_equal(decay_factor, args.decay_factor))
            and (args.ridge_factor is None or almost_equal(ridge_factor, args.ridge_factor))
            and (args.benefit_function is None or benefit_function == args.benefit_function))

def main():
    args = parser.parse_args()
    plot_data = []
    initial_exec_time_by_bench = {}
    best_times_by_bench = {}
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
        initial_exec_time_by_bench[benchmark] = geometric_mean(initial_exec_times)
        best_times_by_bench[benchmark] = best_time
    del best_time
    all_records = collections.defaultdict(list)

    for benchmark in py_common.EXPERIMENT_TO_PARAMETERS.keys():
        bench_dir = (
            "../results/%s/linear-general-reward-without-normalisation/"
            % benchmark)
        csv_files = os.listdir(bench_dir)
        initial_exec_time = initial_exec_time_by_bench[benchmark]
        best_time = best_times_by_bench[benchmark]
        for filename in csv_files:
            with open(os.path.join(bench_dir, filename), "rb") as f:
                times = []
                for line in csv.reader(f):
                    times.append(parse_time(line[4]))
                if len(times) >= 1:
                    time = geometric_mean(times)
                    ratio = (time - best_time) / (initial_exec_time - best_time)
                    all_records[benchmark].append((filename, time, ratio))


    for benchmark in py_common.EXPERIMENT_TO_PARAMETERS.keys():
        best_ratio = None
        worst_ratio = None
        active_ratio = None

        for filename, _time, ratio in all_records[benchmark]:
            if best_ratio is None:
                best_ratio = ratio
                worst_ratio = ratio
            else:
                best_ratio = min(best_ratio, ratio)
                worst_ratio = max(worst_ratio, ratio)

            if matches_filter(filename, args):
                if active_ratio is None:
                    active_ratio = ratio
                else:
                    active_ratio = min(active_ratio, ratio)

        plot_data.append((benchmark, best_ratio, worst_ratio, active_ratio))
        print benchmark, best_ratio, initial_exec_time, best_time

    x = [a[0] for a in plot_data]

    plt.title("Individually-tuned Models on Individual Benchmarks")
    plt.axhline(y=1.0, color='r', linestyle='--')
    plt.axhline(y=0.0, color='g', linestyle='--')

    plt.plot(x, [a[1] for a in plot_data], 'gx')
    plt.plot(x, [a[2] for a in plot_data], 'rx')
    plt.bar(x, [a[3] or 0 for a in plot_data])

    plt.show()

if __name__ == "__main__":
    main()
