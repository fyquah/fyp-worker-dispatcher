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
parser.add_argument("--model", type=str, required=True,
        help="linear-general-reward-without-normalisation OR linear-general-reward-learn-normalised-use-unnormalised")

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
    for benchmark in py_common.INITIAL_EXPERIMENTS:
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

    for benchmark in py_common.INITIAL_EXPERIMENTS:
        bench_dir = (
            "../results/%s/%s/"
            % (benchmark, args.model))
        csv_files = os.listdir(bench_dir)
        initial_exec_time = initial_exec_time_by_bench[benchmark]
        best_time = best_times_by_bench[benchmark]
        for filename in csv_files:
            if not matches_filter(filename, args):
                continue
            with open(os.path.join(bench_dir, filename), "rb") as f:
                times = []
                for line in csv.reader(f):
                    times.append(parse_time(line[4]))
                if len(times) >= 1:
                    time = geometric_mean(times)
                    ratio = (time - best_time) / (initial_exec_time - best_time)
                    speedup = (initial_exec_time - time) / initial_exec_time
                    all_records[filename].append((benchmark, time, ratio, speedup))

    collected_ratios = collections.defaultdict(list)
    NUM_ENTRIES = 10

    arr = []
    for k, v in all_records.iteritems():
        arr.append((k, v))

    # arr.sort(key=lambda (_k, v): max(e[3] for e in v))
    arr.sort(key=lambda (_k, v): sum(e[2] for e in v))
    header = ", ".join([
        "Benchmark", 
        "Best Seen",
        "Best Hyperparams",
        ] + ["Hyperparams %d" % i for i in range(NUM_ENTRIES)])
    print header

    best_source = []

    for benchmark in py_common.INITIAL_EXPERIMENTS:
        out = [benchmark]

        out.append("%.3f%%" % ((initial_exec_time_by_bench[benchmark] - best_times_by_bench[benchmark]) / (initial_exec_time_by_bench[benchmark]) * 100))

        # AAAAA: bdd ('bdd', 12.579324946812703, -0.15538934235601762, 0.04276107403823157) decay-0.300000-ridge-0.003000-benefit-log_speedup_over_mean.csv [('kahan-sum', 8.627332007101767, 0.0906675710143047, 0.013002210856812734), ('lens', 4.491999999999999, 0.00402666060794026, 0.5420957908099022)]

        all_seen_for_benchmark = []
        for i in range(len(arr)):
            for entry in arr[i][1]:
                if entry[0] != benchmark:
                    continue
                all_seen_for_benchmark.append(entry)
        best = max(all_seen_for_benchmark, key=lambda v:v[3])
        param_name = None
        for k, v in arr:
            if best in v:
                param_name = k
        assert param_name is not None
        best_source.append(" (%s  %s)" % (benchmark, os.path.splitext(os.path.basename(param_name))[0]))
        out.append("%.3f%% (%.3f)" % (best[3] * 100, best[2]))

        # The best set of hyperparams
        for i in range(min(len(arr), NUM_ENTRIES)):
            found = False
            for entry in arr[i][1]:
                if entry[0] != benchmark:
                    continue
                speedup = entry[3]
                ratio = entry[2]
                out.append("%.3f%% (%.3f)" % (speedup * 100, ratio))
                found = True
                break
            if not found:
                out.append("N/A")
        print ", ".join(out)

    for i in range(min(len(arr), NUM_ENTRIES)):
        print "\\textit{Hyperparams} %d & %s \\\\" % (i, arr[i][0])

    with open("best-hyperparams.sexp", "w") as f:
        f.write("(\n")
        for line in best_source:
            f.write(line + "\n")
        f.write(")\n")


if __name__ == "__main__":
    main()
