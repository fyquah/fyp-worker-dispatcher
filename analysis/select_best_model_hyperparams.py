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
            "../results/%s/%s/"
            % (benchmark, args.model))
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

    collected_ratios = collections.defaultdict(list)

    for benchmark in py_common.EXPERIMENT_TO_PARAMETERS.keys():
        best_ratio = None
        worst_ratio = None
        active_ratio = None

        for filename, time, ratio in all_records[benchmark]:
            collected_ratios[filename].append(max(0, ratio))

    for k in collected_ratios.keys():
        if len(collected_ratios[k]) < len(py_common.EXPERIMENT_TO_PARAMETERS.keys()):
            del collected_ratios[k]

    for k, v in collected_ratios.iteritems():
        print k, sum(v)

    print min([(k, v) for k, v in collected_ratios.iteritems()],
            key=lambda a: sum(a[1]))

if __name__ == "__main__":
    main()
