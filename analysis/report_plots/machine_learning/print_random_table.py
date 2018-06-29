import csv
import os
import sys
import collections
import math
import cPickle as pickle

import sexpdata
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import py_common
from inlining_tree import parse_time, geometric_mean


PLUGIN_SUBDIR = os.environ.get("PLUGINS_SUBDIR", "plugins")


def read(algo):
    with open("report_plots/data_generation/" % algo, 'rb') as f:
        (exec_times, initial_exec_times) = pickle.load(f)
    return (exec_times, initial_exec_times)


def read_plugin_times(benchmark, plugin_name):
    bench_dir = "../results/%s/%s/" % (benchmark, PLUGIN_SUBDIR)
    with open(os.path.join(bench_dir, "plugin_%s.csv" % plugin_name), "rb") as f:
        times = []
        for line in csv.reader(f):
            for i in range(4, len(line)):
                times.append(parse_time(line[i]))
        if len(times) >= 1:
            return geometric_mean(times)
        else:
            return None


def get_initial_time_from_records(benchmark):
    initial_exec_times = []
    try:
        with open("../pca-data/%s.csv" % benchmark, "rb") as f:
            for line in csv.reader(f):
                t = parse_time(line[-1] + "s")
                if "initial" in line[0]:
                    initial_exec_times.append(t)
        if initial_exec_times:
            return geometric_mean(initial_exec_times)
        else:
            return None
    except IOError:
        return None


def get_initial_time_from_results(benchmark):
    return read_plugin_times(benchmark, plugin_name="nothing")


def get_initial_exec_time(benchmark):
    arr = []
    time_from_pca = get_initial_time_from_records(benchmark)
    if time_from_pca is not None:
        arr.append(time_from_pca)
    t = get_initial_time_from_results(benchmark)
    if t is not None:
        arr.append(t)
    if arr:
        return min(arr)
    else:
        return None


def main():
    initial_exec_time_by_bench = {}
    all_records = collections.defaultdict(list)
    exps = py_common.EXPERIMENT_TO_PARAMETERS.keys()

    for benchmark in exps:
        initial_exec_times = []
        best_time = None
        initial_exec_time_by_bench[benchmark] = get_initial_exec_time(benchmark)


    for benchmark in exps:
        plugin_names = []
        for fname in os.listdir("../results/%s/plugins" % benchmark):
            if "random" in fname:
                plugin_names.append(os.path.splitext(os.path.basename(fname))[0].replace("plugin_", ""))

        initial_exec_time = initial_exec_time_by_bench[benchmark]
        for plugin_name in plugin_names:
            time = read_plugin_times(benchmark, plugin_name=plugin_name)
            ratio = None
            if time is not None and initial_exec_time is not None:
                speedup = (initial_exec_time - time) / initial_exec_time
            else:
                speedup = None

            if time is not None:
                all_records[benchmark].append((time, ratio, speedup, plugin_name))

    arr = []

    print "&".join(headers)
    print ">>>>> TRAINING SET <<<<<"
    for bench, items in all_records.iteritems():
        if bench not in py_common.INITIAL_EXPERIMENTS:
            continue
        print_row(bench, items)
    print ""

    global global_acc
    global_acc = []
    print ">>>>> TEST SET <<<<<"
    for bench, items in all_records.iteritems():
        if bench in py_common.INITIAL_EXPERIMENTS:
            continue
        print_row(bench, items)
    print np.median(global_acc)
    print np.mean(global_acc)
    print np.var(global_acc)
    print np.std(global_acc)
    print len(global_acc)

global_acc = []


def print_row(bench, items):

    speedups = sorted([a for (_, _, a, _) in items])
    min_speedup = np.min(speedups)
    median_speedup = np.median(speedups)
    max_speedup = np.max(speedups)

    min_speedup = "%.3f\\%%" % (min_speedup * 100)
    first_quartile = "%.3f\\%%" % (np.percentile(speedups, 25) * 100)
    median_speedup = "%.3f\\%%" % (median_speedup * 100)
    third_quartile = "%.3f\\%%" % (np.percentile(speedups, 75) * 100)
    max_speedup = "%.3f\\%%" % (max_speedup * 100)

    pass_basline = "?"

    global PLUGIN_SUBDIR
    PLUGIN_SUBDIR="plugins-valid"
    time = read_plugin_times(bench, plugin_name="v1_neural_network_ridge_moe")
    initial_exec_time = get_initial_exec_time(bench)
    speedup = (initial_exec_time - time) / initial_exec_time
    found = None

    for i, the_speedup in enumerate(speedups):
        if the_speedup > speedup:
            if i == 0:
                found = 0
            else:
                found = i - 1
            break

    if found is None:
        found = len(speedups)
    averaging_percentile = found / float(len(speedups))

    if "fyq" not in bench:
        global_acc.append(averaging_percentile)

    print "%s & %.3f\\%% \\\\" % (bench, (speedup - np.median(speedups)) * 100)

    # print "%s & %.3f & %.3f\\%% & %.3f \\\\" % (bench, (speedup - np.median(speedups)) * 100.0, speedup * 100.0, averaging_percentile)
    # print "%s & %d & %s & %s & %s & %s & %s \\\\" % (bench, len(speedups), min_speedup, first_quartile, median_speedup, third_quartile, max_speedup)

headers = ["Benchmark", "Num samples", "Min", "25th Percentile", "Median", "75th Percentile", "Max"]


if __name__ == "__main__":
    main()
