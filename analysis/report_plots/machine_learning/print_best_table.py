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
    try:
        with open(os.path.join(bench_dir, "plugin_%s.csv" % plugin_name), "rb") as f:
            times = []
            for line in csv.reader(f):
                for i in range(4, len(line)):
                    times.append(parse_time(line[i]))
            if len(times) >= 1:
                return geometric_mean(times)
            else:
                return None
    except IOError:
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
    all_records = {}
    exps = py_common.EXPERIMENT_TO_PARAMETERS.keys()

    for benchmark in exps:
        initial_exec_times = []
        best_time = None
        initial_exec_time_by_bench[benchmark] = get_initial_exec_time(benchmark)

    for benchmark in exps:
        initial_exec_time = initial_exec_time_by_bench[benchmark]
        time = None
        name = None
        try:
            for fname in os.listdir(os.path.join("../results", benchmark, PLUGIN_SUBDIR)):
                plugin_name = str(os.path.splitext(os.path.basename(fname))[0][len("plugin_"):])
                time_ = read_plugin_times(benchmark, plugin_name=plugin_name)
                if time is None or time_ < time:
                    time = time_
                    name = plugin_name
        except IOError:
            all_records[benchmark] = (None, None, None)

        if time is not None:
            ratio = None
            if initial_exec_time is not None:
                speedup = (initial_exec_time - time) / initial_exec_time
            else:
                speedup = None
            all_records[benchmark] = (time, name, speedup)
        else:
            all_records[benchmark] = (None, None, None)

    arr = []

    print ">>>>> TRAINING SET <<<<<"
    for bench, (time, name, speedup) in all_records.iteritems():
        if bench not in py_common.INITIAL_EXPERIMENTS:
            continue
        print_row(bench, speedup, time, plugin_name=name)
    print ""

    print ">>>>> TEST SET <<<<<"
    keys = sorted(all_records.keys())
    for bench in keys:
        (time, name, speedup) = all_records[bench]
        if bench in py_common.INITIAL_EXPERIMENTS:
            continue
        print_row(bench, speedup, time, plugin_name=name)


def print_row(bench, speedup, time, plugin_name):
    if speedup is not None:
        speedup = "%.3f%%" % (speedup * 100)
    else:
        speedup = "N/A"
    if time is not None:
        time = "%.3fs" % time
    else:
        time = "N/A"
    print "%s: %s (%s) %s" % (bench, speedup, time, plugin_name)


if __name__ == "__main__":
    main()
