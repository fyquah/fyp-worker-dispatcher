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

def read(algo):
  with open("report_plots/data_generation/" % algo, 'rb') as f:
    (exec_times, initial_exec_times) = pickle.load(f)
  return (exec_times, initial_exec_times)


def main():
    plugin_name = sys.argv[1]
    initial_exec_time_by_bench = {}
    best_times_by_bench = {}
    all_records = {}
    exps = py_common.EXPERIMENT_TO_PARAMETERS.keys()

    for benchmark in exps:
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

    for benchmark in exps:
        bench_dir = (
            "../results/%s/%s/"
            % (benchmark, "plugins"))
        if not os.path.exists(bench_dir):
            all_records[benchmark] = (None, None, None)
            continue
        csv_files = os.listdir(bench_dir)
        initial_exec_time = initial_exec_time_by_bench[benchmark]
        best_time = best_times_by_bench[benchmark]
        try:
            with open(os.path.join(bench_dir, "plugin_%s.csv" % plugin_name), "rb") as f:
                times = []
                for line in csv.reader(f):
                    for i in range(4, len(line)):
                        times.append(parse_time(line[i]))
                if len(times) >= 1:
                    time = geometric_mean(times)
                    ratio = (time - best_time) / (initial_exec_time - best_time)
                    speedup = (initial_exec_time - time) / initial_exec_time
                    all_records[benchmark] = (time, ratio, speedup)
                else:
                    all_records[benchmark] = (None, None, None)
        except IOError:
            all_records[benchmark] = (None, None, None)

    arr = []

    print ">>>>> TRAINING SET <<<<<"
    for bench, (time, _ratio, speedup) in all_records.iteritems():
        if bench not in py_common.INITIAL_EXPERIMENTS:
            continue
        if speedup is None:
            print "%s: N/A" % (bench)
        else:
            print "%s: %f%% (%.3fs)" % (bench, speedup * 100, time)
    print ""

    print ">>>>> TEST SET <<<<<"
    for bench, (time, _ratio, speedup) in all_records.iteritems():
        if bench in py_common.INITIAL_EXPERIMENTS:
            continue
        if speedup is None:
            print "%s: N/A" % (bench)
        else:
            print "%s: %f%% (%.3fs)" % (bench, speedup * 100, time)

if __name__ == "__main__":
    main()
