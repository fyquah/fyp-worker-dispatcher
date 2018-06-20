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

Entry = collections.namedtuple(
        "Entry", ["plugin", "benchmark", "time", "speedup"])


PLUGIN_SUBDIR = os.environ.get("PLUGINS_SUBDIR", "plugins-valid")


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

ALL_PLUGINS = [
        "nothing",
        "v1_neural_network_lasso_general",
        "v1_neural_network_lasso_hand",
        "v1_neural_network_lasso_star",
        "v1_neural_network_ridge_general_0.00005",
        "v1_neural_network_ridge_general_0.0001",
        "v1_neural_network_ridge_general_0.0005",
        "v1_neural_network_ridge_general_0.001",
        "v1_neural_network_ridge_general_0.005",
        "v1_neural_network_ridge_general_0.01",
        "v1_neural_network_ridge_general_0.05",
        "v1_neural_network_ridge_hand_0.00005",
        "v1_neural_network_ridge_hand_0.0001",
        "v1_neural_network_ridge_hand_0.0005",
        "v1_neural_network_ridge_hand_0.001",
        "v1_neural_network_ridge_hand_0.005",
        "v1_neural_network_ridge_hand_0.01",
        "v1_neural_network_ridge_hand_0.05",
        "v1_neural_network_ridge_star_0.00005",
        "v1_neural_network_ridge_star_0.0001",
        "v1_neural_network_ridge_star_0.0005",
        "v1_neural_network_ridge_star_0.001",
        "v1_neural_network_ridge_star_0.005",
        "v1_neural_network_ridge_star_0.01",
        "v1_neural_network_ridge_star_0.05",
        "v1_neural_network_ridge_moe",
        "v1_neural_network_lasso_moe",
]


def main():
    initial_exec_time_by_bench = {}
    entries_by_bench = {}
    all_records = collections.defaultdict(list)
    exps = py_common.EXPERIMENT_TO_PARAMETERS.keys()

    for benchmark in exps:
        initial_exec_times = []
        best_time = None
        initial_exec_time_by_bench[benchmark] = get_initial_exec_time(benchmark)

    for benchmark in exps:
        for plugin_name in ALL_PLUGINS:
            initial_exec_time = initial_exec_time_by_bench[benchmark]
            time = read_plugin_times(benchmark, plugin_name=plugin_name)
            if time is not None:
                ratio = None
                if initial_exec_time is not None:
                    speedup = (initial_exec_time - time) / initial_exec_time
                else:
                    speedup = None
            else:
                time = None
                speedup = None
    
            all_records[benchmark].append(Entry(
                plugin=plugin_name, benchmark=benchmark, time=time, speedup=speedup))
        all_records[benchmark].sort(key=lambda e : -np.inf if e.plugin == "nothing" else e.time)

    print "\\begin{longtable}"
    print "{  l | l l l | r r }"
    print "\\textbf{Benchmark} & \\multicolumn{2}{c}{\\textbf{Reward Model}} & $\\tau$ & \\textbf{Time(s)} & \\textbf{Speedup} \\\\"
    print "\\hline"

    for benchmark, entries in all_records.iteritems():
        has_transition = False
        for i, entry in enumerate(entries):
            if not has_transition and entry.speedup <= 0.0 and i != 0:
                print "\\cline{2-6}"
                has_transition = True

            if i == 0:
                print "\\textit{", benchmark, "}& ",
            else:
                print (" " * len(benchmark)), "& ",

            print translate(entry.plugin.replace("v1_neural_network_", "").replace("_", "-")), "&",
            print ("%.3f" % (entry.time or 0.0)), "&",
            print ("%.3f\\%%" % (100.0 * (entry.speedup or 0.0))), "\\\\"

            if i == 0:
                print "\\cline{2-6}"

        print "\\hline"
    print "\\end{longtable}"

def translate(a):
    if a == "nothing":
        return " & Baseline & "
    elif "ridge" in a:
        if "moe" in a:
            return "L2 & CMoE & -"

        _ridge, name, factor = a.split("-")
        factor = float(factor)

        if name == "star":
            return "L2 & $H_{*}$ & %f" % factor
        elif name == "general":
            return "L2 & $h_{general}$ & %f" % factor
        elif name == "hand":
            return "L2 & $h_{hand}$ & %f" % factor
        else:
            assert False

    elif "lasso" in a:
        if "moe" in a:
            return "L1 & CMoE & -"
        lasso_, name = a.split("-")
        if name == "star":
            return "L1 & $H_{*}$ & -"
        elif name == "general":
            return "L1 & $h_{general}$ & -"
        elif name == "hand":
            return "L1 & $h_{hand}$ & -"
        else:
            assert False


if __name__ == "__main__":
    main()
