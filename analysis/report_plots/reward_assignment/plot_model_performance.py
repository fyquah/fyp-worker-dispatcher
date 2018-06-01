import argparse
import collections
import csv
import os
import re

import matplotlib
import matplotlib.pyplot as plt
from matplotlib.backends import backend_pdf
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
parser.add_argument("--title", type=str, help="title")
parser.add_argument("--decay-factor", type=str, help="filename")
parser.add_argument("--ridge-factor", type=str, help="filename")
parser.add_argument("--benefit-function", type=str, help="")
parser.add_argument("--model", type=str, required=True,
        help="linear-general-reward-without-normalisation OR linear-general-reward-learn-normalised-use-unnormalised")
parser.add_argument("--pdf", type=str, default=None, help="")

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
        if not os.path.exists(bench_dir):
            continue
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
                    speedup = (initial_exec_time - time) / initial_exec_time
                    all_records[benchmark].append((filename, time, ratio, speedup))


    for benchmark in py_common.INITIAL_EXPERIMENTS:
        best_ratio = None
        worst_ratio = None
        best_speedup = None
        worst_speedup = None
        active_ratio = None
        active_speedup = None

        for filename, _time, ratio, speedup in all_records[benchmark]:
            if best_ratio is None:
                best_ratio = ratio
                worst_ratio = ratio
                best_speedup = speedup
                worst_speedup = speedup
            else:
                best_ratio = min(best_ratio, ratio)
                worst_ratio = max(worst_ratio, ratio)
                best_speedup = max(best_speedup, speedup)
                worst_speedup = min(worst_speedup, speedup)

            if matches_filter(filename, args):
                if active_ratio is None:
                    active_ratio = ratio
                    active_speedup = speedup
                else:
                    active_ratio = min(active_ratio, ratio)
                    active_speedup = max(active_speedup, speedup)

        plot_data.append((benchmark, best_ratio, worst_ratio, active_ratio, best_speedup, worst_speedup, active_speedup))
        print benchmark, best_ratio, initial_exec_time, best_time

    plot_data.sort(key=lambda k : float(k[3]))

    x = [a[0] for a in plot_data]

    labelsize = 10
    matplotlib.rc('font',**{
        'family':'sans-serif',
        'sans-serif':['Helvetica'],
    })
    matplotlib.rcParams.update({'font.size': 12, "font.family": "serif"})
    matplotlib.rc('xtick', labelsize=labelsize)
    matplotlib.rc('ytick', labelsize=labelsize)
    matplotlib.rc('figure', figsize=(11.69,7.27))

    plt.figure()
    plt.axhline(y=1.0, color='r', linestyle='--')
    plt.axhline(y=0.0, color='g', linestyle='--')
    plt.grid()
    plt.title("Relative performance of \"Optimal Tree\"")
    plt.plot(range(len(plot_data)), [a[1] for a in plot_data], 'kx')
    plt.plot(range(len(plot_data)), [a[2] for a in plot_data], 'rx')
    plt.bar(range(0, len(x)), [a[3] or 0 for a in plot_data])
    plt.xticks(range(0, len(x)), x)
    plt.xlabel("Benchmark")
    plt.ylabel("Relative performance")
    plt.ylim([-1.5, 1.5])
    plt.xticks(range(0, len(x)), x, rotation="60")
    plt.tight_layout()
    plt.savefig("report_plots/reward_assignment/model_relative_performance.pdf")

    plt.figure()
    plt.bar(range(len(plot_data)), [a[6] or 0 for a in plot_data])
    plt.xlabel("Benchmark")
    plt.ylabel("Speedup")
    plt.title("Absolute Performance of \"Optimal Tree\"")
    plt.plot(range(len(plot_data)), [a[4] for a in plot_data], 'kx')
    plt.plot(range(len(plot_data)), [a[5] for a in plot_data], 'rx')
    plt.ylim([-0.1, 0.2])
    plt.xticks(range(0, len(x)), x, rotation="60")
    plt.grid()
    plt.tight_layout()
    plt.savefig("report_plots/reward_assignment/model_obtained_speedup.pdf")

    # plt.grid(True)
    # plt.savefig("report_plots/reward_assignment/model_performance.pdf")


if __name__ == "__main__":
    main()
