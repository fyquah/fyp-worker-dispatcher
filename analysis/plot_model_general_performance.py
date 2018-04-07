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


    for benchmark in py_common.EXPERIMENT_TO_PARAMETERS.keys():
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

    x = [a[0] for a in plot_data]

    matplotlib.rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})

    fig, axes = plt.subplots(2, 1, figsize=(18, 9))
    fig.suptitle(args.title)

    axes[0].axhline(y=1.0, color='r', linestyle='--')
    axes[0].axhline(y=0.0, color='g', linestyle='--')

    axes[0].grid()
    axes[0].plot(x, [a[1] for a in plot_data], 'kx')
    axes[0].plot(x, [a[2] for a in plot_data], 'rx')
    axes[0].bar(range(0, len(x)), [a[3] or 0 for a in plot_data])
    # axes[0].xticks(range(0, len(x)), x)
    axes[0].set_xlabel("benchmark")
    axes[0].set_xlabel("Relative performance")
    axes[0].set_ylim([-1.5, 1.5])

    # axes[0].xticks(range(0, len(x)), x)
    axes[1].bar(x, [a[6] or 0 for a in plot_data])
    axes[1].set_xlabel("benchmark")
    axes[1].set_xlabel("Obtained speedup")
    axes[1].plot(x, [a[4] for a in plot_data], 'kx')
    axes[1].plot(x, [a[5] for a in plot_data], 'rx')
    axes[1].set_ylim([-0.1, 0.2])

    plt.grid(True)

    if args.pdf is None:
        plt.show()
    else:
        pp = backend_pdf.PdfPages(args.pdf)
        pp.savefig()
        pp.close()


if __name__ == "__main__":
    main()
