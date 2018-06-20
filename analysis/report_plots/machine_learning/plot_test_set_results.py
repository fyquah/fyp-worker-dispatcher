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

def box_plot_of_speedups(xs, speedups, name):
    speedups = np.array(speedups).T

    plt.boxplot(speedups, positions=np.arange(len(xs)))
    plt.title("Performance of Unimodel Inlining Policies in %s Benchmarks." % name)
    plt.xticks(np.arange(len(xs)), xs, rotation="60")
    plt.xlabel("Benchmark")
    plt.ylabel("Speedup")
    plt.tight_layout()
    plt.grid()


def find_entry(entries, b):
    found = None
    for entry in entries:
        if entry.benchmark == b:
            if found is None:
                found = entry
            else:
                assert False
    if found is None:
        print len(entries), [e.benchmark for e in entries], b
        assert False
    return found


def compute_regret(
        all_records_by_plugin,
        current_set,
        benchmarks,
        max_speedup_by_bench,
        min_speedup_by_bench,
        kind):
    if kind == "max":
        f = max
    elif kind == "mean":
        f = lambda g: sum(g) / float(len(benchmarks))
    else:
        assert False
    return f(min(float(max_speedup_by_bench[b] - find_entry(all_records_by_plugin[p], b=b).speedup)
                                  / float(max_speedup_by_bench[b] - min_speedup_by_bench[b])
                             for p in current_set)
                         for b in benchmarks)


Solution = collections.namedtuple("Solution", ["solution", "regrets", "test_regrets"])


def greedily_solve_solutions(
        all_records_by_plugin,
        reference_benchmarks,
        test_benchmarks,
        min_speedup_by_bench,
        max_speedup_by_bench, kind):

    current_set = []
    regrets = []
    test_regrets = []

    for k in range(len(all_records_by_plugin)):
        best_regret = np.inf
        p_best = None

        for p_candidate, entries in all_records_by_plugin.iteritems():
            if p_candidate in current_set:
                continue

            regret = compute_regret(
                    all_records_by_plugin=all_records_by_plugin,
                    current_set=current_set + [p_candidate],
                    benchmarks=reference_benchmarks,
                    min_speedup_by_bench=min_speedup_by_bench,
                    max_speedup_by_bench=max_speedup_by_bench,
                    kind=kind)

            if regret < best_regret:
                best_regret = regret
                p_best = p_candidate

        current_set.append(p_best)
        test_regret = compute_regret(
                all_records_by_plugin=all_records_by_plugin,
                current_set=current_set,
                benchmarks=test_benchmarks,
                min_speedup_by_bench=min_speedup_by_bench,
                max_speedup_by_bench=max_speedup_by_bench,
                kind=kind)
        test_regrets.append(test_regret)
        regrets.append(best_regret)

    assert set(current_set) == set(all_records_by_plugin.keys())
    return Solution(solution=current_set, regrets=regrets, test_regrets=test_regrets)

def illustrate_greedy_solutions(
        plot_dir,
        uni_model_plugins, min_unimodel_speedup_by_bench, max_unimodel_speedup_by_bench,
        training_xs, test_xs):

    normal_solution = greedily_solve_solutions(
            uni_model_plugins,
            reference_benchmarks=training_xs,
            test_benchmarks=test_xs,
            min_speedup_by_bench=min_unimodel_speedup_by_bench,
            max_speedup_by_bench=max_unimodel_speedup_by_bench,
            kind="mean")
    cheat_solution = greedily_solve_solutions(
            uni_model_plugins,
            reference_benchmarks=test_xs,
            test_benchmarks=test_xs,
            min_speedup_by_bench=min_unimodel_speedup_by_bench,
            max_speedup_by_bench=max_unimodel_speedup_by_bench,
            kind="mean")

    plt.figure()
    plt.grid()
    plt.title("Mean Regret Over Benchmarks with Greedy Allocation")
    plt.plot(normal_solution.regrets, label="Training Regret (%d benchmarks)" % len(training_xs))
    plt.plot(normal_solution.test_regrets, label="Test Regret (%d benchmarks)" % len(test_xs))
    plt.plot(cheat_solution.regrets, label="Test Regret (Cheating) (%d benchmarks)" % len(test_xs))
    plt.xlabel("Number of Models, K")
    plt.legend()
    plt.ylabel(r"Regret, $J$")
    plt.hlines(0.1, 0, len(uni_model_plugins), colors='k', linestyles="--", )
    plt.hlines(0.05, 0, len(uni_model_plugins), colors='k', linestyles="--", )
    plt.tight_layout()
    plt.savefig(fname=os.path.join(plot_dir, "mean-regret-plot.pdf"))

    normal_solution = greedily_solve_solutions(
            uni_model_plugins,
            reference_benchmarks=training_xs,
            test_benchmarks=test_xs,
            min_speedup_by_bench=min_unimodel_speedup_by_bench,
            max_speedup_by_bench=max_unimodel_speedup_by_bench,
            kind="max")
    cheat_solution = greedily_solve_solutions(
            uni_model_plugins,
            reference_benchmarks=test_xs,
            test_benchmarks=test_xs,
            min_speedup_by_bench=min_unimodel_speedup_by_bench,
            max_speedup_by_bench=max_unimodel_speedup_by_bench,
            kind="max")

    plt.figure()
    plt.grid()
    plt.title("Max Regret Over Benchmarks with Greedy Allocation")
    plt.plot(normal_solution.regrets, label="Training Regret (%d benchmarks)" % len(training_xs))
    plt.plot(normal_solution.test_regrets, label="Test Regret (%d benchmarks)" % len(test_xs))
    plt.plot(cheat_solution.regrets, label="Test Regret (Cheating) (%d benchmarks)" % len(test_xs))
    plt.xlabel("Number of Models, K")
    plt.legend()
    plt.ylabel(r"Regret, $J$")
    plt.hlines(0.1, 0, len(uni_model_plugins), colors='k', linestyles="--", )
    plt.hlines(0.05, 0, len(uni_model_plugins), colors='k', linestyles="--", )
    plt.tight_layout()
    plt.savefig(fname=os.path.join(plot_dir, "max-regret-plot.pdf"))


def main():
    matplotlib.rc("text", usetex=True)
    matplotlib.rc("font", size=12)
    matplotlib.rc('figure', figsize=(11.69,7.27))

    initial_exec_time_by_bench = {}
    entries_by_bench = {}
    min_speedup_by_bench = {}
    max_speedup_by_bench = {}
    min_unimodel_speedup_by_bench = {}
    max_unimodel_speedup_by_bench = {}

    all_records_by_bench = collections.defaultdict(list)
    all_records_by_plugin = collections.defaultdict(list)

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

            entry = Entry(plugin=plugin_name, benchmark=benchmark, time=time, speedup=speedup)
    
            all_records_by_bench[benchmark].append(entry)
            all_records_by_plugin[plugin_name].append(entry)

        min_speedup_by_bench[benchmark] = min(e.speedup for e in all_records_by_bench[benchmark])
        max_speedup_by_bench[benchmark] = max(e.speedup for e in all_records_by_bench[benchmark])

        arr = [e.speedup for e in all_records_by_bench[benchmark] if e.plugin != "nothing" and "moe" not in e.plugin]
        min_unimodel_speedup_by_bench[benchmark] = min(arr)
        max_unimodel_speedup_by_bench[benchmark] = max(arr)
        all_records_by_bench[benchmark].sort(key=lambda e : -np.inf if e.plugin == "nothing" else e.time)

    plot_dir = "report_plots/machine_learning/plots/plugins-results/"
    box_plot_of_speedups_for_hyper(
            plot_dir=plot_dir,
            all_records_by_plugin=all_records_by_plugin,
            max_unimodel_speedup_by_bench=max_unimodel_speedup_by_bench,
            min_unimodel_speedup_by_bench=min_unimodel_speedup_by_bench)
    return

    test_speedups = []
    test_xs = []
    training_speedups = []
    training_xs = []

    for benchmark in exps:
        if "fyq" in benchmark:
            continue

        if benchmark in py_common.INITIAL_EXPERIMENTS:
            arr = [e.speedup for e in all_records_by_bench[benchmark] if e.plugin != "nothing" and "moe" not in e]
            training_speedups.append(arr)
            training_xs.append(benchmark)
        else:
            arr = [e.speedup for e in all_records_by_bench[benchmark] if e.plugin != "nothing" and "moe" not in e]
            test_speedups.append(arr)
            test_xs.append(benchmark)

    uni_model_plugins = [p for p in ALL_PLUGINS if "moe" not in p and p != "nothing"]

    
    plot_dir = "report_plots/machine_learning/plots/plugins-results/"
    if not os.path.exists(plot_dir):
        os.makedirs(plot_dir)

    illustrate_greedy_solutions(
            plot_dir,
            uni_model_plugins={ k: all_records_by_plugin[k] for k in uni_model_plugins },
            min_unimodel_speedup_by_bench=min_unimodel_speedup_by_bench,
            max_unimodel_speedup_by_bench=max_unimodel_speedup_by_bench,
            training_xs=training_xs,
            test_xs=test_xs)

    plt.figure()
    box_plot_of_speedups(training_xs, training_speedups, name="Training")
    plt.savefig(fname=os.path.join(plot_dir, "box-plot-of-hyperparams-in-training.pdf"))

    plt.figure()
    box_plot_of_speedups(test_xs, test_speedups, name="Test")
    plt.savefig(fname=os.path.join(plot_dir, "box-plot-of-hyperparams-in-test.pdf"))

    print "Median of Median training speedup:", np.median([np.median(x) for x in training_speedups])
    print "Median of Median test speedup:", np.median([np.median(x) for x in test_speedups])
    print "Mean of Median training speedup:", np.mean([np.median(x) for x in training_speedups])
    print "Mean of Median test speedup:", np.mean([np.median(x) for x in test_speedups])
    print "25-th test speedup:", np.median([np.percentile(x, 25) for x in test_speedups])
    print "75-th test speedup:", np.median([np.percentile(x, 75) for x in test_speedups])
    
    for plugin in ALL_PLUGINS:
        if "nothing" == plugin or "moe" in plugin:
            continue

        if benchmark in py_common.INITIAL_EXPERIMENTS:
            arr = [e.speedup for e in all_records_by_bench[benchmark] if e.plugin != "nothing" and "moe" not in e]
            training_speedups.append(arr)
            training_xs.append(benchmark)
        else:
            arr = [e.speedup for e in all_records_by_bench[benchmark] if e.plugin != "nothing" and "moe" not in e]
            test_speedups.append(arr)
            test_xs.append(benchmark)

    uni_model_plugins = [p for p in ALL_PLUGINS if "moe" not in p and p != "nothing"]


def is_test_benchmarks(b):
    assert isinstance(b, str)
    return "fyq" not in b and b not in py_common.INITIAL_EXPERIMENTS


def box_plot_of_speedups_for_hyper(
        plot_dir,
        all_records_by_plugin, max_unimodel_speedup_by_bench, min_unimodel_speedup_by_bench):
    regrets = []
    uni_model_plugins = [p for p in ALL_PLUGINS if "moe" not in p and p != "nothing"]

    for p in uni_model_plugins:
        my_regret = []
        my_speedup = []
        entries = all_records_by_plugin[p]

        for e in entries:
            speedup = e.speedup
            b = e.benchmark
            if is_test_benchmarks(b):
                my_speedup.append(speedup)
                my_regret.append(
                        float(max_unimodel_speedup_by_bench[b] - speedup)
                        / (max_unimodel_speedup_by_bench[b] - min_unimodel_speedup_by_bench[b]))
        regrets.append((p, my_regret, my_speedup))

    regrets.sort(key=lambda (_n, r, _s) : -np.median(r))

    names   = [a for (a, _r, _s) in regrets]
    speedups = np.array([a for (_n, _r, a) in regrets])
    regrets = np.array([a for (_n, a, _s) in regrets])
    regrets = np.array(regrets)
    ticks = [translate(n.replace("v1_neural_network_", "").replace("_", "-")) for n in names]

    plt.figure()
    plt.title(r"Box Plot of Regret for a Single Model in $\mathcal{B}_{test}$")
    plt.boxplot(regrets.T, vert=False)
    plt.yticks(range(1, 1 + len(names)), ticks)
    plt.grid()
    plt.tight_layout()
    plt.savefig(fname=os.path.join(plot_dir, "test-regret-by-model.pdf"))

    plt.figure()
    plt.title(r"Box Plot of Speedup for a Single Model in $\mathcal{B}_{test}$")
    plt.boxplot(speedups.T, vert=False)
    plt.yticks(range(1, 1 + len(names)), ticks)
    plt.tight_layout()
    plt.grid()
    plt.savefig(fname=os.path.join(plot_dir, "test-speedup-by-model.pdf"))


def translate(a):
    if a == "nothing":
        return " & Baseline & "
    elif "ridge" in a:
        if "moe" in a:
            return "L2 CMoE"

        _ridge, name, factor = a.split("-")
        factor = float(factor)

        if name == "star":
            return "L2 $H_{*}$ %f" % factor
        elif name == "general":
            return "L2 $h_{general}$ %f" % factor
        elif name == "hand":
            return "L2 $h_{hand}$ %f" % factor
        else:
            assert False

    elif "lasso" in a:
        if "moe" in a:
            return "L1 CMoE"
        lasso_, name = a.split("-")
        if name == "star":
            return "L1 $H_{*}$"
        elif name == "general":
            return "L1 $h_{general}$"
        elif name == "hand":
            return "L1 $h_{hand}$"
        else:
            assert False


if __name__ == "__main__":
    main()
