import scipy.stats
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


MLStats = collections.namedtuple("MLStats", ["name", "accuracy", "doesnt_matter", "inline", "apply"])


def get_model_accuracy(plugin_name):
    logfile = "../tools/fyp_compiler_plugins/training-logs/plugin_" + plugin_name + ".log"
    if not os.path.exists(logfile):
        logfile = "../tools/fyp_compiler_plugins/training-logs/plugin_" + plugin_name + "/out.log"

    with open(logfile, "r") as f:
        for line in f:
            line = line.strip()
            if "- decision model score: " in line.strip():
                accuracy = float(line.strip().split(":")[1])
            if "doesnt matter =" in line.strip():
                doesnt_matter_prop = float(line[line.index("(") + 1:-1])
            if "inline =" in line.strip() and "predict" not in line:
                inline_prop = float(line[line.index("(") + 1:-1])
            if "apply" in line.strip() and "-" in line and "predict" not in line:
                apply_prop = float(line[line.index("(") + 1:-1])
        return MLStats(name=plugin_name, accuracy=accuracy, doesnt_matter=doesnt_matter_prop, inline=inline_prop, apply=apply_prop)


def box_plot_of_speedups(xs, speedups, name):
    matplotlib.rc("text", usetex=True)
    matplotlib.rc("font", size=15)
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

        if k == 9:
            # print the solution for the table here
            import py_common
            speedups = []
            for exp in py_common.EXPERIMENT_TO_PARAMETERS.keys():
                if exp in py_common.INITIAL_EXPERIMENTS or "fyq" in exp:
                    continue

                speedup = None
                chosen = None
                for plugin in current_set:
                    for e in all_records_by_plugin[plugin]:
                        if e.benchmark == exp and (e.speedup > speedup or speedup is None):
                            speedup = e.speedup
                            chosen = plugin

                assert chosen is not None
                name = chosen.replace("plugin_", "").replace("v1_neural_network_", "").replace("_", "-")
                print exp, "&", ("%.3f\\%%" % (speedup * 100)), "&", translate(name)
                speedups.append(speedup)
            print np.max(speedups)
            print np.median(speedups)
            sys.exit(-1)

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
    matplotlib.rc("font", size=15)
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
    matplotlib.rc("font", size=15)
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


    box_plot_of_speedups_for_hyper_with_moe(
            plot_dir=plot_dir,
            all_records_by_plugin=all_records_by_plugin,
            max_unimodel_speedup_by_bench=max_unimodel_speedup_by_bench,
            min_unimodel_speedup_by_bench=min_unimodel_speedup_by_bench)
    box_plot_of_speedups_for_hyper(
            plot_dir=plot_dir,
            all_records_by_plugin=all_records_by_plugin,
            max_unimodel_speedup_by_bench=max_unimodel_speedup_by_bench,
            min_unimodel_speedup_by_bench=min_unimodel_speedup_by_bench)
    box_plot_of_speedups_for_hyper_training(
            plot_dir=plot_dir,
            all_records_by_plugin=all_records_by_plugin,
            max_unimodel_speedup_by_bench=max_unimodel_speedup_by_bench,
            min_unimodel_speedup_by_bench=min_unimodel_speedup_by_bench)


    plt.figure()
    box_plot_of_speedups(training_xs, training_speedups, name="Training")
    plt.savefig(fname=os.path.join(plot_dir, "box-plot-of-hyperparams-in-training.pdf"))

    plt.figure()
    box_plot_of_speedups(test_xs, test_speedups, name="Test")
    plt.savefig(fname=os.path.join(plot_dir, "box-plot-of-hyperparams-in-test.pdf"))

    plt.figure()
    box_plot_of_speedups(training_xs, training_speedups, name="Training")
    plt.title("Performance of CMoE L2 Compared to Uni-Model Policies in Training Benchmarks.")
    arr = []
    for b in training_xs:
        found = False
        for e in all_records_by_plugin["v1_neural_network_ridge_moe"]:
            assert isinstance(b, str)
            if e.benchmark == b:
                arr.append(e.speedup)
                found = True
                break
        assert found
    plt.bar(range(len(training_xs)), arr)
    plt.savefig(fname=os.path.join(plot_dir, "box-plot-of-hyperparams-in-training-with-moe.pdf"))

    plt.figure()
    box_plot_of_speedups(test_xs, test_speedups, name="Test")
    plt.title("Performance of CMoE L2 Compared to Uni-Model Policies in Test Benchmarks.")
    arr = []
    for b in test_xs:
        found = False
        for e in all_records_by_plugin["v1_neural_network_ridge_moe"]:
            assert isinstance(b, str)
            if e.benchmark == b:
                arr.append(e.speedup)
                found = True
                break
        assert found
    plt.bar(range(len(test_xs)), arr)
    plt.savefig(fname=os.path.join(plot_dir, "box-plot-of-hyperparams-in-test-with-moe.pdf"))

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


def box_plot_of_speedups_for_hyper_with_moe(
        plot_dir,
        all_records_by_plugin, max_unimodel_speedup_by_bench, min_unimodel_speedup_by_bench):
    regrets = []
    # uni_model_plugins = [p for p in ALL_PLUGINS if "moe" not in p and p != "nothing"]
    model_plugins = [p for p in ALL_PLUGINS if p != "nothing"]

    for p in model_plugins:
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
    regret_third_quartile = np.array([np.percentile(s, 75) for s in regrets])
    regret_first_quartile = np.array([np.percentile(s, 25) for s in regrets])
    regret_max = np.array([max(s) for s in regrets])
    regret_min = np.array([min(s) for s in regrets])

    plt.figure()
    width = 0.35
    l1 = plt.bar(np.arange(len(ticks)), regret_third_quartile - regret_first_quartile, width=width)
    l2 = plt.bar(np.arange(len(ticks)) + width, regret_max - regret_min, width=width)
    plt.xticks(np.arange(len(ticks)), ticks, rotation="60")
    plt.grid()
    plt.title("Interquartile Range and Range of Regrets")
    plt.legend([l1, l2], ["Interquartile Range", "Range"])
    plt.tight_layout()
    plt.savefig(fname=os.path.join(plot_dir, "regret-iq-bar-by-model-with-moe.pdf"))

    plt.figure()
    plt.title(r"Box Plot of Regret for a Single Model in $\mathcal{B}_{test}$, including CMoE")
    plt.boxplot(regrets.T, vert=False)
    plt.yticks(range(1, 1 + len(names)), ticks)
    plt.xlabel("Regret")
    plt.grid()
    plt.tight_layout()
    plt.savefig(fname=os.path.join(plot_dir, "test-regret-by-model-with-moe.pdf"))

    plt.figure()
    plt.title(r"Box Plot of Speedup for a Single Model in $\mathcal{B}_{test}$, including CMoE")

    print names[-2]
    print "Median:", np.median(speedups[-2, :])
    print "Max:", np.max(speedups[-2, :])

    plt.boxplot(speedups.T, vert=False)
    plt.yticks(range(1, 1 + len(names)), ticks)
    plt.xlabel("Speedup")
    plt.tight_layout()
    plt.grid()
    plt.savefig(fname=os.path.join(plot_dir, "test-speedup-by-model-with-moe.pdf"))

def box_plot_of_speedups_for_hyper_training(
        plot_dir,
        all_records_by_plugin, max_unimodel_speedup_by_bench, min_unimodel_speedup_by_bench):
    regrets = []
    # uni_model_plugins = [p for p in ALL_PLUGINS if "moe" not in p and p != "nothing"]
    uni_model_plugins = [p for p in ALL_PLUGINS if p != "nothing"]

    for p in uni_model_plugins:
        my_regret = []
        my_speedup = []
        entries = all_records_by_plugin[p]

        for e in entries:
            speedup = e.speedup
            b = e.benchmark
            if b in py_common.INITIAL_EXPERIMENTS:
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
    plt.title(r"Box Plot of Regret for a Single Model in $\mathcal{B}_{training}$")
    plt.boxplot(regrets.T, vert=False)
    plt.yticks(range(1, 1 + len(names)), ticks)
    plt.xlabel("Regret")
    plt.grid()
    plt.tight_layout()
    plt.savefig(fname=os.path.join(plot_dir, "training-regret-by-model-with-moe.pdf"))

    plt.figure()
    plt.title(r"Box Plot of Speedup for a Single Model in $\mathcal{B}_{training}$")
    plt.boxplot(speedups.T, vert=False)
    plt.yticks(range(1, 1 + len(names)), ticks)
    plt.xlabel("Speedup")
    plt.tight_layout()
    plt.grid()
    plt.savefig(fname=os.path.join(plot_dir, "training-speedup-by-model-with-moe.pdf"))


def box_plot_of_speedups_for_hyper(
        plot_dir,
        all_records_by_plugin, max_unimodel_speedup_by_bench, min_unimodel_speedup_by_bench):
    regrets = []
    # uni_model_plugins = [p for p in ALL_PLUGINS if "moe" not in p and p != "nothing"]
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
    plt.xlabel("Regret")
    plt.grid()
    plt.tight_layout()
    plt.savefig(fname=os.path.join(plot_dir, "test-regret-by-model.pdf"))

    plt.figure()
    plt.title(r"Box Plot of Speedup for a Single Model in $\mathcal{B}_{test}$")
    plt.boxplot(speedups.T, vert=False)
    plt.yticks(range(1, 1 + len(names)), ticks)
    plt.xlabel("Speedup")
    plt.tight_layout()
    plt.grid()
    plt.savefig(fname=os.path.join(plot_dir, "test-speedup-by-model.pdf"))

    plt.figure()
    plt.title(r"Median Speedup vs Median Regret")
    median_speedups = np.array([np.median(v) for v in speedups])
    median_regrets = np.array([np.median(v) for v in regrets])
    plt.xlabel("Median Speedup")
    plt.ylabel("Median Regret")
    plt.scatter(median_speedups, median_regrets)
    plot_best_fit(median_speedups, median_regrets)
    plt.grid()
    plt.savefig(fname=os.path.join(plot_dir, "median-speedup-vs-median-regret.pdf"))

    plt.figure()
    plt.title(r"Median Speedup vs InterQuartile Range of Speedup")
    median_speedups = np.array([np.median(v) for v in speedups])
    first_quartiles = np.array([np.percentile(v, 25) for v in speedups])
    third_quartiles = np.array([np.percentile(v, 75) for v in speedups])
    plt.scatter(median_speedups, (third_quartiles - first_quartiles))
    plot_best_fit(median_speedups, (third_quartiles - first_quartiles))
    plt.xlabel("Median Speedup")
    plt.ylabel("Interquartile Range of Speedup")
    plt.grid()
    plt.savefig(fname=os.path.join(plot_dir, "median-speedup-vs-interquartile-speedup.pdf"))

    plt.figure()
    plt.title(r"Median Regret vs Interquartile Range of Speedup")
    median_speedups = np.array([np.median(v) for v in regrets])
    first_quartiles = np.array([np.percentile(v, 25) for v in speedups])
    third_quartiles = np.array([np.percentile(v, 75) for v in speedups])
    plt.scatter(median_speedups, (third_quartiles - first_quartiles))
    plot_best_fit(median_speedups, (third_quartiles - first_quartiles))
    plt.xlabel("Median Regret")
    plt.ylabel("Interquartile Range of Speedup")
    plt.grid()
    plt.savefig(fname=os.path.join(plot_dir, "median-regret-vs-interquartile-speedup.pdf"))

    plt.figure()
    plt.title(r"Interquartile Range of Speedup vs Interquartile Range of Regret")
    median_speedups = np.array([np.median(v) for v in regrets])
    first_quartiles_regret = np.array([np.percentile(v, 25) for v in regrets])
    third_quartiles_regret = np.array([np.percentile(v, 75) for v in regrets])

    first_quartiles_speedup = np.array([np.percentile(v, 25) for v in speedups])
    third_quartiles_speedup = np.array([np.percentile(v, 75) for v in speedups])

    d_speedup = third_quartiles_speedup - first_quartiles_speedup
    d_regret = third_quartiles_regret - first_quartiles_regret

    plt.scatter(d_speedup, d_regret)
    plot_best_fit(d_speedup, d_regret)
    plt.xlabel("Interquartile Range of Speedup")
    plt.ylabel("Interquartile Range of Regret")
    plt.grid()
    plt.savefig(fname=os.path.join(plot_dir, "median-regret-vs-interquartile-regret.pdf"))

    plt.figure()
    plt.title(r"Median Regret vs Interquartile Range of Regret")
    median_speedups = np.array([np.median(v) for v in regrets])
    first_quartiles = np.array([np.percentile(v, 25) for v in regrets])
    third_quartiles = np.array([np.percentile(v, 75) for v in regrets])
    plt.scatter(median_speedups, (third_quartiles - first_quartiles))
    plot_best_fit(median_speedups, (third_quartiles - first_quartiles))
    plt.xlabel("Median Regret")
    plt.ylabel("Interquartile Range of Regret")
    plt.grid()
    plt.savefig(fname=os.path.join(plot_dir, "median-regret-vs-interquartile-regret.pdf"))

    unsorted_model_accuracies = {}
    for plugin_name in ALL_PLUGINS:
        if "moe" not in plugin_name and plugin_name != "nothing":
            unsorted_model_accuracies[plugin_name] = get_model_accuracy(plugin_name)
    stats = []
    model_accuracies = []
    median_regrets = np.array([np.median(v) for v in regrets])
    median_speedups = np.array([np.median(v) for v in speedups])
    prop_idk = []
    prop_inline = []
    prop_apply = []

    for i, name in enumerate(names):
        stat = unsorted_model_accuracies[name]
        stats.append(stat)
        model_accuracies.append(stats[-1].accuracy)
        prop_idk.append(stats[-1].doesnt_matter)
        prop_inline.append(stats[-1].inline)
        prop_apply.append(stats[-1].apply)

        print "%.3f,%.3f,%.3f,%.3f,%.3f" % (stat.inline, stat.apply, stat.doesnt_matter, stat.accuracy, median_regrets[i])

    a =  np.array([x for x in model_accuracies])
    b =  np.array([x for x in prop_idk])
    c =  np.array([x for x in prop_inline])
    d =  np.array([x for x in prop_apply])
    X = np.zeros((len(a), 6))

    names = [
            "Accuracy",
            "Proportion of I don't Know",
            "Proportion of Inline",

            "Product of Accuracy and Proportion of I Don't Know",
            "Product of Accuracy and Proportion of Inline",
            "Product of Proportions of I Don't Know and Inline",
            ]

    X[:, 0]  = a
    X[:, 1]  = b
    X[:, 2]  = c
    X[:, 3]  = a * b
    X[:, 4]  = a * c
    X[:, 5]  = b * c

    from sklearn.linear_model import LinearRegression
    lm = LinearRegression(normalize=True)
    lm.fit(X, median_regrets)
    print "r^2 = ", lm.score(X, median_regrets)

    matplotlib.rc("font", size=8)
    plt.figure(figsize=(8.27, 11.69))

    for i in range(X.shape[1]):
        plt.subplot(3, 2, i + 1)
        plt.title("%s vs Median Regret" % names[i])
        plt.scatter(X[:, i],   median_regrets)
        plot_best_fit(X[:, i], median_regrets)
        plt.grid()
        plt.xlabel(names[i])
        plt.tight_layout()
        plt.ylabel("Median Regret")

    plt.savefig(fname=os.path.join(plot_dir, "regret-vs-model-features.pdf"))


def plot_best_fit(xs, ys):
    slope, intercept, r_value, p_value, std_err = scipy.stats.linregress(
            xs, ys)

    eqn = r"$y = %.4f x + %.4f (r^2 = %.4f)$" % (slope, intercept, r_value ** 2)
    diff = (max(xs) - min(xs)) / 20.0
    xs = [(min(xs) + diff * i) for i in range(0, 21)]
    ys = [slope * x + intercept for x in xs]
    plt.plot(xs, ys, "r", label=eqn)
    plt.legend()


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
