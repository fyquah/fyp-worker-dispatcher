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

from extract_data_from_experiments import remove_prefix, script_name_to_exp_name, iterate_rundirs
from inlining_tree import sexp_to_map, geometric_mean, unpack_atom

def read_log_file(filename):
    rundirs = collections.defaultdict(list)
    with open(filename) as batch_f:
        for line in csv.reader(batch_f):
            (script_name, sub_rundir) = line
            if "simulated-annealing" not in script_name:
                continue

            exp_name = script_name_to_exp_name(script_name)
            rundir = "/media/usb" + sub_rundir
            if os.path.exists(rundir):
                rundirs[exp_name].append(rundir)

            rundir = "/media/usb2" + sub_rundir
            if os.path.exists(rundir):
                rundirs[exp_name].append(rundir)

            rundir = "/media/usb3/prod/rundir/" + remove_prefix(sub_rundir, prefix="/home/fyquah/fyp/prod/rundir/")
            if os.path.exists(rundir):
                rundirs[exp_name].append(rundir)

    return rundirs

def parse_time(s):
    s = unpack_atom(s)
    assert isinstance(s, str)
    if s[-1] == "s":
        return float(s[:-1])
    assert False

def read_exec_time(filename):
    if not os.path.exists(filename):
        return None
    assert isinstance(filename, str)
    with open(filename, "r") as f:
        sexp = sexpdata.loads(f.read())
    m = sexp_to_map(sexp)
    return geometric_mean([parse_time(t) for t in m["raw_execution_time"]])

def merge_list_dist(main, copy):
    for k, v in copy.iteritems():
        main[k].extend(v)

CACHE_FILE = os.path.join(
        os.path.dirname(os.path.realpath(__file__)),
        'exec_times.pickle')

def load():
    rundirs = collections.defaultdict(list)

    merge_list_dist(rundirs, read_log_file(
        "../important-logs/batch_executor_before_proof.log"))
    merge_list_dist(rundirs, read_log_file(
        "../important-logs/batch_executor_before_module_paths.log"))
    merge_list_dist(rundirs, read_log_file(
        "../important-logs/batch_executor_before_specialise_for.log"))

    initial_exec_times = {}
    exec_times = {}

    for exp_name in py_common.INITIAL_EXPERIMENTS:
        tasks = list(iterate_rundirs(rundirs[exp_name]))
        exec_times[exp_name] = []
        initial_exec_times[exp_name] = []

        for task in tasks:
            assert isinstance(task[0], str)
            s = read_exec_time(os.path.join(task[0], "execution_stats.sexp"))
            if s is not None:
                exec_times[exp_name].append(s)

                if "initial" in task[0]:
                    initial_exec_times[exp_name].append(s)

    with open(CACHE_FILE, 'wb') as f:
        pickle.dump((exec_times, initial_exec_times), f)
    return (exec_times, initial_exec_times)


def main():
    labelsize = 5
    matplotlib.rc('font',**{
        'family':'sans-serif',
        'sans-serif':['Helvetica'],
    })
    matplotlib.rcParams.update({'font.size': 8, "font.family": "serif"})
    matplotlib.rc('xtick', labelsize=labelsize)
    matplotlib.rc('ytick', labelsize=labelsize)

    all_experiments = py_common.INITIAL_EXPERIMENTS

    if os.path.exists(CACHE_FILE):
        print "Loading from cache"
        with open(CACHE_FILE, 'rb') as f:
            (exec_times, initial_exec_times) = pickle.load(f)
    else:
        print "Not loading from cache"
        (exec_times, initial_exec_times) = load()

    print "Done with reading heavy-lifting"

    nrows = int(math.ceil(len(all_experiments) / 3.0))
    ncols = 3
    fig, axes = plt.subplots(nrows, ncols)
    print "Num rows =", nrows, "; num cols =", ncols

    for r in range(4):
        for c in range(3):
            if r * 3 + c >= len(all_experiments):
                continue

            if nrows == 1:
                ax1 = axes[r * 3 + c]
            else:
                ax1 = axes[r, c]

            exp_name = all_experiments[r * 3 + c]
            plt.title("Histogram of Execution Time in Experiment [%s]" % exp_name)

            sorted_exec_times = sorted(exec_times[exp_name])
            bins = [min(sorted_exec_times)]
            for t in sorted_exec_times:
                if t - bins[-1] > 1.0:  # Implies a separate cluster
                    bins.append(bins[-1] + 0.03)
                    bins.append(t - 0.1)
                elif t - bins[-1] > 0.03:  # Same cluster, but warrants a different bin
                    bins.append(t)
            bins.append(max(sorted_exec_times))

            ax1.set_xlabel('execution time (s)')

            ax1.hist(exec_times[exp_name], color='#1985FF', bins=bins)
            ax1.set_ylabel('frequency in all runs', color='#1985FF')
            ax1.tick_params('y', color='#1985FF')

            ax2 = ax1.twinx()
            ax2.hist(initial_exec_times[exp_name], color='#303030A0', bins=bins)
            ax2.set_ylabel('frequency in initial runs', color='#303030')
            ax2.tick_params('y', color='#303030')

    fig.tight_layout()
    plt.show()

if __name__ == "__main__":
    main()
