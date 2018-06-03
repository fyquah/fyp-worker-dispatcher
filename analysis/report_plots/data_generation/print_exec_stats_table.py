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

from plot_exec_stats import CACHE_FILE, merge_list_dist 

def read(algo):
  with open(CACHE_FILE % algo, 'rb') as f:
    (exec_times, initial_exec_times) = pickle.load(f)
  return (exec_times, initial_exec_times)


def main():
    rw_exec_times, rw_initial_exec_times= read("random-walk")
    sa_exec_times, sa_initial_exec_times = read("simulated-annealing")
    initial_exec_times = collections.defaultdict(list)
    merge_list_dist(initial_exec_times, sa_initial_exec_times)
    merge_list_dist(initial_exec_times, rw_initial_exec_times)

    arr = []
    for exp_name in py_common.INITIAL_EXPERIMENTS:
        ori = min(initial_exec_times[exp_name])
        sa = min(sa_exec_times[exp_name])
        rw = min(rw_exec_times[exp_name])
        s = "%s, %.3f, %.3f (%.3f%%), %.3f (%.3f%%)" % (exp_name, ori, sa, 100 * (ori - sa) / ori, rw, 100 * (ori - rw) / ori)

        k = max(100 * (ori - rw) / ori, 100 * (ori - sa) / ori)
        arr.append((k, s))
    arr.sort(key=lambda (a, _) : -a)

    for _, s in arr:
        print s




if __name__ == "__main__":
    main()
