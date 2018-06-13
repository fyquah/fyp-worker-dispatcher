import argparse
import collections
import sys
import math
import scipy
import scipy.stats
from sklearn.metrics import r2_score
import numpy as np

import sexpdata
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.backends import backend_pdf

import inlining_tree


Reward = collections.namedtuple("Reward", ["inline", "no_inline"])
DualReward = collections.namedtuple("DualReward", ["long_term", "immediate"])

option_of_sexp = inlining_tree.option_of_sexp

B = 100.0

def sgn(x):
    if x < 0:
        return -1
    else:
        return 1


def parse(sexp):
    def parse_dual_reward(sexp):
        m = inlining_tree.sexp_to_map(sexp)
        return DualReward(
                long_term=float(m["long_term"]),
                immediate=float(m["immediate"]))

    def parse_reward(sexp):
        m = inlining_tree.sexp_to_map(sexp)
        inline = option_of_sexp(m["inline"], f=parse_dual_reward)
        no_inline = option_of_sexp(m["no_inline"], f=float)
        return Reward(inline=inline, no_inline=no_inline)

    assert isinstance(sexp, list)
    return [parse_reward(x) for x in sexp]

def fmap(x, f):
    if x is not None:
        return f(x)
    else:
        return None


# xs.append(sgn(a) * (1 + math.log(abs(a))))

def plot_best_fit(xs, ys):
    slope, intercept, r_value, p_value, std_err = scipy.stats.linregress(
            xs, ys)

    eqn = "y = %.4f x + %.4f (r^2 = %.4f)" % (slope, intercept, r_value ** 2)
    diff = (max(xs) - min(xs)) / 20.0
    xs = [(min(xs) + diff * i) for i in range(0, 21)]
    ys = [slope * x + intercept for x in xs]
    plt.plot(xs, ys, "r", label=eqn)
    plt.legend()

def plot_immediate_and_long_term_correlation(all_data):
    xs = []
    ys = []
    for d in all_data:
        if d.inline is not None:
            xs.append(d.inline.immediate)
            ys.append(d.inline.long_term)

    plt.title("Immediate Reward vs Long Term Reward")
    plt.scatter(xs, ys, marker="x")
    plt.xlabel("Immediate Reward")
    plt.ylabel("Long Term Reward")
    plt.grid()
    plt.scatter(xs, ys, marker="x")
    plot_best_fit(xs, ys)


def plot_immediate_and_no_inline_correlation(all_data):
    xs = []
    ys = []
    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            xs.append(d.inline.immediate)
            ys.append(d.no_inline)

    plt.title("Immediate vs Termination Reward")
    plt.scatter(xs, ys, marker="x")
    plt.xlabel("Immediate Reward")
    plt.ylabel("Termination Reward")
    plt.grid()
    plot_best_fit(xs, ys)


def plot_long_term_reward_histogram(all_data):
    xs = []

    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            xs.append(d.inline.long_term)

    plt.title("Long Term Reward Histogram (%d samples)" % len(xs))
    plt.hist(xs, bins=300)
    plt.xlabel("Long Term Reward")
    plt.ylabel("Frequency")
    plt.grid()


def plot_no_inline_reward_histogram(all_data):
    xs = []

    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            xs.append(d.no_inline)

    plt.title("Termination Reward Histogram (%d samples)" % len(xs))
    plt.hist(xs, bins=300)
    plt.xlabel("Long Term Reward")
    plt.ylabel("Frequency")
    plt.grid()


def plot_long_term_and_no_inline_correlation(all_data):
    xs = []
    ys = []
    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            xs.append(d.inline.long_term)
            ys.append(d.no_inline)

    plt.title("Long Term vs Termination Compensation (%d samples)" % len(xs))
    plt.scatter(xs, ys, marker="x")
    plt.xlabel("Long Term Reward")
    plt.ylabel("Termination Compensation")
    plt.grid()
    plot_best_fit(xs, ys)


def plot_long_term_and_no_inline_correlation_non_trivial(all_data):
    xs = []
    ys = []
    for d in all_data:
        if d.inline is not None and d.no_inline is not None and (abs(d.inline.long_term) > 0.1 or abs(d.no_inline) > 0.1):
            xs.append(d.inline.long_term)
            ys.append(d.no_inline)

    plt.title("Long Term vs Termination Compensation (Non-trivial Nodes) (%d samples)" % len(xs))
    plt.scatter(xs, ys, marker="x")
    plt.xlabel("Long Term Reward")
    plt.ylabel("Termination Compensation")
    plt.grid()
    plot_best_fit(xs, ys)


def plot_immediate_reward_log_histrogram(all_data):

    def f(x):
        return sgn(x) * math.log(1 + abs(x))

    xs = []

    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            xs.append(f(d.inline.immediate))

    plt.title("Imm. Reward Log-Space Histogram")
    plt.hist(xs, bins=50)
    plt.xlabel("Immediate Reward")
    plt.ylabel("Frequency")
    plt.grid()


def remove_annomalises(all_data):
    ret = []
    for d in all_data:
        if (fmap(d.inline, f=lambda x : abs(x.immediate) > B)
                or fmap(d.inline, f=lambda x : abs(x.long_term) >B)
                or fmap(d.no_inline, f=lambda x : abs(x) > B)):
            pass
        else:
            ret.append(d)
    return ret

import csv

def parse_time(s):
    if s[-1] == 's':
        return float(s[:-1])
    assert False


def get_times_from_file(fname):
    times = []
    with open(fname, "rb") as f:
        rdr = csv.reader(f)
        for line in rdr:
            times.extend([parse_time(x) for x in line[4:]])
    return times


def geometric_mean(xs):
    a = set(xs) - {min(xs), max(xs)}
    m = 1.0
    for t in a:
        m *= t
    return m ** (1.0 / len(a))


def main():
    decay_factor = float(sys.argv[1])
    benefit_function = sys.argv[2]
    search_log_file = (
            "out-v0-reproduce-relabel/bdd/lasso/decay-%s-benefit-%s-lasso-factor-auto/search_log.csv"
            % (("%.6f" % decay_factor), benefit_function))
    results_files = []

    font = {'size'   : 8}
    matplotlib.rc('font', **font)

    all_alpha = []
    all_r_squared  = []
    all_sum_abs = []
    all_num_non_zero = []
    all_times = []
    headers = None

    with open(search_log_file, "rb") as f:
        rdr = csv.reader(f)

        for i, row in enumerate(rdr):
            if i == 0:
                headers = row
            else:
                alpha, r_squared, sum_abs, num_non_zero = [float(x) for x in row]

                results_file = ("../results/bdd/lasso-with-alpha-v0-reproduce-relabel/decay-%s-benefit-%s-lasso-factor-%s.csv"
                        % (("%.6f" % decay_factor), benefit_function, row[0]))
                print results_file
                try:
                    times = get_times_from_file(results_file)

                    all_times.append(geometric_mean(times))
                    all_alpha.append(alpha)
                    all_r_squared.append(r_squared)
                    all_sum_abs.append(sum_abs)
                    all_num_non_zero.append(num_non_zero)
                except Exception as e:
                    print e


    all_alpha.reverse()
    all_r_squared.reverse()
    all_sum_abs.reverse()
    all_num_non_zero.reverse()
    all_times.reverse()
    print min(all_times)

    all_sum_abs = np.array(all_sum_abs)
    all_num_non_zero = np.array(all_num_non_zero)

    ax = plt.subplot(511)
    ax.set_xscale("log", nonposx='clip')
    plt.plot(all_alpha, all_r_squared)

    ax = plt.subplot(512)
    ax.set_xscale("log", nonposx='clip')
    plt.plot(all_alpha, all_sum_abs)

    ax = plt.subplot(513)
    ax.set_xscale("log", nonposx='clip')
    plt.plot(all_alpha, all_num_non_zero)

    ax = plt.subplot(514)
    ax.set_xscale("log", nonposx='clip')
    plt.plot(all_alpha, all_r_squared / (1 + all_num_non_zero))

    ax = plt.subplot(515)
    ax.set_xscale("log", nonposx='clip')
    plt.plot(all_alpha, all_times)
    plt.show()


if __name__ == "__main__":
    main()
