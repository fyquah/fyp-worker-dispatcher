import os
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
    exp_name = sys.argv[1]
    decay_factor = float(sys.argv[2])
    benefit_function = sys.argv[3]
    search_log_file = (
            "report_plots/reward_assignment/data/lasso/%s/decay-%s-benefit-%s-lasso-factor-auto/search_log.csv"
            % (exp_name, ("%.6f" % decay_factor), benefit_function))
    results_files = []

    font = {'size'   : 8}
    matplotlib.rc('font', **font)

    all_alpha = []
    all_r_squared  = []
    all_r_squared_validation = []
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
                alpha, r_squared, r_squared_validation, sum_abs, num_non_zero = [float(x) for x in row]

                results_file = ("../results/%s/lasso-with-alpha-v0-reproduce-relabel/decay-%s-benefit-%s-lasso-factor-%s.csv"
                        % (exp_name, ("%.6f" % decay_factor), benefit_function, row[0]))
                try:
                    times = get_times_from_file(results_file)

                    if len(times) == 0:
                        continue

                    all_times.append(geometric_mean(times))
                    all_alpha.append(alpha)
                    all_r_squared.append(r_squared)
                    all_r_squared_validation.append(r_squared_validation)
                    all_sum_abs.append(sum_abs)
                    all_num_non_zero.append(num_non_zero)
                except IOError as e:
                    print e

    all_alpha.reverse()
    all_r_squared.reverse()
    all_r_squared_validation.reverse()
    all_sum_abs.reverse()
    all_num_non_zero.reverse()
    all_times.reverse()

    all_slowdown = []
    for t in all_times:
        all_slowdown.append((t - min(all_times)) / min(all_times))

    best_alpha_index = 0
    for i, alpha in enumerate(all_alpha):
        if all_r_squared_validation[i] > all_r_squared_validation[best_alpha_index]:
            best_alpha_index = i
    best_alpha = all_alpha[best_alpha_index]

    all_sum_abs = np.array(all_sum_abs)
    all_num_non_zero = np.array(all_num_non_zero)

    labelsize = 10
    font = {'size'   : 12}
    matplotlib.rc('font', **font)
    matplotlib.rc('text', usetex=True)
    matplotlib.rcParams.update({'font.size': 12, "font.family": "serif"})
    matplotlib.rc('xtick', labelsize=labelsize)
    matplotlib.rc('ytick', labelsize=labelsize)
    matplotlib.rc('figure', figsize=(11.69,17.27))

    ax = plt.subplot(411)
    ax.set_xscale("log", nonposx='clip')
    plt.title(r"$\alpha$ vs $r^2$")
    plt.xlim(max(all_alpha), min(all_alpha))
    plt.axvline(x=best_alpha, color="g", linestyle="--")
    plt.grid()
    plt.plot(all_alpha, all_r_squared, marker='x', label="training")
    plt.plot(all_alpha, all_r_squared_validation, marker='x', color="r", label="validation")

    ax = plt.subplot(412)
    ax.set_xscale("log", nonposx='clip')
    plt.xlim(max(all_alpha), min(all_alpha))
    plt.title(r"$\alpha$ vs $\sum{|w|}$")
    plt.grid()
    plt.axvline(x=best_alpha, color="g", linestyle="--")
    plt.plot(all_alpha, all_sum_abs, marker='x')

    ax = plt.subplot(413)
    ax.set_xscale("log", nonposx='clip')
    plt.xlim(max(all_alpha), min(all_alpha))
    plt.title(r"$\alpha$ vs $\sum{I\{|w| > 0\}}$")
    plt.grid()
    plt.axvline(x=best_alpha, color="g", linestyle="--")
    plt.plot(all_alpha, all_num_non_zero, marker='x')

    ax = plt.subplot(414)
    ax.set_xscale("log", nonposx='clip')
    plt.xlim(max(all_alpha), min(all_alpha))
    plt.title(r"$\alpha$ vs $T_{exec}(s)$")
    plt.grid()
    plt.axvline(x=best_alpha, color="g", linestyle="--")
    plt.plot(all_alpha, all_times, marker='x')
    plt.ylabel(r"$T_{exec}$(s)")

    plt.suptitle(r"Varying Lasso $\alpha$ [%s] ($\gamma = %.6f$, $f_{benefit} =$ %s)" % (exp_name, decay_factor, benefit_function.replace("_", "\\_")))
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    fname = "report_plots/reward_assignment/plots/lasso/hyperparams/%s-%.6f-%s.pdf" % (exp_name, decay_factor, benefit_function)
    d = os.path.dirname(fname)
    if not os.path.exists(d):
        os.makedirs(d)
    plt.savefig(fname=fname)


if __name__ == "__main__":
    main()
