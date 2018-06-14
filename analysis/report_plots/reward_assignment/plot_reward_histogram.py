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


Reward = collections.namedtuple("Reward", ["inline", "no_inline", "exp"])
DualReward = collections.namedtuple("DualReward", ["long_term", "immediate"])

option_of_sexp = inlining_tree.option_of_sexp

B = 100.0

def sgn(x):
    if x < 0:
        return -1
    else:
        return 1


def parse(sexp, exp):
    def parse_dual_reward(sexp):
        m = inlining_tree.sexp_to_map(sexp)
        return DualReward(
                long_term=float(m["long_term"]),
                immediate=float(m["immediate"]))

    def parse_reward(sexp):
        m = inlining_tree.sexp_to_map(sexp)
        inline = option_of_sexp(m["inline"], f=parse_dual_reward)
        no_inline = option_of_sexp(m["no_inline"], f=float)
        return Reward(inline=inline, no_inline=no_inline, exp=exp)

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

    eqn = r"$y = %.4f x + %.4f (r^2 = %.4f)$" % (slope, intercept, r_value ** 2)
    diff = (max(xs) - min(xs)) / 20.0
    xs = [(min(xs) + diff * i) for i in range(0, 21)]
    ys = [slope * x + intercept for x in xs]
    plt.plot(xs, ys, "r", label=eqn)
    plt.legend()


def plot_zero_value_immediate_hist(all_data):
    xs = []
    ys = []
    for d in all_data:
        if d.inline is not None and abs(d.inline.immediate) < 1e-15 and abs(d.inline.long_term) > 1e-15:
            xs.append(np.log10(abs(d.inline.long_term)))

    xs = np.array(xs)
    plt.title(r"Histogram of long term rewards when $\mathcal{R}_{inline} = 0$ (%d samples)" % len(xs))
    plt.hist(xs)


def plot_immediate_and_long_term_correlation(all_data):
    xs = []
    ys = []
    for d in all_data:
        if d.no_inline is not None and (d.inline is not None and np.log10(abs(d.inline.long_term)) > -10 and np.log10(abs(d.inline.immediate)) > -30):
            xs.append(d.inline.immediate)
            ys.append(d.inline.long_term)

    xs = np.array(xs)
    ys = np.array(ys)

    xs = np.maximum(-50, np.log10(abs(xs)))
    ys = np.maximum(-50, np.log10(abs(ys)))

    plt.title("$log_{10}(|R_{inline}|)$ vs $log_{10}(V^*_{inline})$ (%d samples)" % len(xs))
    plt.scatter(xs, ys, marker="x")
    plt.xlabel("Immediate Reward")
    plt.ylabel("Long Term Reward")
    plt.grid()
    plt.scatter(xs, ys, marker="x")
    # plot_best_fit(xs, ys)


def plot_log_long_term_and_no_inline_correlation(all_data, threshold):

    xs = []
    ys = []
    ctr = 0

    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            x = np.maximum(np.log10(abs(d.inline.long_term)), -50)
            y = np.maximum(np.log10(abs(d.no_inline)), -50)

            if x > threshold and y > threshold:
                xs.append(x)
                ys.append(y)

            if x < -20 and y < -20:
                ctr += 1

    plt.title(r"$log_{10}(|R_{apply}|)$ vs $log_{10}(|V^*_{inline}|)$ (%d samples, $\tau = %s$)" % (len(xs), str(threshold)))
    plt.scatter(xs, ys, marker="x")
    plt.xlabel("Termination Reward")
    plt.ylabel("Immediate Reward")
    plt.grid()
    plot_best_fit(xs, ys)


def plot_reward_difference(all_data, log10_threshold):

    xs = []
    ctr = 0

    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            log10_x = np.maximum(np.log10(abs(d.inline.long_term)), -50)
            log10_y = np.maximum(np.log10(abs(d.no_inline)), -50)

            if log10_x > log10_threshold and log10_y > log10_threshold:
                xs.append(np.log10(abs(d.inline.long_term - d.no_inline)))

    plt.title(r"Hist (%d samples, $\tau = %s$)" % (len(xs), str(log10_threshold)))
    plt.hist(xs)


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


def plot_log_long_term_reward_histogram(all_data):
    xs = []

    for d in all_data:
        if d.inline is not None and abs(d.inline.long_term) > 1e-15:
            xs.append(np.log10(abs(d.inline.long_term)))

    plt.title("$log_{10}(|V^*_{inline}|)$ Histogram (%d samples)" % len(xs))
    plt.hist(xs)
    plt.xlabel("Long Term Reward")
    plt.ylabel("Frequency")
    plt.grid()


def plot_log_no_inline_reward_histogram(all_data):
    xs = []

    for d in all_data:
        if d.inline is not None and d.no_inline is not None and abs(d.no_inline) > 1e-15:
            xs.append(math.log10(abs(d.no_inline)))

    plt.title(r"Histogram of $log_{10}(|R_{apply}|)$ (%d samples)" % len(xs))
    plt.hist(xs)
    plt.xlabel("$log_{10}(|R_{apply}|)$")
    plt.ylabel("Frequency")
    plt.grid()


def plot_long_term_reward_histogram(all_data):
    xs = []

    for d in all_data:
        if d.inline is not None and abs(d.inline.long_term) > 1e-15:
            xs.append(d.inline.long_term)

    plt.title("$V^*_{inline}$ Histogram (%d samples)" % len(xs))
    plt.hist(xs)
    plt.xlabel("Long Term Reward")
    plt.ylabel("Frequency")
    plt.grid()


def plot_no_inline_reward_histogram(all_data):
    xs = []

    for d in all_data:
        if d.inline is not None and d.no_inline is not None and abs(d.no_inline) > 1e-15:
            xs.append(math.log10(abs(d.no_inline)))

    plt.title(r"Histogram of $R_{apply}$ (%d samples)" % len(xs))
    plt.hist(xs)
    plt.xlabel(r"$R_{apply}$")
    plt.ylabel("Frequency")
    plt.grid()


def plot_long_term_and_no_inline_correlation(all_data):
    xs = []
    ys = []
    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            xs.append(d.inline.long_term)
            ys.append(d.no_inline)

    plt.title(r"$V_{inline}^*$ vs $R_{apply}$ (%d samples)" % len(xs))
    plt.scatter(xs, ys, marker="x")
    plt.xlabel("Long Term Reward")
    plt.ylabel("Termination Compensation")
    plt.grid()
    plot_best_fit(xs, ys)


def plot_abs_long_term_and_abs_no_inline_correlation(all_data):
    xs = []
    ys = []
    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            xs.append(abs(d.inline.long_term))
            ys.append(abs(d.no_inline))

    plt.title(r"$|V_{inline}^*|$ vs $|R_{apply}|$ (%d samples)" % len(xs))
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

import py_common


def print_allocation_table(all_data, threshold=-25):  # threshold is empirical
    xs = []
    ys = []
    ctr = 0
    tbl = np.array([[0, 0], [0, 0]])

    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            x = np.maximum(np.log10(abs(d.inline.long_term)), -50)
            y = np.maximum(np.log10(abs(d.no_inline)), -50)

            if x > threshold and y > threshold:
                xs.append(x)
                ys.append(y)

            a = 0 if x < threshold else 1
            b = 0 if y < threshold else 1
            tbl[a, b] += 1

    print "Inline trivial, Apply trivial:", tbl[0, 0]
    print "Inline Significant, Apply trivial", tbl[0, 1]
    print "Inline Trivial, Apply Significant", tbl[1, 0]
    print "Inline Significant, Apply Significant", tbl[1, 1]


def plot_log_learnt_values(all_data, exp=None):
    arr = []
    for r in all_data:
        if r is not None and (exp is None or r.exp == exp):
            if r.inline is not None:
                arr.append(r.inline.immediate)

            if r.no_inline is not None:
                arr.append(r.no_inline)

    arr = [x for x in np.maximum(np.log10(sorted(abs(np.array(arr)))), -50) if x > -40]
    plt.title("Learnt values in [%s] (%d non-zero points)" % (exp, len(arr)))
    plt.grid()
    plt.plot(arr, marker='x')


def main():
    font = {'size'   : 8}
    matplotlib.rc('font', **font)
    matplotlib.rc('text', usetex=True)

    model = sys.argv[1]

    all_data = []
    for exp in py_common.INITIAL_EXPERIMENTS:
        fname = os.path.join("report_plots/reward_assignment/data/", model, exp, "rewards_dump.sexp")
        with open(fname, "r") as f:
            all_data.extend(parse(sexpdata.load(f), exp=exp))

    plots_dir = os.path.join("report_plots/reward_assignment/", "plots", model)
    if not os.path.exists(plots_dir):
        os.makedirs(plots_dir)

    plt.figure()
    plot_long_term_and_no_inline_correlation(all_data)
    plt.savefig(os.path.join(plots_dir, "long-term-vs-termination.pdf"))

    plt.figure()
    plot_abs_long_term_and_abs_no_inline_correlation(all_data)
    plt.savefig(os.path.join(plots_dir, "long-term-magnitude-vs-termination-magnitude.pdf"))

    plt.figure()
    plot_immediate_and_long_term_correlation(all_data)
    plt.savefig(os.path.join(plots_dir, "immediate-vs-long-term.pdf"))

    plt.figure()
    plot_immediate_and_no_inline_correlation(all_data)
    plt.savefig(os.path.join(plots_dir, "immediate-vs-apply.pdf"))

    print_allocation_table(all_data, threshold=-25)  # threshold is empirical

    for threshold in [-10, -20, -30, -40, -50, -60]:
        plt.figure()
        plot_log_long_term_and_no_inline_correlation(all_data, threshold=threshold)
        plt.savefig(os.path.join(plots_dir, "log-long-term-vs-log-apply-threshold-%d.pdf" % threshold))

    plt.figure()
    plot_log_long_term_reward_histogram(all_data)
    plt.savefig(os.path.join(plots_dir, "log-long-term-histogram.pdf"))

    plt.figure()
    plot_log_no_inline_reward_histogram(all_data)
    plt.savefig(os.path.join(plots_dir, "log-apply-reward-histogram.pdf"))

    plt.figure()
    plot_long_term_reward_histogram(all_data)
    plt.savefig(os.path.join(plots_dir, "long-term-histogram.pdf"))

    plt.figure()
    plot_no_inline_reward_histogram(all_data)
    plt.savefig(os.path.join(plots_dir, "apply-reward-histogram.pdf"))

    plt.figure()
    plot_zero_value_immediate_hist(all_data)
    plt.savefig(os.path.join(plots_dir, "zero-value-immediate-histogram.pdf"))

    plt.figure()
    plot_reward_difference(all_data, log10_threshold=-25)
    plt.savefig(os.path.join(plots_dir, "log-abs-reward-diff-histogram.pdf"))


    for exp in py_common.INITIAL_EXPERIMENTS:
        plt.figure()
        plot_log_learnt_values(all_data, exp)
        plt.savefig(os.path.join(plots_dir, "log-learnt-values-%s.pdf" % exp))

    plt.figure()
    plot_log_learnt_values(all_data)
    plt.savefig(os.path.join(plots_dir, "log-learnt-values.pdf"))



if __name__ == "__main__":
    main()
