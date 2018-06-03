import argparse
import collections
import sys
import math
import scipy
import scipy.stats
from sklearn.metrics import r2_score

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


def main():
    font = {'size'   : 8}
    matplotlib.rc('font', **font)

    with open("report_plots/reward_assignment/rewards_dump.sexp", "r") as f:
        all_data = parse(sexpdata.load(f))

    all_data = remove_annomalises(all_data)

    plt.figure()
    plot_immediate_and_long_term_correlation(all_data)
    plt.savefig("report_plots/reward_assignment/immediate-vs-long-term.pdf")

    plt.figure()
    plot_immediate_and_no_inline_correlation(all_data)
    plt.savefig("report_plots/reward_assignment/immediate-vs-apply.pdf")

    plt.figure()
    plot_long_term_and_no_inline_correlation(all_data)
    plt.savefig("report_plots/reward_assignment/long-term-vs-apply.pdf")

    plt.figure()
    plot_long_term_reward_histogram(all_data)
    plt.savefig("report_plots/reward_assignment/long-term-histogram.pdf")

    plt.figure()
    plot_no_inline_reward_histogram(all_data)
    plt.savefig("report_plots/reward_assignment/apply-reward-histogram.pdf")

    plt.figure()
    plot_long_term_and_no_inline_correlation_non_trivial(all_data)
    plt.savefig("report_plots/reward_assignment/long-term-vs-apply-non-trivial.pdf")

if __name__ == "__main__":
    main()
