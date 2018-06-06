import argparse
import collections
import sys
import math
import cPickle as pickle

import scipy
import scipy.stats

import sexpdata
import matplotlib
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.backends import backend_pdf

import os

import numpy as np
import inlining_tree
import py_common

from sklearn.decomposition import PCA
from sklearn.linear_model import LogisticRegression
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis as LDA
from sklearn.metrics import roc_curve
from sklearn.cluster import KMeans

Features = collections.namedtuple("Features", ["int_features", "bool_features", "numeric_features"])
Reward = collections.namedtuple("Reward", ["inline", "no_inline"])
DualReward = collections.namedtuple("DualReward", ["long_term", "immediate"])

option_of_sexp = inlining_tree.option_of_sexp

B = 5.0

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

    def parse_feature_list(sexp, f):
        return [(inlining_tree.unpack_atom(k), f(inlining_tree.unpack_atom(v))) for k, v in sexp]

    def parse_bool(s):
        if s == "true":
            return True
        elif s == "false":
            return False
        else:
            assert False


    def parse_features(sexp):
        m = inlining_tree.sexp_to_map(sexp)
        int_features = parse_feature_list(m["int_features"], f=int)
        numeric_features = parse_feature_list(m["numeric_features"], f=float)
        bool_features = parse_feature_list(m["bool_features"], f=parse_bool)
        return Features(int_features=int_features, bool_features=bool_features, numeric_features=numeric_features)

    assert isinstance(sexp, list)
    return [(parse_features(a), option_of_sexp(b, f=parse_reward)) for (a, b) in sexp]

def fmap(x, f):
    if x is not None:
        return f(x)
    else:
        return None


# xs.append(sgn(a) * (1 + math.log(abs(a))))

def plot_best_fit(xs, ys):
    slope, intercept, r_value, p_value, std_err = scipy.stats.linregress(
            xs, ys)
    eqn = "%.4f x + %.4f" % (slope, intercept)
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


def plot_immediate_reward_histrogram(all_data):
    xs = []

    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            xs.append(d.inline.long_term)

    plt.title("Immediate Reward Histogram (%d samples)" % len(xs))
    plt.hist(xs, bins=300)
    plt.xlabel("Long Term Reward")
    plt.ylabel("Normalised Frequency")
    plt.grid()


def plot_long_term_and_no_inline_correlation(all_data):
    xs = []
    ys = []
    for d in all_data:
        if d.inline is not None and d.no_inline is not None:
            xs.append(d.inline.long_term)
            ys.append(d.no_inline)

    plt.title("Long Term vs Termination Reward")
    plt.scatter(xs, ys, marker="x")
    plt.xlabel("Long Term Reward")
    plt.ylabel("Termination Reward")
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
    plt.hist(xs, normalised=True, bins=50)
    plt.xlabel("Immediate Reward")
    plt.ylabel("Normalised Frequency")
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


def plot_pca(features, labels, title, fname, legend, num_classes):
    pca = PCA(n_components=2)
    pca.fit(features)
    transformed = pca.transform(features)

    fig = plt.figure()
    plt.title(title)
    plt.xlabel("PCA Component 0")
    plt.ylabel("PCA Component 1")
    ls = []

    for cls in range(num_classes):
        ls.append(plt.scatter(transformed[labels == cls, 0], transformed[labels == cls, 1], marker='x', s=4))
    plt.legend(ls, legend)
    plt.tight_layout()
    plt.grid()
    plt.savefig(fname)


def plot_lda(features, labels, title, fname, legend):
    lda = LDA(n_components=1)
    lda.fit(features, labels)
    pca = PCA(n_components=1)
    pca.fit(features)
    transformed = np.hstack((pca.transform(features), lda.transform(features)))

    fig = plt.figure()
    plt.xlabel("PCA primary component")
    plt.ylabel("LDA component")
    plt.title(title)
    l1 = plt.scatter(transformed[np.array(labels), 0], transformed[np.array(labels), 1], color='r', marker='x', s=4)
    l2 = plt.scatter(transformed[np.logical_not(labels), 0], transformed[np.logical_not(labels), 1], color='b', marker='x', s=4)
    plt.legend([l1, l2], legend)
    plt.tight_layout()
    plt.grid()
    plt.savefig(fname)


def plot_lda_3_classes(features, labels, title, fname, legend):
    lda = LDA(n_components=2)
    lda.fit(features, labels)
    transformed = lda.transform(features)

    fig = plt.figure()
    plt.xlabel("LDA Component 0")
    plt.ylabel("LDA Component 1")
    plt.title(title)
    labels = np.array(labels)

    if transformed.shape[1] >= 2:
        l1 = plt.scatter(transformed[labels == 0, 0], transformed[labels == 0, 1], color='r', marker='x', s=4)
        l2 = plt.scatter(transformed[labels == 1, 0], transformed[labels == 1, 1], color='g', marker='x', s=4)
        l3 = plt.scatter(transformed[labels == 2, 0], transformed[labels == 2, 1], color='b', marker='x', s=4)
    
        plt.legend([l1, l2, l3], legend)
        plt.tight_layout()
        plt.grid()
        plt.savefig(fname)
    

def compute_heatmap(transformed, side_bins):
    x_min = transformed[:, 0].min()
    x_max = transformed[:, 0].max()
    y_min = transformed[:, 1].min()
    y_max = transformed[:, 1].max()

    x_gap = float(x_max - x_min) / side_bins
    y_gap = float(y_max - y_min) / side_bins
    density = np.zeros((side_bins, side_bins), dtype=np.int)

    for (x, y) in transformed:
        i = int(math.floor((y - y_min) / y_gap))
        j = int(math.floor((x - x_min) / x_gap))
        if i == side_bins:
            i = side_bins - 1
        if j == side_bins:
            j = side_bins - 1
        assert 0 <= i and i < side_bins
        assert 0 <= j and j < side_bins
        i = side_bins - 1 - i  # because image increases from top to bottom, but our axes is bottom to top
        density[i, j] += 1
    return density / float(len(transformed))


def plot_pca_3_classes(features, labels, title, fname, legend):
    pca = PCA(n_components=2)
    pca.fit(features)
    transformed = pca.transform(features)

    fig = plt.figure()
    plt.xlabel("PCA Component 0")
    plt.ylabel("PCA Component 1")
    plt.title(title)
    labels = np.array(labels)

    l1 = plt.scatter(transformed[labels == 0, 0], transformed[labels == 0, 1], color='r', marker='x', s=4)
    l2 = plt.scatter(transformed[labels == 1, 0], transformed[labels == 1, 1], color='g', marker='x', s=4)
    l3 = plt.scatter(transformed[labels == 2, 0], transformed[labels == 2, 1], color='b', marker='x', s=4)

    plt.legend([l1, l2, l3], legend)
    plt.tight_layout()
    plt.grid()
    plt.savefig(fname)


def plot_lda_density(features, labels, title, fname):
    lda = LDA(n_components=2)
    lda.fit(features, labels)
    transformed = lda.transform(features)
    heat_map = compute_heatmap(transformed, side_bins=20)

    plt.figure()
    plt.title(title)
    plt.imshow(heat_map)
    plt.savefig(fname)


def plot_pca_density(features, title, fname):
    pca = PCA(n_components=2)
    pca.fit(features)
    transformed = pca.transform(features)

    side_bins = 20
    heat_map = compute_heatmap(transformed, side_bins=side_bins)

    plt.figure()

    xlabels = []
    ylabels = []

    x_min = transformed[:, 0].min()
    x_max = transformed[:, 0].max()
    x_gap = (x_max - x_min) / 20.0

    y_min = transformed[:, 1].min()
    y_max = transformed[:, 1].max()
    y_gap = (y_max - y_min) / 20.0

    for i in range(20):
        xlabels.append("%.2f" % (x_min + (i + 0.5) * x_gap))
        ylabels.append("%.2f" % (y_min + (18.5 - i) * y_gap))

    ax = plt.gca()
    plt.title(title)
    im = ax.imshow(heat_map)
    cbar = ax.figure.colorbar(im, ax=ax)
    ax.set_xticks(np.arange(side_bins))
    ax.set_yticks(np.arange(side_bins))
    ax.set_xticklabels(xlabels, rotation="60")
    ax.set_yticklabels(ylabels)
    plt.savefig(fname)


def main():
    # all_data = []
    # for exp in py_common.INITIAL_EXPERIMENTS:
    #     with open("./report_plots/reward_assignment/data/%s/feature_reward_pair.sexp" % exp, "r") as f:
    #         all_data.extend(parse(sexpdata.load(f)))
    #
    # with open("./report_plots/machine_learning/v1_data.pickle", "wb") as f:
    #     pickle.dump(all_data, f)

    minimal = float(sys.argv[1])
    print "Minimal:", minimal
    with open("./report_plots/machine_learning/v1_data.pickle", "rb") as f:
        all_data = pickle.load(f)

    print "No Information about rewards", len([t for (_, t) in all_data if t is None])
    print "Both inline and termination", len([t for (_, t) in all_data if t is not None and t.inline is not None and t.no_inline is not None])
    print "Just inline", len([t for (_, t) in all_data if t is not None and t.inline is not None and t.no_inline is None])
    print "Just termination", len([t for (_, t) in all_data if t is not None and t.inline is None and t.no_inline is not None])
    print "Total", len(all_data)

    all_numeric_features  = np.zeros((len(all_data), len(all_data[0][0].numeric_features)))
    all_bool_features     = np.zeros((len(all_data), len(all_data[0][0].bool_features)))
    raw_targets           = [b for (_, b) in all_data]

    for i, (features, raw_target) in enumerate(all_data):
        all_numeric_features[i, :] = [a for (_, a) in features.numeric_features]
        all_bool_features[i, :]    = [a for (_, a) in features.bool_features]

    relevant_numeric_features = all_numeric_features[:, (np.std(all_numeric_features,  axis=0) > 0.0001)]
    relevant_bool_features    = all_bool_features[:, (np.mean(all_bool_features,  axis=0) > 0.0001)]

    normalised_numeric_features = (relevant_numeric_features - np.mean(relevant_numeric_features, axis=0))
    normalised_numeric_features = normalised_numeric_features / np.std(relevant_numeric_features, axis=0)

    features = np.concatenate([normalised_numeric_features, relevant_bool_features], axis=1)

    thorough_labels = []
    familiarity_labels = []
    decision_features = []
    decision_labels = []

    assert len(features) == len(raw_targets)

    for i, t in enumerate(raw_targets):
        familiarity_labels.append(
                t is not None
                and t.inline is not None
                and t.no_inline is not None
                and (abs(t.inline.long_term) > minimal or abs(t.no_inline) > minimal)
        )

        if not familiarity_labels[-1]:
            thorough_labels.append(0)
        else:
            decision_features.append(features[i, :])
            decision_labels.append(raw_targets[i].inline.long_term > raw_targets[i].no_inline)
            if not decision_labels[-1]:
                thorough_labels.append(1)
            else:
                thorough_labels.append(2)

    familiarity_features = np.array(features)
    familiarity_labels = np.array(familiarity_labels)

    print "familiarity label mean:", np.mean(familiarity_labels)
    model = LogisticRegression()
    model.fit(features, familiarity_labels)
    print "familiarity model score:", model.score(features, familiarity_labels)
    fpr, tpr, thresholds = roc_curve(familiarity_labels, model.predict_proba(features)[:, 1])

    plot_lda(familiarity_features, familiarity_labels,
            title="LDA Scatter Plot of Familiarity Points (B = %f)" % minimal,
            legend=["Familiar", "Not Familiar"],
            fname="report_plots/machine_learning/familiarity_lda_scatter_plot.pdf")
    plot_pca(familiarity_features, familiarity_labels, num_classes=2,
            title="PCA Scatter Plot of Familiarity Points (B = %f)" % minimal,
            legend=["Familiar", "Not Familiar"],
            fname="report_plots/machine_learning/familiarity_pca_scatter_plot.pdf")

    decision_features = np.array(decision_features)
    decision_labels = np.array(decision_labels)
    print "decision training examples:", len(decision_labels)
    print "decision label mean:", np.mean(decision_labels)
    decision_model = LogisticRegression()
    decision_model.fit(decision_features, decision_labels)
    print "decision model score:", decision_model.score(decision_features, decision_labels)

    # inertias = []
    # for n_clusters in range(2, 20):
    #     kmeans = KMeans(n_clusters=n_clusters)
    #     kmeans.fit(features)
    #     inertias.append(kmeans.inertia_)
    #     print "- kmeans %f clusters: %f" % (n_clusters, kmeans.inertia_)

    # fig = plt.figure()
    # plt.plot(clusters, inertias)
    # plt.show()
    # return

    n_clusters = 3
    kmeans = KMeans(n_clusters=n_clusters)
    kmeans.fit(features)
    clusters = np.array(kmeans.labels_, dtype=np.int32)

    plot_pca(decision_features, decision_labels, num_classes=2,
            title="PCA Scatter Plot of Decision Points (B = %f)" % minimal,
            legend=["Inline", "Terminate"],
            fname="report_plots/machine_learning/decision_pca_scatter_plot.pdf")

    plot_pca(features, kmeans.labels_, num_classes=kmeans.n_clusters,
            title="PCA Scatter Plot of Decision Points (B = %f)" % minimal,
            legend=[str(x) for x in range(kmeans.n_clusters)],
            fname="report_plots/machine_learning/decision_cluster_scatter_plot.pdf")

    for i in range(100):
        fname = "report_plots/machine_learning/all_lda_scatter_plot-cluster_%d.pdf" % i
        if os.path.exists(fname):
            os.unlink(fname)

    for i in range(n_clusters):
        cluster_features = features[clusters == i, :]
        cluster_labels = np.array(thorough_labels)[clusters == i]
        print "Cluster %d: %d points" % (i, len(cluster_features))
        plot_lda_3_classes(cluster_features, cluster_labels,
                title="LDA Scatter Plot of Decision Points (B = %f, cluster = %d) [%d points]" % (minimal, i, len(cluster_features)),
                legend=["I Don't Know", "Inline", "Terminate"],
                fname="report_plots/machine_learning/all_lda_scatter_plot-cluster_%d.pdf" % i)
        plot_pca_3_classes(cluster_features, cluster_labels,
                title="PCA Scatter Plot of Decision Points (B = %f, cluster = %d) [%d points]" % (minimal, i, len(cluster_features)),
                legend=["I Don't Know", "Inline", "Terminate"],
                fname="report_plots/machine_learning/all_pca_scatter_plot-cluster_%d.pdf" % i)

    plot_lda_3_classes(features, thorough_labels,
            title="LDA Scatter Plot of Decision Points (B = %f)" % minimal,
            legend=["I Don't Know", "Inline", "Terminate"],
            fname="report_plots/machine_learning/all_lda_scatter_plot.pdf")
    plot_lda_density(features, thorough_labels,
            title="Heat Map of LDA Scatter Plot",
            fname="report_plots/machine_learning/all_lda_heat_map.pdf")

    plot_pca_3_classes(features, thorough_labels,
            title="PCA Scatter Plot of Decision Points (B = %f)" % minimal,
            legend=["I Don't Know", "Inline", "Terminate"],
            fname="report_plots/machine_learning/all_pca_scatter_plot.pdf")
    plot_pca_density(features,
            title="Heat Map of PCA Scatter Plot",
            fname="report_plots/machine_learning/all_pca_heat_map.pdf")


if __name__ == "__main__":
    main()
