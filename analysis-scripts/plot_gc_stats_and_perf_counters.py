import collections
import csv
import math
import os
import sys

import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.cluster import KMeans

PLOT_DIR = "plots/perf-counters-and-gc-stats"

gc_fields = [
        "major_collections",
        "minor_collections",
        "compactions",
        "minor_words",
        "promoted_words",
        "major_words",
        "top_heap_words",
        "heap_words",
        "live_words",
        "free_words",
        "largest_free",
        "fragments",
        "live_blocks",
        "heap_chunks",
]

first_perf_fields = [
        "branches",
        "branch-misses",
        "branches",
        "branch-misses",
        "L1-icache-load-misses",
        "branch-load-misses",
        "branch-loads",
        "iTLB-load-misses",
        "iTLB-loads",
        "cpu-cycles",
        "instructions",
]

second_perf_fields = [
        "branches",
        "branch-misses",
        "L1-icache-load-misses",
        "branch-load-misses",
        "branch-loads",
        "iTLB-load-misses",
        "iTLB-loads",
        "cpu-cycles",
        "instructions",
        "L1-dcache-load-misses",
        "L1-dcache-loads",
        "L1-dcache-stores",
        "LLC-loads",
        "LLC-stores",
        "cache-misses",
        "bus-cycles",
        "cache-references",
        "ref-cycles",
        "dTLB-load-misses",
        "dTLB-loads",
        "dTLB-store-misses",
        "dTLB-stores",
]

other_fields = ["instruction-count"]

def make_range(*, begin, length):
    return list(range(begin, begin + length))

PERF_INDICES = make_range(begin=len(gc_fields) + 2, length=7)
INSTR_COUNT_INDEX = len(gc_fields) + len(first_perf_fields) \
        + len(second_perf_fields)

fields = [
        gc_fields,
        first_perf_fields,
        second_perf_fields,
        other_fields
]

def plot_set_of_graphs(X, Y, field_names, title, filename, marker_colors):
    nrows = math.ceil((len(field_names)) / 4)
    ncols = 4

    fig, axes = plt.subplots(nrows, ncols)
    fig.suptitle(title)
    assert len(field_names) == X.shape[1]

    for field_index, field in enumerate(field_names):
        row = math.floor(field_index / ncols)
        col = field_index % ncols
        ax = axes[row, col]
        baseline_indices = []
        non_baseline_indices = []

        for i, c in enumerate(marker_colors):
            if c == 'r':
                non_baseline_indices.append(i)
            elif c == 'b':
                baseline_indices.append(i)
            else:
                assert False

        ax.scatter(
                X[non_baseline_indices, field_index],
                Y[non_baseline_indices],
                color='r',
                marker='o',
                s=3
        )

        ax.scatter(
                X[baseline_indices, field_index],
                Y[baseline_indices],
                color='b',
                marker='o',
                s=3
        )
        ax.set_title(field)
        ax.grid()

    fig.tight_layout(rect=[0, 0.03, 1, 0.95])
    fig.savefig(fname=os.path.join(PLOT_DIR, filename) + ".pdf", format='pdf')


def plot_pca_scatter(X, field_names, title, filename):
    pca = PCA(n_components=2, svd_solver='full')
    X_normalise = (X - np.mean(X)) / np.std(X)
    pca.fit(X_normalise)
    transformed = pca.transform(X_normalise)

    fig = plt.figure()
    plt.title(title)
    plt.scatter(transformed[:, 0], transformed[:, 1], color='r', marker='x', s=3)
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    plt.grid()
    plt.savefig(fname=os.path.join(PLOT_DIR, filename) + ".pdf", format='pdf')

    eigenvectors_filename = os.path.join(PLOT_DIR, filename) + "eigenvectors.csv"
    with open(eigenvectors_filename, "w") as f:
        writer = csv.writer(f)
        writer.writerow(["Variance"] + field_names)
        for component_idx in range(2):
            component = pca.components_[component_idx]
            variance = pca.explained_variance_ratio_[component_idx]
            assert len(component) == len(field_names)

            writer.writerow([variance] + ["%.3f" % value for value in component])
    return pca

def group_by_colors(data, colors):
    ret = collections.defaultdict(list)
    for datum, color in zip(data, colors):
        ret[color].append(datum)
    return ret


def plot_runtime_histogram(runtimes, title, filename, colors):
    plt.title(title)
    fig, ax1 = plt.subplots()
    data = group_by_colors(runtimes, colors)

    ax1.hist(np.concatenate([data["r"], data["b"]]), color="r")
    ax1.set_ylabel('Frequency of all runs', color='r')

    ax2 = ax1.twinx()
    ax2.hist(data["b"], color="b")
    ax2.set_ylabel("baseline time (s)", color="b", alpha=0.0)

    fig.tight_layout(rect=[0, 0.03, 1, 0.95])
    plt.grid()
    fig.savefig(fname=os.path.join(PLOT_DIR, filename) + ".pdf", format='pdf')


def plot(filename):
    all_data = []
    baseline_data = []
    marker_colors = []

    with open(filename, "r") as f:
        for line in csv.reader(f):
            assert len(line) == 1 + 2 + 1 + len(gc_fields) + len(first_perf_fields) + len(second_perf_fields)
            row = []
            accepted = True
            for x in line[2:]:
                if x == "N/A":
                    accepted = False
                    break
                row.append(float(x))

            if accepted:
                all_data.append(row)
                if "initial" in line[0]:
                    baseline_data.append(row)
                    marker_colors.append("b")
                else:
                    marker_colors.append("r")


    all_data = np.array(all_data)
    Y = all_data[:, -1]

    plot_set_of_graphs(
            marker_colors=marker_colors,
            X=all_data[:, :len(gc_fields)],
            Y=Y,
            field_names=gc_fields,
            title="GC Stats vs Execution Time",
            filename=os.path.splitext(os.path.basename(filename))[0] + "_gc_stats")

    plot_set_of_graphs(
            marker_colors=marker_colors,
            X=all_data[:, PERF_INDICES],
            Y=Y,
            field_names=first_perf_fields[2:9],
            title="Perf Stats (w/o normalisation) vs Execution Time",
            filename=os.path.splitext(os.path.basename(filename))[0] + "_raw_perf_stats")

    instructions_count = all_data[:, INSTR_COUNT_INDEX:INSTR_COUNT_INDEX+1]
    plot_set_of_graphs(
            marker_colors=marker_colors,
            X=all_data[:, PERF_INDICES] / instructions_count,
            Y=Y,
            field_names=first_perf_fields[2:9],
            title="Perf Stats (Normalised over Instructions Count) vs Execution Time",
            filename=os.path.splitext(os.path.basename(filename))[0] + "_norm_inst_count_perf_stats")

    X_pca = np.concatenate([
        all_data[:, :len(gc_fields)],
        all_data[:, PERF_INDICES] / instructions_count
    ], axis=1)
    assert len(X_pca) == len(all_data)
    field_names = []
    for name in gc_fields:
        field_names.append(name)
    for name in first_perf_fields[2:9]:
        field_names.append(name + " (NORM INST)")

    pca = plot_pca_scatter(
            X=X_pca,
            title="PCA Scatter",
            filename=os.path.splitext(os.path.basename(filename))[0] + "_pca",
            field_names=field_names)

    plot_runtime_histogram(
            runtimes=Y,
            colors=marker_colors,
            title="Execution Time Histogram",
            filename=os.path.splitext(os.path.basename(filename))[0] + "_exec_time")


def main():
    labelsize = 5
    matplotlib.rc('font',**{
        'family':'sans-serif',
        'sans-serif':['Helvetica'],
    })
    matplotlib.rcParams.update({'font.size': 8, "font.family": "serif"})
    matplotlib.rc('xtick', labelsize=labelsize)
    matplotlib.rc('ytick', labelsize=labelsize)
    for filename in sys.argv[1:]:
        print("Plotting %s" % filename)
        plot(filename)

if __name__ == "__main__":
    main()
