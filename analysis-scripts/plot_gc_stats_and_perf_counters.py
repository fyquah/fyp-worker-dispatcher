import csv
import math
import os
import sys

import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA

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

fields = [
        gc_fields,
        first_perf_fields,
        second_perf_fields,
        other_fields
]

def plot_set_of_graphs(X, Y, field_names, title, filename):
    nrows = math.ceil((len(field_names)) / 4)
    ncols = 4

    fig, axes = plt.subplots(nrows, ncols)
    fig.suptitle(title)
    assert len(field_names) == X.shape[1]

    for index, field in enumerate(field_names):
        row = math.floor(index / ncols)
        col = index % ncols
        ax = axes[row, col]
        ax.scatter(X[:, index], Y, color='r', marker='o', s=3)
        ax.set_title(field)
        ax.grid()

    fig.tight_layout(rect=[0, 0.03, 1, 0.95])
    fig.savefig(fname=os.path.join("pca-data", filename) + ".pdf", format='pdf')


def plot_pca_scatter(X, title, filename):
    pca = PCA(n_components=2, svd_solver='full')
    pca.fit(X)
    print(pca.explained_variance_ratio_)
    transformed = pca.transform(X)

    plt.title(title)
    plt.scatter(transformed[:, 0], transformed[:, 1], color='r', marker='x', s=3)
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    plt.savefig(fname=os.path.join("pca-data", filename) + ".pdf", format='pdf')


def plot(filename):
    with open(filename, "r") as f:
        data = []
        for line in csv.reader(f):
            assert len(line) == 1 + 2 + 1 + len(gc_fields) + len(first_perf_fields) + len(second_perf_fields)
            data.append([float(x) for x in line[2:]])
    data = np.array(data)
    Y = data[:, -1]

    plot_set_of_graphs(
            X=data[:, :len(gc_fields)],
            Y=Y,
            field_names=gc_fields,
            title="GC Stats vs Execution Time",
            filename=os.path.splitext(os.path.basename(filename))[0] + "_gc_stats")
    plot_set_of_graphs(
            X=data[:, len(gc_fields):len(gc_fields) + 9],
            Y=Y,
            field_names=first_perf_fields[:9],
            title="Perf Stats (w/o normalisation) vs Execution Time",
            filename=os.path.splitext(os.path.basename(filename))[0] + "_raw_perf_stats")
    cycles = data[:, len(gc_fields) + 9:len(gc_fields) + 10]
    instructions_count = data[:, -1:]
    plot_set_of_graphs(
            X=data[:, len(gc_fields):len(gc_fields) + 9] / instructions_count,
            Y=Y,
            field_names=first_perf_fields[:9],
            title="Perf Stats (Normalised over Cycles) vs Execution Time",
            filename=os.path.splitext(os.path.basename(filename))[0] + "_norm_cycles_perf_stats")

    pca_indices = []
    for i in range(len(gc_fields)):
        pca_indices.append(i)
    for i in range(9):
        pca_indices.append(i + len(gc_fields))

    plot_pca_scatter(
            X=data[:, pca_indices],
            title="PCA Scatter",
            filename=os.path.splitext(os.path.basename(filename))[0] + "_pca")

    # perf_features = data[:, len(gc_fields):(len(gc_fields) + 9)]


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
