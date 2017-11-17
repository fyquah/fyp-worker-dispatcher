import argparse
import sys
import csv

import matplotlib
from matplotlib import pylab as plt
from matplotlib.backends.backend_pdf import PdfPages
import pylab

import numpy as np

parser = argparse.ArgumentParser()
parser.add_argument("--title", type=str, dest="title")
parser.add_argument("--output", type=str, dest="output", default=None)

def main():
    args = parser.parse_args()
    arr = []
    for line in csv.reader(sys.stdin):
        arr.append([float(x) for x in line])
    arr = np.array(arr)

    plt.rc("font", family="serif", serif=["Computer Modern"])
    plt.rc("xtick", labelsize=8)
    plt.rc("ytick", labelsize=8)
    plt.rc("axes", labelsize=8)
    # plt.rc("lines", linewidth=1.0, marker="x", markersize=3)
    plt.rc("lines", linewidth=1.0)

    fig, ax = plt.subplots()
    plt.suptitle(args.title)

    plt.subplot(2, 2, 1)
    l1, = plt.plot(arr[:, 0], arr[:, 1], label="current")
    l2, = plt.plot(arr[:, 0], arr[:, 2], label="proposal")
    plt.grid()
    plt.title("Iteration vs Execution time")
    plt.xlabel("iteration")
    plt.ylabel("execution time")
    plt.legend(handles=[l1, l2], loc="best")

    plt.subplot(2, 2, 2)
    l1, = plt.plot(arr[:, 0], arr[:, 3], label="current")
    l2, = plt.plot(arr[:, 0], arr[:, 4], label="proposal")
    plt.grid()
    plt.title("Iteration vs Energy")
    plt.xlabel("iteration")
    plt.ylabel("energy (runtime relative to original)")
    plt.legend(handles=[l1, l2], loc="best")

    plt.subplot(2, 2, 3)
    l1, = plt.plot(arr[:, 0], arr[:, 5], label="current")
    l2, = plt.plot(arr[:, 0], arr[:, 6], label="proposal")
    plt.grid()
    plt.title("Iteration vs Major collections")
    plt.xlabel("iteration")
    plt.ylabel("major collections")
    plt.legend(handles=[l1, l2], loc="best")

    plt.subplot(2, 2, 4)
    l1, = plt.plot(arr[:, 0], arr[:, 7], label="current")
    l2, = plt.plot(arr[:, 0], arr[:, 8], label="proposal")
    plt.grid()
    plt.title("Iteration vs Minor collections")
    plt.xlabel("iteration")
    plt.ylabel("minor collections")
    plt.legend(handles=[l1, l2], loc="best")

    fig.set_size_inches(10, 10)

    if not args.output:
        plt.show()
    else:
        plt.savefig(args.output)


if __name__ == "__main__":
    main()
