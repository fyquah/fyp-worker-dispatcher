import argparse
import sys
import csv

from matplotlib import pylab as plt
from matplotlib.backends.backend_pdf import PdfPages
import pylab

import numpy as np

parser = argparse.ArgumentParser()
parser.add_argument("--title", type=str, dest="title")
parser.add_argument("--pdf", type=str, dest="pdf", default=None)

def main():
    args = parser.parse_args()
    arr = []
    for line in csv.reader(sys.stdin):
        arr.append([float(x) for x in line])
    arr = np.array(arr)

    plt.suptitle(args.title)

    plt.subplot(2, 2, 1)
    l1, = plt.plot(arr[:, 0], arr[:, 1], label="current")
    l2, = plt.plot(arr[:, 0], arr[:, 2], label="proposal")
    plt.title("Iteration vs Execution time")
    plt.xlabel("iteration")
    plt.ylabel("execution time")
    plt.legend(handles=[l1, l2], loc="best")

    plt.subplot(2, 2, 2)
    l1, = plt.plot(arr[:, 0], arr[:, 3], label="current")
    l2, = plt.plot(arr[:, 0], arr[:, 4], label="proposal")
    plt.title("Iteration vs Energy")
    plt.xlabel("iteration")
    plt.ylabel("energy (runtime relative to original)")
    plt.legend(handles=[l1, l2], loc="best")

    plt.subplot(2, 2, 3)
    l1, = plt.plot(arr[:, 0], arr[:, 5], label="current")
    l2, = plt.plot(arr[:, 0], arr[:, 6], label="proposal")
    plt.title("Iteration vs Major collections")
    plt.xlabel("iteration")
    plt.ylabel("major collections")
    plt.legend(handles=[l1, l2], loc="best")

    plt.subplot(2, 2, 4)
    l1, = plt.plot(arr[:, 0], arr[:, 7], label="current")
    l2, = plt.plot(arr[:, 0], arr[:, 8], label="proposal")
    plt.title("Iteration vs Minor collections")
    plt.xlabel("iteration")
    plt.ylabel("minor collections")
    plt.legend(handles=[l1, l2], loc="best")

    if not args.pdf:
        plt.show()
    else:
        pp = PdfPages(args.pdf)
        plt.savefig(pp, format="pdf")
        pp.savefig()
        pp.close()


if __name__ == "__main__":
    main()
