import csv
import sys
import os
import matplotlib.pyplot as plt
import matplotlib

def do_it(filename, label, index):

    def r(line):
        if callable(index):
            return index(line)
        else:
            return line[index]

    xs = []
    y1 = []
    y2 = []
    y3 = []
    with open(filename, "rb") as f:
        for line in csv.reader(f):
            if line[0].strip() == "ridge-hand":
                xs.append(float(line[1]))
                y1.append(float(r(line)))
            elif line[0].strip() == "ridge-star":
                y2.append(float(r(line)))
            elif line[0].strip() == "ridge-general":
                y3.append(float(r(line)))

    plt.plot(xs, y1, label="$h_{hand}$")
    plt.plot(xs, y2, label="$H_*$")
    plt.plot(xs, y3, label="$h_{general}$")
    plt.xscale("log", nonposx="clip")
    plt.title(r"$\tau$ vs %s" % label)
    plt.legend()
    plt.grid()

def main():
    matplotlib.rc("text", usetex=True)
    d = "report_plots/machine_learning/plots/ridge"
    if not os.path.exists(d):
        os.makedirs(d)

    do_it(sys.argv[1], label="Proportion of 'I Don't Know'", index=2)
    plt.savefig(os.path.join(d, "prop_idk.pdf"))

    do_it(sys.argv[1], label="Proportion of 'Inline'", index=3)
    plt.savefig(os.path.join(d, "prop_inline.pdf"))

    do_it(sys.argv[1], label="Proportion of 'Apply'", index=4)
    plt.savefig(os.path.join(d, "prop_apply.pdf"))

    do_it(sys.argv[1], label="Ratio of Inline to Apply Examples", index=lambda x : float(x[3]) / float(x[4]))
    plt.savefig(os.path.join(d, "tau_vs_ratio.pdf"))


if __name__ == "__main__":
    main()
