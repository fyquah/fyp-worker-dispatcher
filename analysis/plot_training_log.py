import argparse
import collections

import matplotlib.pyplot as plt
from matplotlib.backends import backend_pdf

import sexp_parser
import inlining_tree


Entry = collections.namedtuple("Entry", ["accuracy", "loss"])
Snapshot = collections.namedtuple("Snapshot", ["epoch", "training", "validation", "test"])

def ss_entry_of_sexp(sexp):
    m = inlining_tree.sexp_to_map(sexp)
    accuracy = float(m["accuracy"])
    loss = float(m["loss"])
    return Entry(loss=loss, accuracy=accuracy)


def snapshot_of_sexp(sexp):
    m = inlining_tree.sexp_to_map(sexp)
    option_of_sexp = inlining_tree.option_of_sexp
    epoch = int(m["epoch"])
    training = option_of_sexp(m["training"], f=ss_entry_of_sexp)
    validation = option_of_sexp(m["validation"], f=ss_entry_of_sexp)
    test = option_of_sexp(m["test"], f=ss_entry_of_sexp)
    return Snapshot(epoch=epoch, training=training, validation=validation, test=test)


def parse_line(s):
    if "INFO" not in s:
        return None
    sexp = sexp_parser.parse(s[s.find("("):])
    ss = snapshot_of_sexp(sexp)
    return ss

parser = argparse.ArgumentParser(description="Parse")
parser.add_argument("filename", type=str, help="shit")
parser.add_argument("--pdf", type=str, help="shit")


def main():
    args = parser.parse_args()
    arr = []
    with open(args.filename, "r") as f:
        for line in f:
            a = parse_line(line)
            if a is not None:
                arr.append(a)

    epochs = []
    training_loss = []
    validation_loss = []
    test_loss = []
    training_accuracy = []
    validation_accuracy = []
    test_accuracy = []
    for ss in arr:
        epochs.append(ss.epoch)
        training_loss.append(ss.training.loss)
        validation_loss.append(ss.validation.loss)
        test_loss.append(ss.test.loss)
        training_accuracy.append(ss.training.accuracy)
        validation_accuracy.append(ss.validation.accuracy)
        test_accuracy.append(ss.test.accuracy)

    h = []
    plt.suptitle(args.filename)

    plt.subplot(1, 2, 1)
    h.extend(plt.plot(epochs, training_loss, label="training"))
    h.extend(plt.plot(epochs, validation_loss, label="validation"))
    h.extend(plt.plot(epochs, test_loss, label="test"))
    plt.legend(handles=h)

    plt.subplot(1, 2, 2)
    h = []
    h.extend(plt.plot(epochs, training_accuracy, label="training"))
    h.extend(plt.plot(epochs, validation_accuracy, label="validation"))
    h.extend(plt.plot(epochs, test_accuracy, label="test"))
    plt.legend(handles=h)
    plt.grid()

    pp = backend_pdf.PdfPages(args.pdf)
    pp.savefig()
    pp.close()

if __name__ == "__main__":
    main()
