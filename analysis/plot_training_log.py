import argparse
import collections

import sexpdata
import matplotlib.pyplot as plt
from matplotlib.backends import backend_pdf

import sexp_parser
import inlining_tree


Entry = collections.namedtuple("Entry", ["accuracy", "loss"])
Snapshot = collections.namedtuple("Snapshot", ["epoch", "training", "validation", "test"])

def ss_entry_of_sexp(sexp):
    m = inlining_tree.sexp_to_map(sexp)
    accuracy = m.get("accuracy", None)
    if accuracy is not None:
        accuracy = float(accuracy)
    loss = float(m["loss"])
    return Entry(loss=loss, accuracy=accuracy)


def snapshot_of_sexp(sexp):
    try:
        m = inlining_tree.sexp_to_map(sexp)
        option_of_sexp = inlining_tree.option_of_sexp
        epoch = int(m["epoch"])
        training = option_of_sexp(m["training"], f=ss_entry_of_sexp)
        validation = option_of_sexp(m["validation"], f=ss_entry_of_sexp)
        test = option_of_sexp(m["test"], f=ss_entry_of_sexp)
        return Snapshot(epoch=epoch, training=training, validation=validation, test=test)
    except ValueError:  # When parsing other sexps
        return None


def parse_line(s):
    if "Info" not in s:
        return None
    s = s[s.find("("):].strip()
    if not s:
        return None
    sexp = sexp_parser.parse(s)
    ss = snapshot_of_sexp(sexp)
    return ss

def parse_baseline(s):
    if "Info" not in s:
        return None
    indicator = "Baseline test accuracy ="
    idx = s.find(indicator)
    if idx == -1 or idx == None:
        return None
    s = s[idx + len(indicator):].strip()
    if not s:
        return None
    return float(s)


parser = argparse.ArgumentParser(description="Parse")
parser.add_argument("filename", type=str, help="shit")
parser.add_argument("--pdf", type=str, help="shit")


def remove_nones(arr):
    return [x for x in arr if x is not None]


def main():
    args = parser.parse_args()
    arr = []
    baseline = None
    with open(args.filename, "r") as f:
        for line in f:
            a = parse_line(line)
            baseline = parse_baseline(line)
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
    training_accuracy = remove_nones(training_accuracy)
    validation_accuracy = remove_nones(validation_accuracy)
    test_accuracy = remove_nones(test_accuracy)

    is_classification = True
    if len(training_accuracy) == 0:
        is_classification = False

    if is_classification:
        plt.subplot(1, 2, 1)

    h.extend(plt.plot(epochs, training_loss, label="training"))
    h.extend(plt.plot(epochs, validation_loss, label="validation"))
    h.extend(plt.plot(epochs, test_loss, label="test"))
    plt.grid()
    plt.legend(handles=h)

    if is_classification:
        plt.subplot(1, 2, 2)
        h = []
        h.extend(plt.plot(epochs, training_accuracy, label="training"))
        h.extend(plt.plot(epochs, validation_accuracy, label="validation"))
        h.extend(plt.plot(epochs, test_accuracy, label="test"))
        h.append(plt.axhline(y=baseline, color='r', linestyle='--', label="baseline"))
        plt.legend(handles=h)
        plt.grid()

    if args.pdf is None:
        plt.show()
    else:
        pp = backend_pdf.PdfPages(args.pdf)
        pp.savefig()
        pp.close()

if __name__ == "__main__":
    main()
