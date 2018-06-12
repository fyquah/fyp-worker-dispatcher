import argparse
import collections
import sys
import math
import cPickle as pickle
from StringIO import StringIO

import scipy
import scipy.stats

import py_common
import sexpdata
import matplotlib
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.backends import backend_pdf
from sklearn.neural_network import MLPClassifier

import os

import numpy as np
import inlining_tree

from sklearn.decomposition import PCA
from sklearn.linear_model import LogisticRegression
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis as LDA
from sklearn.metrics import roc_curve

Features = collections.namedtuple("Features",
        ["int_features", "bool_features", "numeric_features", "exp_name"])
Reward = collections.namedtuple("Reward", ["inline", "no_inline"])
DualReward = collections.namedtuple("DualReward", ["long_term", "immediate"])

option_of_sexp = inlining_tree.option_of_sexp

B = 5.0

I_DONT_KNOW = 0
ONLY_KNOW_INLINE = 1
ONLY_KNOW_APPLY = 2
BETTER_INLINE = 3
BETTER_APPLY  = 4

def sgn(x):
    if x < 0:
        return -1
    else:
        return 1


def parse(sexp, exp_name):
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
        return Features(
                int_features=int_features, bool_features=bool_features,
                numeric_features=numeric_features,
                exp_name=exp_name)

    assert isinstance(sexp, list)
    return [(parse_features(a), option_of_sexp(b, f=parse_reward)) for (a, b) in sexp]

def target_to_thorough_labels(raw_targets, minimal):
    thorough_labels = []
    for i, ta in enumerate(raw_targets):
        apply_reward  = None
        inline_reward = None

        if ta is not None and ta.inline is not None and abs(ta.inline.long_term) > minimal:
            inline_reward = ta.inline.long_term
        if ta is not None and ta.no_inline is not None and abs(ta.no_inline) > minimal:
            apply_reward = ta.no_inline

        if apply_reward is None and inline_reward is None:
            thorough_labels.append(I_DONT_KNOW)
        elif inline_reward is None:
            thorough_labels.append(ONLY_KNOW_APPLY)
        elif apply_reward is None:
            thorough_labels.append(ONLY_KNOW_INLINE)
        elif inline_reward > apply_reward:
            thorough_labels.append(BETTER_INLINE)
        else:
            thorough_labels.append(BETTER_APPLY)
    return np.array(thorough_labels)


def float_to_string(x):
    if x < 0:
        return "(-." + str(abs(x)) + ")"
    else:
        return str(x)

def float_to_bool(x):
    if x > 0.5:
        return "true"
    else:
        return "false"


def codegen_single_test_case(
        f, model,
        num_numeric_features, num_bool_features,
        numeric_feature_indices, bool_feature_indices,
        numeric_feature_means, numeric_feature_std):

    numeric_features = np.array([np.random.rand() * 2.0 - 1.0 for _ in range(num_numeric_features)])
    bool_features    = np.array([True if np.random.rand() > 0.5 else False for _ in range(num_bool_features)])
    relevant_numeric_features = numeric_features[numeric_feature_indices]
    relevant_bool_features = bool_features[bool_feature_indices]
    normalised_numeric_features = (relevant_numeric_features - numeric_feature_means) / numeric_feature_std
    features = np.concatenate([normalised_numeric_features, relevant_bool_features])
    p = model.predict_proba([features])[0]
    expected_output = [ p[0], p[1] ]
    print expected_output

    def write(s):
        f.write("  " + s + "\n")

    f.write("let () =\n")
    write("let numeric_features = [ %s ] in"
            % "; ".join(float_to_string(x) for x in numeric_features))
    write("let bool_features = [ %s ] in"
            % "; ".join(float_to_bool(x) for x in bool_features))

    write("let numeric_features =")
    write("  List.map2 (fun a b -> (a, b))")
    write("      (Array.to_list numeric_features_names) numeric_features")
    write("  |> Feature_utils.Feature_list.of_list")
    write("in")
    write("let bool_features =")
    write("  List.map2 (fun a b -> (a, b))")
    write("      (Array.to_list bool_features_names) bool_features")
    write("  |> Feature_utils.Feature_list.of_list")
    write("in")
    write("let int_features = Feature_utils.Feature_list.of_list [] in")
    write("let output = model ~numeric_features ~int_features ~bool_features in")
    write("let expected_output = Tf_lib.Vec [| %s |] in"
        % "; ".join(float_to_string(x) for x in expected_output))
    # write("let expected_output = Tf_lib.Vec [| 0.0; 0.0; |] in")
    write("assert (Tf_lib.approx_equal output expected_output)")
    f.write(";;\n\n")


def read_pickle(reward_model, feature_version):
    with open("./report_plots/machine_learning/%s/%s/data.pickle" % (reward_model.lower(), feature_version.lower()), "rb") as f:
        return pickle.load(f)


def main():
    model   = sys.argv[1]
    version = sys.argv[2]

    all_data = []
    for exp in py_common.INITIAL_EXPERIMENTS:
        with open("./report_plots/reward_assignment/data/%s/%s/feature_reward_pair_%s.sexp" % (model, exp, version.lower()), "r") as f:
            all_data.extend(parse(sexpdata.load(f), exp_name=exp))

    if not os.path.exists("report_plots/machine_learning/%s/%s" % (model, version)):
        os.makedirs("report_plots/machine_learning/%s/%s" % (model, version))
    
    with open("report_plots/machine_learning/%s/%s/data.pickle" % (model, version), "wb") as f:
        pickle.dump(all_data, f)


if __name__ == "__main__":
    main()
