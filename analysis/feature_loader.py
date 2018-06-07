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

def target_to_thorough_labels(raw_targets):
    thorough_labels = []
    assert len(features) == len(raw_targets)
    for i, ta in enumerate(raw_targets):
        apply_reward  = None
        inline_reward = None

        if ta.inline.long_term is not None and abs(ta.inline.long_term) > minimal:
            inline_reward = ta.inline.long_term
        if ta.no_inline is not None and abs(ta.no_inline) > minimal:
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


def main():
    version = sys.argv[1]

    all_data = []
    for exp in py_common.INITIAL_EXPERIMENTS:
        with open("./report_plots/reward_assignment/data/%s/feature_reward_pair.sexp" % exp, "r") as f:
            all_data.extend(parse(sexpdata.load(f), exp_name=exp))
    
    with open("./report_plots/machine_learning/%s_data.pickle" % version, "wb") as f:
        pickle.dump(all_data, f)


if __name__ == "__main__":
    main()
