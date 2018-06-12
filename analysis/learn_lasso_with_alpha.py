import argparse
import collections
import sympy
import logging
import os
import cPickle as pickle
import sys
import csv

import numpy as np
from scipy import linalg
import sklearn.linear_model

import inlining_tree
import learn_problem



parser = argparse.ArgumentParser(description="formulate the problem")
parser.add_argument("directory", type=str, help="experiment dir")
parser.add_argument("--decay-factor", type=float, default=None, required=True)
parser.add_argument("--benefit-function", type=str, default=None, required=True)
parser.add_argument("--skip-normalisation", action="store_true")
parser.add_argument("--force", action="store_true")


HyperParametersBase = collections.namedtuple("HyperParametersBase",
        ["decay_factor", "lasso_factor", "benefit_function"])

class HyperParameters(HyperParametersBase):

    def directory_name(self):
        lasso_factor = self.lasso_factor
        if lasso_factor is None:
            lasso_factor = "auto"
        return "decay-%f-benefit-%s-lasso-factor-%s" % (
                self.decay_factor, self.benefit_function, lasso_factor)


def is_very_small(x):
    return x < 1e-10


def compute_vector_space_of_solutions(R, rhs):
    logging.info("Computing vector space of solutions")

    w = list(sympy.symbols("w:" + str(num_features)))
    assert len(w) == num_features

    for rev_i in range(num_features):
        i = num_features - 1 - rev_i

        if all(is_very_small(x) for x in R[i, i:num_features]):
            continue

        left_most_index = None
        left_most = None
        acc = 0

        for j in range(i, num_features):
            if not is_very_small(R[i][j]):
                if left_most is None:
                    left_most_index = j
                    left_most = R[i][j]
                else:
                    acc += -w[j] * R[i][j]
        assert left_most is not None
        w[left_most_index] = acc / left_most
        print("w[%d] = %s" % (left_most_index, str(w[left_most_index])))

    # w is now the symbol of all things
    # The solution is in the form of
    #   soln = bias + a1 * v1 + a2 * v2 + ... + an * vn
    # We are primarily interested in the components in the bias
    # term
    print(w)


def _sigmoid_speedup_over_mean(execution_times, problem):
    baseline = np.mean(problem.execution_times)
    return learn_problem.sigmoid(
            -20 * (execution_times - baseline) / baseline)

def _sigmoid_speedup_over_baseline(execution_times, problem):
    baseline = get_baseline_execution_time(problem)
    return learn_problem.sigmoid(
            -20 * (execution_times - baseline) / baseline)


def _linear_speedup_over_mean(execution_times, problem):
    baseline = np.mean(problem.execution_times)
    return -(execution_times - baseline) / baseline


def _linear_speedup_over_baseline(execution_times, problem):
    baseline = get_baseline_execution_time(problem)
    return -(execution_times - baseline) / baseline


def _log_speedup_over_mean(execution_times, problem):
    baseline = np.mean(problem.execution_times)
    return -np.log(execution_times / baseline)


def _log_speedup_over_baseline(execution_times, problem):
    baseline = get_baseline_execution_time(problem)
    return -np.log(execution_times / baseline)


def _tanh_speedup_over_mean(execution_times, problem):
    baseline = np.mean(problem.execution_times)
    return np.tanh(
            -10 * (execution_times - baseline) / baseline)


def _tanh_speedup_over_baseline(execution_times, problem):
    baseline = get_baseline_execution_time(problem)
    return np.tanh(
            -10 * (execution_times - baseline) / baseline)


def construct_benefit_from_exec_time(kind, problem):
    execution_times = problem.execution_times
    dispatch = {
            "sigmoid_speedup_over_mean": _sigmoid_speedup_over_mean,
            "linear_speedup_over_mean":  _linear_speedup_over_mean,
            "log_speedup_over_mean":     _log_speedup_over_mean,
            "tanh_speedup_over_mean":     _tanh_speedup_over_mean,

            "sigmoid_speedup_over_baseline": _sigmoid_speedup_over_baseline,
            "linear_speedup_over_baseline":  _linear_speedup_over_baseline,
            "log_speedup_over_baseline":     _log_speedup_over_baseline,
            "tanh_speedup_over_baseline": _tanh_speedup_over_baseline,
    }
    return dispatch[kind](execution_times, problem=problem)


def geometric_mean(arr):
    acc = 1.0
    for a in arr:
        acc *= a
    return acc ** (1.0 / len(arr))


def get_baseline_execution_time(problem):
    times = []
    for i, dir in enumerate(problem.execution_directories):
        if "initial" in dir:
            times.append(problem.execution_times[i])
    assert len(times) > 0
    logging.info("Geometric  mean initial time = %f" % geometric_mean(times))
    logging.info("Arithmetic mean initial time = %f" % np.mean(times))
    logging.info("Aithmetic mean time over everything = %f" % np.mean(problem.execution_times))
    return geometric_mean(times)


def run(args):
    logging.getLogger().setLevel(logging.INFO)
    args = parser.parse_args(args)
    problem_directory = args.directory
    logging.info("Loading problem definition ...")
    hyperparams = HyperParameters(
            decay_factor=args.decay_factor,
            benefit_function=args.benefit_function,
            lasso_factor=None)
    assert args.skip_normalisation
    experiment_name = "lasso"
    exp_directory = os.path.join(
            problem_directory, experiment_name, hyperparams.directory_name())

    if os.path.exists(os.path.join(exp_directory, "contributions.npy")) and not args.force:
        logging.info("A solution already exist for %s/%s/%s! Pass --force to recompute"
                % (problem_directory, experiment_name, hyperparams.directory_name()))
        return

    problem = inlining_tree.Problem.load(problem_directory)
    execution_times = problem.execution_times

    if not os.path.exists(exp_directory):
        os.makedirs(exp_directory)

    normalise_with_num_children = not args.skip_normalisation
    problem_matrices = learn_problem.construct_problem_matrices(
            problem, hyperparams, normalise_with_num_children)

    target_benefit = construct_benefit_from_exec_time(
            args.benefit_function, problem)
    num_features = problem_matrices.benefit_relations.shape[1]

    logging.info("Computing analytical solution for %s." % (experiment_name))
    logging.info("  decay factor = %.6f" % (args.decay_factor))
    logging.info("  benefit function = %s" % args.benefit_function)

    A = problem_matrices.benefit_relations

    search_log = open(os.path.join(exp_directory, "search_log.csv"), "w")
    print "Logging to", os.path.join(exp_directory, "search_log.csv")
    wrt = csv.writer(search_log)
    wrt.writerow(["alpha", "r_squared", "sum(abs(w))", "sum(abs(w) > 0.0)"])

    ctr = 0
    lo = 0.0
    hi = 1.0

    m = int(0.2 * len(A))
    print "Number of entries in validation set =", m
    print "Number of variables in linear eqn =", A.shape[1]

    A_validation = A[:m, :]
    target_benefit_validation = target_benefit[:m]
    A_train = A[m:,:]
    target_benefit_train = target_benefit[m:]

    while (hi - lo) > 1e-7:
        mid = (hi + lo) / 2.0
        lambda_ = mid

        model = sklearn.linear_model.Lasso(alpha=lambda_, fit_intercept=False)
        # model = gpu_shit.Lasso(alpha=lambda_, fit_intercept=False)
        model.fit(A_train, target_benefit_train)

        w = model.coef_
        assert w.shape == (A_train.shape[1],)
        r_squared = model.score(A_train, target_benefit_train)
        validation_r_squared = model.score(A_validation, target_benefit_validation)

        row = [lambda_, r_squared, np.sum(abs(w)), np.sum(abs(w) > 0.000000000001)]
        print lambda_, r_squared, np.sum(abs(w)), np.sum(abs(w) > 0.000000000001), validation_r_squared

        hi = mid
        if r_squared < 0.05:
            continue
        ctr += 1

        search_log.flush()
        wrt.writerow([str(x) for x in row])

        hyperparams = HyperParameters(
                decay_factor=args.decay_factor,
                benefit_function=args.benefit_function,
                lasso_factor=lambda_)

        sub_exp_directory = os.path.join(
                problem_directory, "lasso-with-alpha", hyperparams.directory_name())
        logging.info("Found analytical solution for %s, saving to %s!"
                % (hyperparams.directory_name(), sub_exp_directory))
        try:
            os.makedirs(sub_exp_directory)
        except:
            pass
        with open(os.path.join(sub_exp_directory, "hyperparams.pkl"), "wb") as f:
            pickle.dump(hyperparams, f)
        with open(os.path.join(sub_exp_directory, "contributions.npy"), "wb") as f:
            np.save(f, w)
        if ctr > 10:
            break

    search_log.close()

def main():
    run(sys.argv[1:])

if __name__ == "__main__":
    main()
