import argparse
import collections
import sympy
import logging
import os
import pickle

import numpy as np
from scipy import linalg

import inlining_tree
import learn_problem

EXPERIMENT_NAME = "linear-general-reward"


parser = argparse.ArgumentParser(description="formulate the problem")
parser.add_argument("directory", type=str, help="experiment dir")
parser.add_argument("--decay-factor", type=float, default=None)
parser.add_argument("--vector-space", action="store_true")


HyperParametersBase = collections.namedtuple("HyperParametersBase",
        ["decay_factor", "is_vector_space"])

class HyperParameters(HyperParametersBase):

    def directory_name(self):
        if self.is_vector_space:
            return "without-vector-space-decay-%f" % self.decay_factor
        else:
            return "without-vector-space-decay-%f" % self.decay_factor

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


def main():
    logging.getLogger().setLevel(logging.INFO)
    args = parser.parse_args()
    problem_directory = args.directory
    problem = inlining_tree.Problem.load(problem_directory)
    execution_times = problem.execution_times

    logging.info("Loading problem definition from disk")
    hyperparams = HyperParameters(
            decay_factor=args.decay_factor,
            is_vector_space=args.vector_space)
    exp_directory = os.path.join(
            problem_directory, EXPERIMENT_NAME, hyperparams.directory_name())
    if not os.path.exists(exp_directory):
        os.makedirs(exp_directory)

    with open(os.path.join(exp_directory, "hyerparams.pkl"), "wb") as f:
        pickle.dump(hyperparams, f)

    problem_matrices = learn_problem.construct_problem_matrices(
            problem, hyperparams)
    time_average = np.mean(execution_times)
    target_benefit = learn_problem.sigmoid(
            -20 * (execution_times - time_average) / time_average)
    num_features = problem_matrices.benefit_relations.shape[1]

    if args.vector_space:
        logging.info(
                "Computing analytical vector space of solution for \
                reward")
        A = problem_matrices.benefit_relations

        # A.shape = (m, n)
        # b.shape = (m,)
        #   w = (A' A)^(-1) A' b
        # but we know for sure that it is not going to be invertible
        # due to infinite solutions
        # So:
        #   K = A' A
        #   Q R = K
        #   Q R w = A' b
        #   R w = Q^(-1) A' b  # efficient because Q is othorgonal unit
        # R is now in reduced echlon form
        logging.info("Decomposing matrix")
        Q, R = linalg.qr(np.matmul(A.T, A))

        logging.info("Backward substitution")

        # Q is orthogonal, so inv(Q) == Q.T
        rhs = np.matmul(Q.T, np.matmul(A.T, target_benefit))
        assert R.shape == (num_features, num_features)
        compute_vector_space_of_solutions(R, rhs)
    else:
        logging.info("Finding _a single_ solution")
        A = problem_matrices.benefit_relations
        w = linalg.lstsq(A, target_benefit)
        np.save(os.path.join(exp_directory, "contributions.npz"), w)


if __name__ == "__main__":
    main()
