import argparse
import collections
import logging
import math
import os
import sys

import tensorflow as tf
import numpy as np
import scipy.sparse

import inlining_tree
import learn_problem


def olaf(a, n):
    if n == 0:
        return "N/A"
    else:
        return "%.5f" % (sum(a) / float(n))

parser = argparse.ArgumentParser(description="formulate the problem")
parser.add_argument("dir", type=str, help="experiment dir")
parser.add_argument("--epoch", type=int, help="Epoch that we are interespted in", required=True)
parser.add_argument("--opt-info", action="store_true")
parser.add_argument("--components", action="store_true")


def load_estimates(directory, epoch):
    path = os.path.join(directory, "learnt-values-%d-sparse.npz" % epoch)
    if os.path.exists(path):
        logging.info("Loading values from %s " % path)
        # TODO(fyq14): Really assume that it is a dense matrix?
        return scipy.sparse.load_npz(path).todense()

    path = os.path.join(directory, "learnt-values-%d.npz" % epoch)
    if os.path.exists(path):
        logging.info("Loading values from %s " % path)
        return np.load(path)

    path = os.path.join(directory, "learnt-values-%d.npy" % epoch)
    if os.path.exists(path):
        logging.info("Loading values from %s " % path)
        return np.load(path)


def main():
    logging.getLogger().setLevel(logging.INFO)
    args = parser.parse_args()
    directory = args.dir
    epoch = args.epoch
    problem = inlining_tree.Problem.load(directory)

    id_to_tree_path  = { v: k for k, v in problem.properties.tree_path_to_ids.items() }
    num_vertices = len(problem.properties.tree_path_to_ids)
    num_runs = len(problem.execution_times)

    time_average = np.mean(problem.execution_times)
    execution_times = problem.execution_times
    reference_benefit = learn_problem.ALPHA * np.log(execution_times / time_average)

    X_estimate = load_estimates(directory, args.epoch)
    problem_matrices = learn_problem.construct_problem_matrices(problem)
    # X_estimate = problem_matrices.participation_mask * X_estimate

    objective_tensors = learn_problem.construct_objective(
            reference_benefit=reference_benefit,
            benefit_relations=problem_matrices.benefit_relations,
            participation_mask=problem_matrices.participation_mask,
            X_init=tf.constant_initializer(X_estimate))

    print(">>>>>>> Solution after %d epoch <<<<<<<<" % epoch)

    if args.components:
        participation_counts = np.sum(
                problem_matrices.participation_mask, axis=0)
        arr = []
        for i in range(num_vertices):
            path = id_to_tree_path[i]
            lhs = olaf(X_estimate[:, i * 2], participation_counts[i * 2])
            rhs = olaf(X_estimate[:, i * 2 + 1], participation_counts[i * 2 + 1])
            arr.append((path, lhs, rhs))

        arr.sort(key=lambda (a,b,c): a)
        lhs_size = max(len(x) for (_, x, _) in arr) + 1
        rhs_size = max(len(x) for (_, _, x) in arr) + 1

        for (path, lhs, rhs) in arr:
            print lhs,
            print (" " * (lhs_size - len(lhs))),
            print " | ",
            print rhs,
            print (" " * (lhs_size - len(rhs))),
            print " |", str(path)

    elif args.opt_info:
        with tf.Session() as sess:
            sess.run(tf.global_variables_initializer())
            loss, benefit_loss, variance_loss = sess.run([
                    objective_tensors.loss,
                    objective_tensors.benefit_loss,
                    objective_tensors.variance_loss,
                ], feed_dict={})

            print("- Loss = %.6f" % loss)
            print("- Benefit loss = %.6f" % benefit_loss)
            print("- Variance loss = %.6f" % variance_loss)

    else:
        parser.print_help()


if __name__ == "__main__":
    main()
