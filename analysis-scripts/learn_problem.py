import argparse
import collections
import math
import sys
import os
import pickle
import logging

import matplotlib.pyplot as plt
import numpy as np
from numpy import linalg
import scipy.sparse
import tensorflow as tf

import inlining_tree

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def reduce_to_first_order():
    pass


parser = argparse.ArgumentParser(description="formulate the problem")
parser.add_argument("directory", type=str, help="experiment dir")
parser.add_argument("--resume-from", type=int, help="Epoch to start training from", default=None)
parser.add_argument("--total-epoch", type=int, default=10000)
parser.add_argument("--variance-factor", type=float, default=None)
parser.add_argument("--decay-factor", type=float, default=None)


ALPHA = 100.0  # Scales benefit to something reasonable
LEARNING_RATE = 0.0005   # Not super important, adam usally does reasonably
CONVERGENCE = 0.00002

HyperParameters = collections.namedtuple("HyperParameters",
        ["decay_factor", "variance_factor"])


def construct_linear_benefit_relation(root, num_nodes, edge_list, hyperparams):
    adjacency_list = []
    for _ in range(num_nodes):
        adjacency_list.append([])
    for edge in edge_list:
        adjacency_list[edge[0]].append((edge[1], edge[2]))

    s = [{"factor": 1, "node": root, "kind": "Top_level"}]
    visited = [False] * (2 * num_nodes)
    visited[root] = True
    benefit_relation = np.zeros(2 * num_nodes, dtype=np.float64)
    participation = np.zeros(2 * num_nodes)

    while len(s):
        tos = s.pop()
        factor = tos["factor"]
        node = tos["node"]
        kind = tos["kind"]

        if kind == "Inlined" or kind == "Top_level" or kind == "Declaration":
            benefit_relation[2 * node] = factor
            participation[2 * node] = 1
        elif kind == "Non_inlined": 
            benefit_relation[2 * node + 1] = factor
            participation[2 * node + 1] = 1
        else:
            print("unexpected kind =", kind)
            assert False

        num_children = float(len(adjacency_list[node]))

        for child, kind in adjacency_list[node]:
            # TODO: Why is this check needed?? Technically we are dealing
            #       with trees. This implies there is unintended redundancy
            #       in data source
            if not visited[child]:
                visited[child] = True
                s.append({
                    "factor": factor * hyperparams.decay_factor / num_children,
                    "node":   child,
                    "kind":   kind,
                })

    assert not(
        all(not visited[x] or benefit_relation[x] > 0
        for x in range(num_nodes * 2)))

    return benefit_relation, participation

ObjectiveTensors = collections.namedtuple("ObjectiveTensors", [
        "step", "loss", "variance_loss", "benefit_loss", "estimates"])

ProblemMatrices = collections.namedtuple("ProblemMatrices", [
    "participation_mask", "benefit_relations"])


def construct_problem_matrices(problem, hyperparams):
    logging.info("Constructing problem matrices")
    num_runs = len(problem.execution_times)
    num_vertices = len(problem.properties.tree_path_to_ids)
    participation_mask = np.zeros((num_runs, num_vertices * 2))
    benefit_relations = np.zeros((num_runs, num_vertices * 2))
    for i, edge_list in enumerate(problem.edges_lists):
        root = problem.properties.tree_path_to_ids[inlining_tree.Path([])]
        benefit_relation, participation = construct_linear_benefit_relation(
                root, num_vertices, edge_list, hyperparams=hyperparams)
        benefit_relations[i, :] = benefit_relation
        participation_mask[i, :] = participation
    return ProblemMatrices(participation_mask, benefit_relations)


def construct_objective(
        reference_benefit,
        benefit_relations,
        participation_mask,
        X_init,
        hyperparams):
    logging.info("Constructing tensor graph")
    num_examples = len(reference_benefit)
    num_features = benefit_relations.shape[1]
    num_nodes = num_features / 2

    assert benefit_relations.shape[1] % 2 == 0
    assert reference_benefit.shape == (num_examples,)
    assert benefit_relations.shape == (num_examples, num_nodes * 2)
    assert participation_mask.shape == (num_examples, num_features)

    participation_count = np.sum(participation_mask, axis=0)
    assert participation_count.shape == (num_features, )

    (non_zero_indices,) = participation_count.nonzero()
    non_zero_indices = non_zero_indices.astype(np.int32)
    num_non_zero_features = len(non_zero_indices)
    trimmed_participation_mask = np.squeeze(participation_mask[:, non_zero_indices])
    assert trimmed_participation_mask.shape == (num_examples, num_non_zero_features)

    X = tf.get_variable(
            "X", (num_examples, num_features), dtype=tf.float64,
            initializer=X_init)

    # TODO: consider using sparse for this
    A = tf.constant(benefit_relations, name="benefits_relation")
    T_hat = tf.diag_part(tf.matmul(X, tf.transpose(A)))
    T_reference = tf.constant(reference_benefit, name="reference_benefit")
    assert T_hat.shape == (num_examples,)
    assert T_reference.shape == (num_examples, )

    X_relevant = X * participation_mask
    X_relevant = tf.gather(X_relevant, non_zero_indices, axis=1)  # (num_examples, num_non_zero_features)
    contribution_sum = tf.reduce_sum(X, axis=0)  # (num_features, )
    non_empty_contributions = tf.reduce_sum(X_relevant, axis=0)
    non_empty_participation_count = participation_count[non_zero_indices]
    assert non_empty_contributions.shape == (num_non_zero_features,)
    assert non_empty_participation_count.shape == (num_non_zero_features,)

    non_empty_contribution_mean = \
            non_empty_contributions \
            / tf.cast(non_empty_participation_count, tf.float64)  # (num_non_zero_features, )
    contribution_deviation = tf.reduce_mean(
            (tf.square((X_relevant - non_empty_contribution_mean) * trimmed_participation_mask)),
            axis=0)  / tf.cast(non_empty_participation_count, tf.float64)  # (num_non_zero_features, )

    benefit_error = tf.reduce_mean((T_hat - T_reference) ** 2)
    contribution_error = tf.reduce_mean(contribution_deviation)
    assert benefit_error.shape == ()
    assert contribution_error.shape == ()

    loss = benefit_error + hyperparams.variance_factor * contribution_error
    optimiser = tf.train.AdamOptimizer(LEARNING_RATE)
    stepper = optimiser.minimize(loss)

    return ObjectiveTensors(
            step=stepper,
            loss=loss,
            variance_loss=contribution_error,
            benefit_loss=benefit_error,
            estimates=X,
    )


def load_hyperparams(directory):
    with open(os.path.join(directory, "hyperparams.pkl"), "rb") as f:
        return pickle.load(f)


def main():
    logging.getLogger().setLevel(logging.INFO)
    args = parser.parse_args()
    directory = args.directory
    problem = inlining_tree.Problem.load(directory)

    id_to_tree_path  = { v: k for k, v in problem.properties.tree_path_to_ids.items() }
    apply_nodes = [
            value for value in problem.properties.tree_path_to_ids.keys()
            if value.is_apply_node()
    ]
    num_vertices = len(problem.properties.tree_path_to_ids)
    arr_num_children = [0] * num_vertices
    time_average = np.mean(problem.execution_times)
    num_runs = len(problem.execution_times)

    assert len(problem.execution_times) == len(problem.edges_lists)

    print("Inlining tree depth = %d" % problem.properties.depth)
    print("Number of unique node identifiers = %d" % num_vertices)
    print("Number of apply nodes = %d" % len(apply_nodes))
    print("Number of runs = %d" % num_runs)
    execution_times = np.array(problem.execution_times)

    hyperparams_from_cmd_line = HyperParameters(
            decay_factor=args.decay_factor,
            variance_factor=args.variance_factor)
    hyperparams_path = os.path.join(directory, "hyperparams.pkl")
    if os.path.exists(hyperparams_path):
        logging.info(
                "Loading hyperparameters from %s and checking for "
                "consistency with command line arguments"
                % hyperparams_path
        )
        with open(hyperparams_path, "rb") as f:
            hyperparams = pickle.load(f)

        for i, v in enumerate(hyperparams_from_cmd_line):
            assert v is None or hyperparams[i] == v
    else:
        logging.info(
                "Saving hyperparameters to %s"
                % hyperparams_path
        )
        for v in hyperparams_from_cmd_line:
            assert v is not None
        hyperparams = hyperparams_from_cmd_line
        with open(hyperparams_path, "wb") as f:
            pickle.dump(hyperparams, f)

    # TODO(fyq14): Think through this again ...
    reference_benefit = ALPHA * np.log(execution_times / time_average)

    problem_matrices = construct_problem_matrices(problem, hyperparams)
    if args.resume_from is not None:
        path_to_x_init = os.path.join(
                directory, "learnt-values-%d.npy" % args.resume_from)
        X_init = np.load(path_to_x_init)
        X_init = tf.constant_initializer(X_init)
    else:
        X_init = None
    tensors = construct_objective(
            reference_benefit,
            problem_matrices.benefit_relations,
            problem_matrices.participation_mask,
            X_init=X_init,
            hyperparams=hyperparams)

    gpu_options = tf.GPUOptions(per_process_gpu_memory_fraction=0.47)
    with tf.Session(config=tf.ConfigProto(gpu_options=gpu_options)) as sess:
        sess.run(tf.global_variables_initializer())
        epoch_start_from = args.resume_from
        for epoch in range(epoch_start_from, args.total_epoch):
            _, epoch_loss, epoch_benefit_error, epoch_contribution_error = \
                    sess.run([
                            tensors.step, tensors.loss,
                            tensors.benefit_loss, tensors.variance_loss],
                        feed_dict={})
            logging.info(
                    "Epoch %d -- loss = %.5f (contribution = %.5f, benefit = %.5f)"
                    % (epoch, epoch_loss, epoch_contribution_error, epoch_benefit_error))

            checkpoint_able = (
                    epoch % 100 == 0 or
                    epoch_loss < CONVERGENCE or
                    epoch == args.total_epoch - 1)
            if epoch != args.resume_from and checkpoint_able:
                logging.info("Saving values for epoch %d" % epoch)
                rows, cols = problem_matrices.participation_mask.nonzero()
                estimates = sess.run(tensors.estimates, feed_dict={})

                entries = estimates[rows, cols]
                assert len(rows) == len(cols)
                num_sparse_entries = len(rows)
                logging.info("Converting estimates to sparse matrix with %d entries"
                        % num_sparse_entries)
                sparse_estimates = scipy.sparse.coo_matrix(
                        (entries, (rows, cols)), shape=estimates.shape)
                assert sparse_estimates.shape == estimates.shape
                output_filename = os.path.join(
                        directory, ("learnt-values-%d-sparse.npz" % epoch))

                logging.info("Dumping to %s" % output_filename)
                scipy.sparse.save_npz(output_filename, sparse_estimates)

            if epoch_loss < CONVERGENCE:
                break
                print("Loss less than requirement!")
    print("Optimistaion done!")


if __name__ == "__main__":
    main()
