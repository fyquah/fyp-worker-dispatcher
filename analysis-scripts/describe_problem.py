import math
import sys

import tensorflow as tf
import numpy as np
import scipy.sparse

import inlining_tree

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def reduce_to_first_order():
    pass

alpha = 1.0
BENEFIT_DECAY = 0.5
LOSS_PROPORTIONS = {
        "benefit": 0.5,
        "contribution": 0.5,
}
LEARNING_RATE = 0.05
NUM_EPOCH = 100


def construct_linear_benefit_relation(root, num_nodes, edge_list):
    adjacency_list = []
    for _ in range(num_nodes):
        adjacency_list.append([])
    for edge in edge_list:
        adjacency_list[edge[0]].append((edge[1], edge[2]))

    s = [{"factor": 1, "node": root, "kind": "Top_level"}]
    visited = [False] * (2 * num_nodes)
    visited[0] = True
    benefit_relation = np.zeros(2 * num_nodes)
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

        for child, kind in adjacency_list[node]:
            # TODO: Why is this check needed?? Technically we are dealing
            #       with trees. This implies there is unintended redundancy
            #       in data source
            if not visited[child]:
                visited[child] = True
                s.append({
                    "factor": factor * BENEFIT_DECAY,
                    "node":   child,
                    "kind":   kind,
                })

    assert not(
        all(not visited[x] or benefit_relation[x] > 0
        for x in range(num_nodes * 2)))

    return benefit_relation, participation


def do_some_sexy_math(reference_benefit, benefit_relations, participation_mask):
    num_examples = len(reference_benefit)
    num_features = benefit_relations.shape[1]
    num_nodes = num_features / 2

    assert benefit_relations.shape[1] % 2 == 0
    assert reference_benefit.shape == (num_examples,)
    assert benefit_relations.shape == (num_examples, num_nodes * 2)
    assert participation_mask.shape == (num_examples, num_features)

    X = tf.get_variable("X", (num_examples, num_features), dtype=tf.float64)

    # TODO: consider using sparse for this
    A = tf.constant(benefit_relations, name="benefits_relation")
    T_hat = tf.diag_part(tf.matmul(X, tf.transpose(A)))
    T_reference = tf.constant(reference_benefit, name="reference_benefit")
    assert T_hat.shape == (num_examples,)
    assert T_reference.shape == (num_examples, )

    participation_count = np.sum(participation_mask, axis=0)
    assert participation_count.shape == (num_features, )
    print("Number of features with non zero participants", sum(participation_count > 0))

    X_relevant = tf.boolean_mask(X * participation_mask, participation_count > 0, axis=1)  # (num_examples, num_non_zero_features)
    print(X_relevant.shape)

    participation_count = tf.constant(
            np.sum(participation_mask, axis=0))  # (num_features, )
    contribution_sum = tf.reduce_sum(X, axis=0)  # (num_features, )
    non_empty_contributions = tf.boolean_mask(contribution_sum, participation_count > 0)
    non_empty_participation_count = tf.boolean_mask(participation_count, participation_count > 0)
    non_empty_contribution_mean = non_empty_contributions / non_empty_participation_count  # (num_non_zero_features, )
    contribution_deviation = tf.reduce_sum(
            (tf.square(X_relevant - non_empty_contribution_mean)),
            axis=0)  / non_empty_participation_count  # (num_non_zero_features, )

    benefit_error = tf.reduce_mean(T_hat - T_reference)
    contribution_error = tf.reduce_mean(contribution_deviation)
    assert benefit_error.shape == ()
    assert contribution_error.shape == ()

    loss = (
            (LOSS_PROPORTIONS["benefit"] * benefit_error)
            + (LOSS_PROPORTIONS["contribution"] * contribution_error))

    contribution_normaliser = sum(LOSS_PROPORTIONS.values())
    loss = loss / contribution_normaliser

    optimiser = tf.train.GradientDescentOptimizer(LEARNING_RATE)
    stepper = optimiser.minimize(loss)

    dbg = tf.reduce_sum(A)

    with tf.Session() as sess:
        sess.run(tf.global_variables_initializer())
        for epoch in range(NUM_EPOCH):
            epoch_loss, dbg_value = sess.run([loss, dbg], feed_dict={})
            print("Epoch %d -- loss = %.5f %.3f" % (epoch, epoch_loss, dbg_value))


def main():
    directory = sys.argv[1]
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

    print("Inlining tree depth =", problem.properties.depth)
    print("Number of unique node identifiers =", num_vertices)
    print("Number of apply nodes =", len(apply_nodes))
    execution_times = np.array(problem.execution_times)

    reference_benefit = sigmoid(alpha * np.log(execution_times / time_average))

    participation_mask = np.zeros((num_runs, num_vertices * 2))
    benefit_relations = np.zeros((num_runs, num_vertices * 2))
    for i, edge_list in enumerate(problem.edges_lists):
        root = problem.properties.tree_path_to_ids[inlining_tree.Path([])]
        benefit_relation, participation = construct_linear_benefit_relation(
                root, num_vertices, edge_list)
        benefit_relations[i, :] = benefit_relation
        participation_mask[i, :] = participation

    do_some_sexy_math(
            reference_benefit, benefit_relations, participation_mask)


if __name__ == "__main__":
    main()
