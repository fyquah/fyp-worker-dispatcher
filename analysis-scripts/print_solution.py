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

def main():
    directory = sys.argv[1]
    epoch = int(sys.argv[2])
    problem = inlining_tree.Problem.load(directory)

    id_to_tree_path  = { v: k for k, v in problem.properties.tree_path_to_ids.items() }
    num_vertices = len(problem.properties.tree_path_to_ids)
    num_runs = len(problem.execution_times)

    participation_mask = np.zeros((num_runs, num_vertices * 2))
    benefit_relations = np.zeros((num_runs, num_vertices * 2))
    for i, edge_list in enumerate(problem.edges_lists):
        root = problem.properties.tree_path_to_ids[inlining_tree.Path([])]
        benefit_relation, participation = learn_problem.construct_linear_benefit_relation(
                root, num_vertices, edge_list)
        benefit_relations[i, :] = benefit_relation
        participation_mask[i, :] = participation

    X = np.load(os.path.join(directory, "learnt-values-%d.npy" % epoch))
    participation_counts = np.sum(participation_mask, axis=0)

    for i in range(num_vertices):
        path = id_to_tree_path[i]

        lhs = olaf(X[:, i * 2], participation_counts[i * 2])
        rhs = olaf(X[:, i * 2 + 1], participation_counts[i * 2 + 1])

        print ("%s\t%s\t%s" % (path, lhs, rhs))


if __name__ == "__main__":
    main()
