import math
import sys

import tensorflow as tf
import numpy as np
import scipy.sparse

import inlining_tree

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

    edges = set()
    for edge_list in problem.edges_lists:
        for u, v, _kind in edge_list:
            edges.add((u, v))

    print "Inlining tree depth =", problem.properties.depth
    print "Number of unique node identifiers =", num_vertices
    print "Number of edges =", num_runs
    print "Number of apply nodes =", len(apply_nodes)
    print "Number of runs =", num_runs
    print "Number of unique edges =", len(edges)

    # for path in problem.properties.tree_path_to_ids.keys():
    #     print(path)


if __name__ == "__main__":
    main()
