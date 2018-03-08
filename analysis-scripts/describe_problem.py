import sys

import inlining_tree

def reduce_to_first_order():
    pass


def main():
    directory = sys.argv[1]
    problem = inlining_tree.Problem.load(directory)
    id_to_tree_path  = { v: k for k, v in problem.properties.tree_path_to_ids.items() }
    print(id_to_tree_path[2019])

    apply_nodes = [
            value for value in problem.properties.tree_path_to_ids.keys()
            if value.is_apply_node()
    ]

    print("depth =", problem.properties.depth)
    print("Number of unique node identifiers =", len(problem.properties.tree_path_to_ids))
    print("Number of apply nodes =", len(apply_nodes))

    for i, matrix in enumerate(problem.matrices):
        print("===== matrix %d =====" % i)
        # xs, ys = matrix.nonzero()
        # for x, y in zip(xs, ys):
        #     print("--> (%d, %d) = %d" % (x, y, matrix[x, y]))

if __name__ == "__main__":
    main()
