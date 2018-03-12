import argparse
import logging
import os
import pickle
import sys
import StringIO

import numpy as np
from numpy import linalg
import sexpdata

import constants
import inlining_constants
import inlining_overrides
import inlining_tree
import learn_problem
import learn_linear_general_reward
from learn_linear_general_reward import HyperParameters
import sexp_utils


parser = argparse.ArgumentParser(description="formulate the problem")
parser.add_argument("--experiment-dir", type=str, help="experiment dir")
parser.add_argument("--problem-dir", type=str, help="problem dir")
group1 = parser.add_mutually_exclusive_group(required=True)
group1.add_argument(
        "--opt-info", action="store_true",
        help="dump about how much the model has done something")
group1.add_argument(
        "--optimal-decision", action="store_true",
        help="dump about how much the model has done something")
group1.add_argument(
        "--inspect-run", type=int, default=None,
        help="dump about how much the model has done something")


def choose_left(a, b):
    if a is None:
        return False
    elif b is None:
        return True
    else:
        return a > b


def neg_inf_if_none(a):
    if a is None:
        return -np.inf
    else:
        return a


def get_optimal_decisions(tree, hyperparams, optimal_decisions):
    assert isinstance(tree, inlining_tree.Node)
    assert isinstance(optimal_decisions, list)

    children_optimal_decisions = []

    if len(tree.children) > 0:
        children_value_acc = 0.0
        for child in tree.children:
            children_value_acc += get_optimal_decisions(
                    child, hyperparams, children_optimal_decisions)

        children_value = \
                hyperparams.decay_factor * children_value_acc \
                / float(len(tree.children))
    else:
        children_value = 0.0

    assert tree.value[0] is not None or tree.value[1] is not None
    if not tree.name.is_apply_node():
        assert tree.value[0] is not None
        assert tree.value[1] is None

    lhs_value = neg_inf_if_none(tree.value[0]) + children_value
    rhs_value = neg_inf_if_none(tree.value[1])

    if len(tree.name.trace) == 0 or not tree.name.is_apply_node():
        optimal_decisions.extend(children_optimal_decisions)
        return lhs_value

    if lhs_value > rhs_value:
        optimal_decisions.append((tree.name, constants.INLINE))
        optimal_decisions.extend(children_optimal_decisions)
        return lhs_value
    else:
        optimal_decisions.append((tree.name, constants.DONT_INLINE))
        return rhs_value


def project_benefit_tree(
        root, hyperparams, id_to_tree_path, adjacency_list, contributions, mask, indent=0):
    space = " " * indent
    assert not (mask[2 * root] and mask[2 * root + 1])
    if mask[2 * root]:
        logging.info(
                "%sLooking into %d(%s)" % (space, root, id_to_tree_path[root]))
        base = contributions[2 * root]
        acc = 0.0
        num_children = len(adjacency_list[root])
        for child in adjacency_list[root]:
            assert isinstance(child, int)
            child_value = project_benefit_tree(
                    child, hyperparams, id_to_tree_path, adjacency_list,
                    contributions, mask, indent=indent+1)
            if child_value is None:
                num_children -= 1
            else:
                acc += child_value
        if num_children:
            return base + (hyperparams.decay_factor * acc / float(num_children))
        else:
            return base
    elif mask[2 * root + 1]:
        logging.info(
                "%sTerminating at %d(%s)" % (space, root, id_to_tree_path[root]))
        return contributions[2 * root + 1]
    else:
        logging.info(
                "%sFailed to project benefit at %d(%s)" % (space, root, id_to_tree_path[root]))
        return contributions[2 * root + 1]


def main():
    logging.getLogger().setLevel(logging.INFO)
    args = parser.parse_args()
    hyperparams_path = os.path.join(args.experiment_dir, "hyperparams.pkl")

    problem = inlining_tree.Problem.load(args.problem_dir)
    with open(hyperparams_path, "rb") as f:
        hyperparams = pickle.load(f)

    problem_matrices = learn_problem.construct_problem_matrices(
            problem, hyperparams)
    target_benefit = learn_linear_general_reward.construct_benefit_from_exec_time(
            problem.execution_times)
    num_nodes = problem_matrices.participation_mask.shape[1] / 2
    participation_count = np.sum(problem_matrices.participation_mask, axis=0)
    w, residual, _, _ = np.load(
            os.path.join(args.experiment_dir, "contributions.npy"))

    if args.opt_info:
        A = problem_matrices.benefit_relations
        squared_errors = np.power(target_benefit - np.matmul(A, w), 2)
        mse = np.mean(squared_errors)
        projected_benefits = np.matmul(A, w)

        print "Mean squared error:", mse
        print "Mimimum projected:", min(projected_benefits)
        print "Maximum projected:", max(projected_benefits)
        print "Mimimum error:", min(squared_errors)
        print "Maximum error:", max(squared_errors)

        obtained = np.matmul(A, w)
        target = target_benefit

    elif args.optimal_decision:

        def fill_node_values(node):
            node_id = node.name
            if participation_count[node_id * 2] > 0:
                lhs = w[node_id * 2]
            else:
                lhs = None
            if participation_count[node_id * 2 + 1] > 0:
                rhs = w[node_id * 2 + 1]
            else:
                rhs = None
            return (node.name, (lhs, rhs))

        def rename_id_to_path(node):
            return (id_to_tree_path[node.name], node.value)

        tree_path_to_ids = problem.properties.tree_path_to_ids
        id_to_tree_path = {v: k for k, v in tree_path_to_ids.iteritems()}
        adjacency_list = inlining_tree.adjacency_list_from_edge_lists(
                num_nodes=num_nodes,
                edge_lists=problem.edges_lists)
        root = tree_path_to_ids[inlining_tree.Path([])]
        tree = inlining_tree.build_from_adjacency_list(
                [None] * num_nodes, root, adjacency_list)
        tree = tree.map(f=fill_node_values)
        tree = tree.map(f=rename_id_to_path)
        optimal_decisions = []
        value = get_optimal_decisions(tree, hyperparams, optimal_decisions)
        decisions_sexp = [
                inlining_overrides.build_from_path_and_decision(path, decision)
                for path, decision in optimal_decisions
        ]
        logging.info("Optimal decision has a value of %f" % value)
        sexp_buffer = StringIO.StringIO()
        sexp_utils.dump_without_quotes(sexp_buffer, decisions_sexp)
        print(sexp_buffer.getvalue())

    elif args.inspect_run:
        index = args.inspect_run

        adjacency_list = inlining_tree.adjacency_list_from_edge_lists(
                num_nodes=num_nodes,
                edge_lists=problem.edges_lists)
        tree_path_to_ids = problem.properties.tree_path_to_ids
        id_to_tree_path = {v: k for k, v in tree_path_to_ids.iteritems()}

        A = problem_matrices.benefit_relations
        target_benefit = target_benefit[index]
        projected_benefit = np.matmul(A, w)[index]
        participation_mask = problem_matrices.participation_mask[index, :]
        assert participation_mask.shape == (num_nodes * 2,)
        projected_benefit_with_dfs = project_benefit_tree(
                root=tree_path_to_ids[inlining_tree.Path([])],
                hyperparams=hyperparams,
                adjacency_list=adjacency_list,
                id_to_tree_path=id_to_tree_path,
                contributions=w,
                mask=participation_mask)

        print "--- Information on run %d ---" % index
        print "Execution directory =", problem.execution_directories[index]
        print "Target benefit =", target_benefit
        print "Projected benefit (with matmul) =", projected_benefit
        print "Projected benefit (with DFS) =", projected_benefit_with_dfs

    else:
        assert False


if __name__ == "__main__":
    main()
