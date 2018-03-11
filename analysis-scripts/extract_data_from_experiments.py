import argparse
import collections
import concurrent.futures
import csv
import os
import pickle
import sys
import shutil
import subprocess

import numpy as np
import sexpdata
import scipy.sparse

import inlining_tree


def iterate_rundirs(rundirs):
    for rundir in rundirs:
        opt_data_dir = os.path.join(rundir, "opt_data")

        # initial (can have up to 9 sub steps)
        for substep in range(0, 9):
            substep_dir = os.path.join(opt_data_dir, "initial", str(substep))
            if not os.path.exists(substep_dir):
                continue
            yield substep_dir

        # parse every step
        for step in range(0, 299):
            for substep in range(0, 3):
                substep_dir = os.path.join(
                        opt_data_dir, str(step), str(substep))
                if not os.path.exists(substep_dir):
                    continue
                yield substep_dir


def collect_unique_nodes(acc, trace, tree):
    assert isinstance(tree, inlining_tree.Node)

    if tree.name == "Top_level":
        assert trace == []
        acc.add(inlining_tree.Path([]))
        for child in tree.children:
            collect_unique_nodes(acc, trace, child)
    elif tree.name == "Inlined" or tree.name == "Non_inlined":
        new_trace = trace + [(
            "function",
            tree.value.apply_id.id()
        )]
        acc.add(inlining_tree.Path(new_trace))
        for child in tree.children:
            collect_unique_nodes(acc, new_trace, child)
    elif tree.name == "Declaration":
        new_trace = trace + [(
            "declaration",
            tree.value.closure_origin.id()
        )]
        acc.add(inlining_tree.Path(new_trace))
        for child in tree.children:
            collect_unique_nodes(acc, new_trace, child)
    else:
        print(tree.name)
        assert False


def relabel_to_paths(tree, trace):
    assert isinstance(tree, inlining_tree.Node)

    if tree.name == "Top_level":
        assert trace == []
        children = [relabel_to_paths(child, trace) for child in tree.children]
        return inlining_tree.Node(
                name=tree.name,
                children=children,
                value=inlining_tree.Path([])
        )
            
    elif tree.name == "Inlined" or tree.name == "Non_inlined":
        new_trace = trace + [(
            "function",
            tree.value.apply_id.id()
        )]
        children = [
                relabel_to_paths(child, new_trace)
                for child in tree.children
        ]
        return inlining_tree.Node(
                name=tree.name, children=children,
                value=inlining_tree.Path(new_trace)
        )

    elif tree.name == "Declaration":
        new_trace = trace + [(
            "declaration",
            tree.value.closure_origin.id()
        )]
        children = [
                relabel_to_paths(child, new_trace)
                for child in tree.children
        ]
        return inlining_tree.Node(
                name=tree.name, children=children,
                value=inlining_tree.Path(new_trace)
        )
    else:
        print(tree.name)
        assert False


def count_nodes(tree):
    acc = 1

    if tree.name == "Top_level":
        for child in tree.children:
            acc += count_nodes(child)

    elif tree.name == "Inlined" or tree.name == "Non_inlined":
        for child in tree.children:
            acc += count_nodes(child)
    elif tree.name == "Declaration":
        for child in tree.children:
            acc += count_nodes(child)
    else:
        print(tree.name)
        assert False

    return acc


def filling_sparse_matrices(tree, tree_path_to_ids, sparse_matrices, level):
    assert isinstance(tree, inlining_tree.Node)

    src_path = tree.value
    src = tree_path_to_ids[src_path]

    for child in tree.children:
        if child.name == "Inlined" or child.name == "Non_inlined":
            dest_path = child.value
            dest = tree_path_to_ids[dest_path]
            sparse_matrices[level][src, dest] += 1

        filling_sparse_matrices(child, tree_path_to_ids, sparse_matrices, level+1)


def filling_vertices(tree, tree_path_to_ids, vertices):
    assert isinstance(tree, inlining_tree.Node)

    src_path = tree.value
    src = tree_path_to_ids[src_path]

    index_dispatch = {
            "Inlined": 0,
            "Non_inlined": 1,
            "Declaration": 2,
            "Top_level":   3,
    }
    vertices[tree_path_to_ids[tree.value], index_dispatch[tree.name]] = 1
    for child in tree.children:
        filling_vertices(child, tree_path_to_ids, vertices)


def populate_edge_list(edge_list, tree_path_to_ids, tree):

    for child in tree.children:
        dest_path = child.value
        dest = tree_path_to_ids[dest_path]
        src = tree_path_to_ids[tree.value]
        edge_list.append((src, dest, child.name))

        populate_edge_list(edge_list, tree_path_to_ids, child)


def formulate_problem(raw_trees, execution_times):

    tree_paths = set()
    tree_path_to_ids = {}

    for tree in raw_trees:
        collect_unique_nodes(tree_paths, [], tree)

    for i, tree_path in enumerate(tree_paths):
        tree_path_to_ids[tree_path] = i

    trees_with_path_labels = [relabel_to_paths(tree, []) for tree in raw_trees]
    num_unique_paths = len(tree_paths)
    matrices = collections.defaultdict(
            lambda : scipy.sparse.lil_matrix(
                (num_unique_paths, num_unique_paths),
                dtype=np.int32)
    )

    for tree in trees_with_path_labels:
        vertices = np.zeros((len(tree_paths), 4))
        filling_sparse_matrices(tree, tree_path_to_ids, matrices, level=0)
        # filling_vertices(tree, tree_path_to_ids, vertices)
        # train_x.append(vertices)

    matrices = {k: scipy.sparse.csr_matrix(v) for k, v in matrices.items()}
    edge_lists = []

    for tree in trees_with_path_labels:
        edges = []
        populate_edge_list(edges, tree_path_to_ids, tree)
        edge_lists.append(edges)

    return inlining_tree.Problem(
            tree_path_to_ids=tree_path_to_ids,
            matrices=matrices,
            node_labels=None,
            execution_times=execution_times,
            edges_lists=edge_lists)

parser = argparse.ArgumentParser(description="formulate the problem")
parser.add_argument("--script-name", type=str, help="Name of script", required=True)
parser.add_argument("--output-dir", type=str, help="output dir", required=True)
parser.add_argument("--bin-name", type=str, help="output dir", required=True)


def main():
    args = parser.parse_args()
    rundirs = []
    with open("../important-logs/batch_executor.log") as batch_f:
        for line in csv.reader(batch_f):
            (script_name, sub_rundir) = line
            if script_name == args.script_name:
                rundir = "/media/usb" + sub_rundir
                if os.path.exists(rundir):
                    rundirs.append(rundir)

                rundir = "/media/usb2" + sub_rundir
                if os.path.exists(rundir):
                    rundirs.append(rundir)

    np.random.shuffle(rundirs)
    tasks = list(iterate_rundirs(rundirs))

    pool = concurrent.futures.ThreadPoolExecutor(8)
    futures = [
            pool.submit(inlining_tree.load_tree_from_rundir, task, args.bin_name)
            for task in tasks
    ]
    results = [r.result() for r in concurrent.futures.as_completed(futures)]
    results = [r for r in results if r is not None]

    # loop = asyncio.get_event_loop()
    # done, pending = loop.run_until_complete(asyncio.wait(tasks))
    # assert len(pending) == 0
    # for task in done:
    #     result = task.result()
    #     if result is not None:
    #         results.append(result)
    # loop.close()

    print("Loaded %d samples to train on" % len(results))
    trees, execution_times = zip(*results)

    problem = formulate_problem(trees, execution_times)
    problem.dump(args.output_dir)


if __name__  == "__main__":
    main()