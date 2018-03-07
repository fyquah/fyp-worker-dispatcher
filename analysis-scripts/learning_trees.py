import asyncio
import collections
import concurrent.futures
import csv
import os
import pickle
import sys
import shutil
import subprocess

import aiofiles
import numpy as np
import sexpdata
import scipy.sparse

import inlining_tree


def remove_brackets_from_sexp(sexp):
    def flatten(s):
        if isinstance(s, list):
            return "".join(flatten(x) for x in s)
        else:
            return inlining_tree.unpack_atom(s)


    assert not(isinstance(sexp, sexpdata.Bracket))

    if isinstance(sexp, list):
        ret = []
        for child in sexp:
            if isinstance(child, sexpdata.Bracket):
                ret[-1] = inlining_tree.unpack_atom(ret[-1]) + flatten(child.value())
            else:
                ret.append(remove_brackets_from_sexp(child))
        return ret

    else:
        return sexp


def build_tree_from_str(s):
    s = s.decode("utf-8")
    return inlining_tree.top_level_of_sexp(remove_brackets_from_sexp(sexpdata.loads(s)))

sem = asyncio.Semaphore(4)


async def async_load_tree_from_rundir(substep_dir):
    print("Loading tree from %s" % substep_dir)
    substep_tmp_dir = os.path.join(substep_dir, "tmp")

    if not os.path.exists(substep_tmp_dir):
        os.mkdir(substep_tmp_dir)

    with open(os.devnull, 'w') as FNULL:
        async with sem:
            print("Created tar process for", substep_dir)
            proc = await asyncio.subprocess.create_subprocess_exec("tar", 
                "xzvf",
                os.path.join(substep_dir, "artifacts.tar"),
                "-C", substep_tmp_dir,
                stdout=FNULL, stderr=FNULL)
            print("Waiting on tar process for", substep_dir)
            _stdout, _stderr = await proc.communicate()

    data_collector_file = os.path.join(
            substep_tmp_dir, "main.0.data_collector.v1.sexp")

    if not os.path.exists(data_collector_file):
        return None

    async with sem:
        proc = await asyncio.subprocess.create_subprocess_exec(
            "../_build/default/tools/tree_tools.exe", "v1",
            "decisions-to-tree",
            data_collector_file,
            "-output", "/dev/stdout",
            stdout=subprocess.PIPE)
        tree = build_tree_from_str(await proc.stdout.read())
        await proc.wait()

    shutil.rmtree(substep_tmp_dir)
    print("Done with %s" % substep_dir)
    return tree


def iterate_rundirs(rundirs):
    for rundir in rundirs:
        opt_data_dir = os.path.join(rundir, "opt_data")

        # initial (can have up to 9 sub steps)
        for substep in range(0, 9):
            substep_dir = os.path.join(opt_data_dir, "initial", str(substep))
            if not os.path.exists(substep_dir):
                continue
            yield async_load_tree_from_rundir(substep_dir)

        # parse every step
        for step in range(0, 299):
            for substep in range(0, 3):
                substep_dir = os.path.join(
                        opt_data_dir, str(step), str(substep))
                if not os.path.exists(substep_dir):
                    continue
                yield async_load_tree_from_rundir(substep_dir)


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
            tree.value.function.closure_origin.id(),
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
            tree.value.function.closure_origin.id(),
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


ProblemProperties = collections.namedtuple("ProblemProperties", [
    "depth", "tree_path_to_ids"])


class Problem(object):

    def __init__(self, tree_path_to_ids, matrices):
        self.properties = ProblemProperties(
                depth=len(matrices), tree_path_to_ids=tree_path_to_ids
        )
        self.matrices = matrices

    def dump(self, directory):
        with open(os.path.join(directory, "properties.pkl"), "wb") as f:
            pickle.dump(self.properties, f)

        for i, matrix in self.matrices.items():
            dense_matrix = matrix.todense()
            np.save(
                    os.path.join(directory, ("adjacency_matrix_%d.npz" % i)),
                    dense_matrix,
            )


def trees_to_sparse_matrices(raw_trees):

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
        filling_sparse_matrices(tree, tree_path_to_ids, matrices, level=0)

    return Problem(tree_path_to_ids=tree_path_to_ids, matrices=matrices)


def main():
    rundirs = []
    with open("../important-logs/batch_executor.log") as batch_f:
        for line in csv.reader(batch_f):
            (script_name, rundir) = line
            if script_name == "./experiment-scripts/simulated-annealing-lexifi-g2pp_benchmark":
                rundir = "/media/usb" + rundir
                print(rundir)
                if os.path.exists(rundir):
                    rundirs.append(rundir)

    tasks = []
    for task in iterate_rundirs(rundirs):
        tasks.append(task)
    #    if len(tasks) >= 10:
    #        break

    trees = []
    loop = asyncio.get_event_loop()
    done, pending = loop.run_until_complete(asyncio.wait(tasks))
    assert len(pending) == 0
    for task in done:
        tree = task.result()
        if tree is not None:
            trees.append(tree)
    loop.close()

    problem = trees_to_sparse_matrices(trees)
    problem.dump("lexifi")


if __name__  == "__main__":
    main()
