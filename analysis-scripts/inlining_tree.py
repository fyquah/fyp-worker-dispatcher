import collections
import contextlib
import logging
import os
import pickle
import threading
import subprocess

import numpy as np
import scipy.sparse
import shutil
import sexpdata


NodeBase = collections.namedtuple("NodeBase", ["name", "value", "children"])
Compilation_unit_base = collections.namedtuple("Compilation_unit",
        ["ident", "linkage_name"])
Variable_base = collections.namedtuple("Variable",
        ["kind", "compilation_unit", "name", "stamp"])
Function_metadata_base = collections.namedtuple("Function_metadata",
        ["closure_id", "set_of_closure_id", "closure_origin"])
Function_call_base = collections.namedtuple("Function_call",
        ["function", "apply_id"])
Apply_id_base = collections.namedtuple("Apply_id",
        ["compilation_unit", "stamp"])
Apply_stamp_base = collections.namedtuple("Apply_stamp_base",
        ["kind", "stamp"])


class Node(NodeBase):

    def map(self, f):
        name, value = f(self)
        children = [child.map(f) for child in self.children]
        return Node(name=name, value=value, children=children)

    def iter(self, f):
        f(self)
        for child in self.children:
            child.iter(f)


class Apply_stamp(Apply_stamp_base):

    def __str__(self):
        return "%s[%s]" % (str(self.kind), str(self.stamp))


class Apply_id(Apply_id_base):

    def pprint(self, fp, prefix):
        fp.write("%s | compilation_unit = %s\n" % (prefix, str(self.compilation_unit)))
        fp.write("%s | apply stamp = %s\n" % (prefix, str(self.stamp)))

    def id(self):
        return "%s__%s"% (self.compilation_unit.linkage_name, str(self.stamp.stamp))


class Compilation_unit(Compilation_unit_base):

    def pprint(self, fp, prefix):
        fp.write("%s | ident = %s\n" % (prefix, str(self.ident)))
        fp.write("%s | linkage_name = %s\n" % (prefix, str(self.linkage_name)))

    def __str__(self):
        return "%s/%s" % (self.linkage_name, self.ident)


class Variable(Variable_base):

    def __str__(self):
        return "%s/%s/%s" % (
                self.compilation_unit.linkage_name,
                str(self.name),
                str(self.stamp)
        )

    def id(self):
        return "%s__%s" % (
                self.compilation_unit.linkage_name,
                str(self.name),  # once again, bug with closure origin
        )


class Function_metadata(Function_metadata_base):

    def pprint(self, fp, prefix):
        fp.write("%s | closure_id = %s\n" % (prefix, str(self.closure_id)))
        fp.write("%s | set_of_closure_id = %s\n" % (prefix, str(self.set_of_closure_id)))
        fp.write("%s | closure_origin = %s\n" % (prefix, str(self.closure_origin)))


class Function_call(Function_call_base):

    def pprint(self, fp, prefix):
        fp.write("%s | function:\n" % prefix)
        self.function.pprint(fp, prefix + "  ")
        fp.write("%s | apply_id:\n" % prefix)
        self.apply_id.pprint(fp, prefix + "  ")


def unpack_atom(atom):
    if isinstance(atom, unicode):
        return str(atom)
    elif isinstance(atom, str) or isinstance(atom, int):
        return atom
    elif isinstance(atom, sexpdata.Symbol):
        return atom.value()
    elif isinstance(atom, sexpdata.Quoted):
        return atom.value()
    else:
        assert False


def sexp_to_map(sexp):
    assert isinstance(sexp, list)
    ret = {}
    for (a, b) in sexp:
        assert is_atom(a)
        ret[unpack_atom(a)] = b
    return ret

def is_atom(sexp):
    return isinstance(sexp, str) \
            or isinstance(sexp, sexpdata.Symbol) \
            or isinstance(sexp, int)


def compilation_unit_of_sexp(sexp):
    assert len(sexp) == 2
    return Compilation_unit(ident=unpack_atom(sexp[0]), linkage_name=unpack_atom(sexp[1]))
    

def variable_of_sexp(kind, sexp):
    assert len(sexp) == 3
    return Variable(
            kind= kind,
            compilation_unit= compilation_unit_of_sexp(sexp[0]),
            name= unpack_atom(sexp[1]),
            stamp= unpack_atom(sexp[2]),
    )


def closure_id_of_sexp(sexp):
    return variable_of_sexp("closure_id", sexp)


def closure_origin_of_sexp(sexp):
    return variable_of_sexp("closure_origin", sexp)


def set_of_closure_id_of_sexp(sexp):
    raise NotImplementedError(
            "[set_of_closure_id_of_sexp] has not been implemented")


def option_of_sexp(sexp, f):
    assert isinstance(sexp, list)
    if len(sexp) == 0:
        return None
    else:
        assert len(sexp) == 1
        return f(sexp[0])


def apply_id_stamp_of_sexp(sexp):
    kind = unpack_atom(sexp[0])
    if kind == "Plain_apply" or kind == "Over_application":
        stamp = unpack_atom(sexp[1])
        return Apply_stamp(kind=kind, stamp=stamp)
    else:
        assert len(sexp) == 1
        return Apply_stamp(kind=kind, stamp=None)


def apply_id_of_sexp(sexp):
    compilation_unit = compilation_unit_of_sexp(sexp[0])
    stamp = apply_id_stamp_of_sexp(sexp[1])
    return Apply_id(
            compilation_unit=compilation_unit,
            stamp=stamp
    )


def function_metadata_of_sexp(sexp):
    assert isinstance(sexp, list)
    assert len(sexp) == 3

    # TODO: fix this
    # option_closure_id = option_of_sexp(sexp[0], f=closure_id_of_sexp)
    # option_set_of_closure_id = option_of_sexp(sexp[1], f=set_of_closure_id_of_sexp)
    option_closure_id = None
    option_set_of_closure_id = None
    closure_origin = closure_origin_of_sexp(sexp[2])

    return Function_metadata(
            closure_id=option_closure_id,
            set_of_closure_id=option_set_of_closure_id,
            closure_origin=closure_origin
    )


def declaration_of_sexp(sexp):
    m = sexp_to_map(sexp)
    name = "Declaration"
    value = function_metadata_of_sexp(m["declared"])
    children = [inlining_tree_of_sexp(child) for child in m["children"]]
    return Node(name=name, value=value, children=children)


def inlined_of_sexp(sexp):
    m = sexp_to_map(sexp)
    name = "Inlined"
    value = Function_call(
            function=function_metadata_of_sexp(m["applied"]),
            apply_id=apply_id_of_sexp(m["apply_id"]))
    children = [inlining_tree_of_sexp(child) for child in m["children"]]
    return Node(name=name, value=value, children=children)


def non_inlined_of_sexp(sexp):
    m = sexp_to_map(sexp)
    name = "Non_inlined"
    value = Function_call(
            function=function_metadata_of_sexp(m["applied"]),
            apply_id=apply_id_of_sexp(m["apply_id"]))
    return Node(name=name, value=value, children=[])
    

# Subject to the function prototype
def inlining_tree_of_sexp(sexp):
    dispatch_table = {
            "Declaration": declaration_of_sexp,
            "Apply_inlined_function": inlined_of_sexp,
            "Apply_non_inlined_function": non_inlined_of_sexp,
    }
    return dispatch_table[unpack_atom(sexp[0])](sexp[1])


# Something
def top_level_of_sexp(sexp):
    children = [inlining_tree_of_sexp(s) for s in sexp]
    value = None
    name = "Top_level"
    return Node(name=name, value=value, children=children)


def pprint_tree(fp, tree, indent=0):
    spaces = str("--" * indent)
    fp.write("%s %s\n" % (spaces, tree.name))
    if tree.value is not None:
        if hasattr(tree.value, "pprint"):
            tree.value.pprint(fp, spaces)
        else:
            fp.write("%s | %s\n" % (spaces, str(tree.value)))
    for child in tree.children:
        pprint_tree(fp, child, indent=indent+1)


def max_depth(tree):
    if len(tree.children) == 0:
        return 1
    else:
        return 1 + max(max_depth(node) for node in tree.children)


class Path(object):

    def __init__(self, trace):
        self._trace = tuple(trace)

    @property
    def trace(self):
        return self._trace

    def __str__(self):
        ret = []
        for item in self._trace:
            if item[0] == "function":
                ret.append("<%s__%s>" % (item[1], item[2]))
            elif item[0] == "declaration":
                ret.append("{%s}" % item[1])
            else:
                raise RuntimeError("Unknown item[0] %s" % item[0])
        return "/".join(ret)

    def __eq__(self, other):
        return str(self) == str(other)

    def __hash__(self):
        return hash(str(self))

    def is_apply_node(self):
        return len(self._trace) >= 1 and self._trace[-1][0] == "function"


ProblemProperties = collections.namedtuple("ProblemProperties", [
    "depth", "tree_path_to_ids"])


class Problem(object):

    def __init__(self, tree_path_to_ids, matrices, node_labels, execution_times, edges_lists, execution_directories):
        self.properties = ProblemProperties(
                depth=len(matrices), tree_path_to_ids=tree_path_to_ids
        )
        self.node_labels = node_labels
        self.execution_times = execution_times
        self.matrices = matrices
        self.edges_lists = edges_lists
        self.execution_directories = execution_directories

    def dump(self, directory):
        with open(os.path.join(directory, "properties.pkl"), "wb") as f:
            pickle.dump(self.properties, f)
        with open(os.path.join(directory, "execution_directories.pkl"), "wb") as f:
            pickle.dump(self.execution_directories, f)
        with open(os.path.join(directory, "edges_lists.pkl"), "wb") as f:
            pickle.dump(self.edges_lists, f)

        for i, matrix in self.matrices.items():
            scipy.sparse.save_npz(
                    os.path.join(directory, ("adjacency_matrix_%d.npz" % i)),
                    matrix,
            )

        np.save(
                os.path.join(directory, "execution_times"),
                self.execution_times)
        np.save(
                os.path.join(directory, "node_labels"),
                self.node_labels)

    @classmethod
    def load(cls, directory):
        logging.info("Loading problem definitions and objects from %s" % directory)
        with open(os.path.join(directory, "properties.pkl"), "rb") as f:
            properties = pickle.load(f)
        with open(os.path.join(directory, "edges_lists.pkl"), "rb") as f:
            edges_lists = pickle.load(f)
        with open(os.path.join(directory, "execution_directories.pkl"), "rb") as f:
            execution_directories = pickle.load(f)
        matrices = []
        for i in range(properties.depth):
            matrix = scipy.sparse.load_npz(
                    os.path.join(directory, ("adjacency_matrix_%d.npz" % i)))
            matrices.append(matrix)
        execution_times = np.load(os.path.join(directory, "execution_times.npy"))
        # node_labels = np.load(os.path.join(directory, "node_labels.npy"))
        node_labels = None
        return cls(
                tree_path_to_ids=properties.tree_path_to_ids,
                matrices=matrices,
                node_labels=node_labels,
                execution_times=execution_times,
                edges_lists=edges_lists,
                execution_directories=execution_directories,
        )


def geometric_mean(times):
    p = 1
    for t in times:
        p = p * t
    return p ** (1.0 / len(times))


@contextlib.contextmanager
def in_temporary_directory(substep_tmp_dir):
    if not os.path.exists(substep_tmp_dir):
        os.mkdir(substep_tmp_dir)
    yield
    logging.info("Removed %s" % substep_tmp_dir)
    shutil.rmtree(substep_tmp_dir)


def remove_brackets_from_sexp(sexp):
    def flatten(s):
        if isinstance(s, list):
            return "".join(flatten(x) for x in s)
        else:
            return unpack_atom(s)


    assert not(isinstance(sexp, sexpdata.Bracket))

    if isinstance(sexp, list):
        ret = []
        for child in sexp:
            if isinstance(child, sexpdata.Bracket):
                ret[-1] = unpack_atom(ret[-1]) + flatten(child.value())
            else:
                ret.append(remove_brackets_from_sexp(child))
        return ret

    else:
        return sexp


def build_tree_from_str(s):
    s = s.decode("utf-8")
    try:
        return top_level_of_sexp(remove_brackets_from_sexp(sexpdata.loads(s)))
    except sexpdata.ExpectClosingBracket:
        return None


def parse_time(s):
    if s[-1] == 's':
        return float(s[:-1])
    elif s[-1] == 'm':
        return float(s[:-1]) * 60


def adjacency_list_from_edge_lists(num_nodes, edge_lists):
    adjacency_list = []
    for _ in range(num_nodes):
        adjacency_list.append(set())

    for edge_list in edge_lists:
        for edge in edge_list:
            adjacency_list[edge[0]].add((edge[1]))

    return adjacency_list


def build_from_adjacency_list(visited, root, adjacency_list):
    """
    builds a tree with node.value set to None
    """
    assert isinstance(root, int)
    assert not visited[root]

    visited[root] = True
    children = []
    for child in adjacency_list[root]:
        assert isinstance(child, int)
        child_node = build_from_adjacency_list(visited, child, adjacency_list)
        children.append(child_node)

    return Node(name=root, value=None, children=children)


def edge_list_to_adjacency_list():
    pass


def load_tree_from_rundir(substep_dir, bin_name):
    logging.info("Loading tree from %s" % substep_dir)
    substep_tmp_dir = os.path.join(substep_dir, "tmp")

    with in_temporary_directory(substep_tmp_dir):
        with open(os.devnull, 'w') as FNULL:
            logging.info("Created tar process for %s" % substep_dir)
            subprocess.call(["tar", 
                "xzvf",
                os.path.join(substep_dir, "artifacts.tar"),
                "-C", substep_tmp_dir],
                stdout=FNULL, stderr=FNULL)

        data_collector_file = os.path.join(
                substep_tmp_dir, bin_name + ".0.data_collector.v1.sexp")
        execution_stats_file = os.path.join(substep_dir, "execution_stats.sexp")

        if not os.path.exists(data_collector_file):
            logging.info("Dropping %s due to missing data collector file" % substep_dir)
            return None

        if not os.path.exists(execution_stats_file):
            logging.info("Dropping %s due to missing execution stats" % substep_dir)
            return None

        proc = subprocess.Popen([
            "../_build/default/tools/tree_tools.exe", "v1",
            "decisions-to-tree",
            data_collector_file,
            "-output", "/dev/stdout"], stdout=subprocess.PIPE)

        tree = build_tree_from_str(proc.stdout.read())
        if tree is None:
            logging.info("Dropping %s because cannot parse sexp correctly" % substep_dir)
            return None
        proc.wait()

        with open(execution_stats_file) as f:
            execution_stats_sexp = sexpdata.load(f)
            m = sexp_to_map(execution_stats_sexp)
            execution_time = geometric_mean([
                    parse_time(unpack_atom(x))
                    for x in m["raw_execution_time"]
            ])

        logging.info("Done with %s" % substep_dir)
    return (tree, execution_time)
