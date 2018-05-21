import argparse
import collections

import tensorflow as tf
from tensorflow.core.framework import graph_pb2

parser = argparse.ArgumentParser(description="codegen")
parser.add_argument("graph", type=str, help="pb file for TF graph")
parser.add_argument("--output", type=str, help="output of this")

_SYM = -1


def gensym():
    global _SYM
    _SYM += 1
    return "v%d" % _SYM


def float_array_from_tensor(tensor, name):
    assert len(tensor.tensor_shape.dim) == 0
    return str(tensor.float_val[0])


def typed_array_from_tensor(tensor, name):
    dtype = tensor.dtype


def shape_to_tuple(s):
    dims = []
    for d in s.dim:
        dims.append(d.size)
    return tuple(dims)


Argument = collections.namedtuple("Argument", ["name", "shape"])


class Codegen(object):

    def __init__(self, nodes):
        self._nodes = nodes
        self._body = []
        self._args = []

    def _body(self):
        return self._body

    def run(self, raw_node_name):
        name = raw_node_name.replace("-", "_").lower().split(":")[0]

        if raw_node_name[0] == "^":
            ret = self.run(str(raw_node_name[1:]))
            self._output.append("let %s = not %s in" % (name, ret))
            return

        node = self._nodes[raw_node_name.split(":")[0]]

        dep_names = []
        for input_node_name in node.input:
            dep_names.append(self.run(input_node_name))
        op_name = node.op

        # Regular Ops
        if node.op == "Variable":
            dims = node.attr["shape"].shape
            dims = [str(d.size) for d in dims.dim]
            dims = "; ".join(dims)
            self._body.append(
                    "let %s = get_variable \"%s\" [| %s |] in"
                    % (name, node.name, dims))

        elif node.op == "Const":
            c = typed_array_from_tensor(node.attr["value"].tensor, name=name)
            self._body.append("let %s = %s" % (name, c))

        elif node.op == "Placeholder":
            shape = shape_to_tuple(node.attr["shape"].shape)
            self._args.append(Argument(name=name, shape=shape))

        else:
            assert len(dep_names) > 0
            self._body.append("let %s = %s %s in" % (name, op_name, " ".join(dep_names)))
        return name



def main():
    args = parser.parse_args()
    graph_def = tf.GraphDef()
    with open(args.graph, "rb") as f:
        graph_def.ParseFromString(f.read())
    d = {}
    entry_point = None
    for node in graph_def.node:
        d[node.name] = node
        if node.name.startswith("network_output"):
            if entry_point is None:
                entry_point = node
            else:
                assert False
    codegen = Codegen(d)
    codegen.run(entry_point.name)
    print "\n".join(codegen.get_output())


if __name__ == "__main__":
    main()
