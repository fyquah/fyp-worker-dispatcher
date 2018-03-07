import collections

import sexpdata


Node = collections.namedtuple("Node", ["name", "value", "children"])
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


class Apply_stamp(Apply_stamp_base):

    def __str__(self):
        return "%s[%s]" % (str(self.kind), str(self.stamp))


class Apply_id(Apply_id_base):

    def pprint(self, fp, prefix):
        fp.write("%s | compilation_unit = %s\n" % (prefix, str(self.compilation_unit)))
        fp.write("%s | apply stamp = %s\n" % (prefix, str(self.stamp)))

    def id(self):
        return str(self.stamp.stamp)


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
        return "%s__%s_%s" % (
                self.compilation_unit.linkage_name,
                str(self.name),
                str(self.stamp)
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
    if isinstance(atom, str) or isinstance(atom, int):
        return atom
    elif isinstance(atom, sexpdata.Symbol):
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


def closure_id_of_sexp(sexp):
    return variable_of_sexp("closure_id_of_sexp", sexp)


def closure_origin_of_sexp(sexp):
    return variable_of_sexp("closure_origin", sexp)


def set_of_closure_id_of_sexp(sexp):
    raise NotImplementedError(
            "[set_of_closure_id_of_sexp] has not been implemented")


def option_of_sexp(sexp, *, f):
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
    option_closure_id = option_of_sexp(sexp[0], f=closure_id_of_sexp)
    option_set_of_closure_id = option_of_sexp(
            sexp[1], f=set_of_closure_id_of_sexp)
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
        tree.value.pprint(fp, spaces)
    for child in tree.children:
        pprint_tree(fp, child, indent=indent+1)


class Path(object):

    def __init__(self, trace):
        self._trace = tuple(trace)

    def __str__(self):
        ret = []
        for item in self._trace:
            if item[0] == "function":
                ret.append("%s<%s>" % (item[1], item[2]))
            else:
                ret.append("{%s}" % item[1])
        return "/".join(ret)

    def __eq__(self, other):
        return self._trace == other._trace

    def __hash__(self):
        return hash(str(self))
