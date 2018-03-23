"""
Translates decisions that can be generated as an override
"""

import sexpdata

import constants
import inlining_tree


def trace_item_from_path(component):
    print(component)
    if component[0] == "function":
        return [ "Apply", sexp_of_apply(component[1], component[1]) ]
    elif component[0] == "declaration":
        linkage_name, function_name, stamp = (str(x) for x in component[1].split("__"))
        return [ "Decl", sexp_of_decl() ]
    else:
        assert False


def build_from_path_and_decision(path, decision):
    assert path.trace[-1][0] == "function"
    assert isinstance(path, inlining_tree.Absolute_path)

    round = "0"
    apply_id_stamp = str(path.trace[-1][1].split("__")[1])
    trace = list(reversed([trace_item_from_path(c) for c in path.trace]))

    if decision == constants.INLINE:
        action = "Inline"
    elif decision == constants.DONT_INLINE:
        action = "Apply"
    else:
        assert False

    return [ round, apply_id_stamp, trace, action ]
