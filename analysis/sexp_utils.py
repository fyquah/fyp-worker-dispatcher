import inlining_tree

def dump_without_quotes(output, sexp):
    if inlining_tree.is_atom(sexp):
        output.write(inlining_tree.unpack_atom(sexp))
    elif isinstance(sexp, list):
        output.write("(")
        for i, child in enumerate(sexp):
            dump_without_quotes(output, child)
            if i != len(sexp) - 1:
                output.write(" ")
        output.write(")")
    else:
        print(type(sexp))
        assert False
