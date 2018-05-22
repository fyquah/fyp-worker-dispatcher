import sys
import gen_model
import StringIO

from tensorflow.python import pywrap_tensorflow

to_var_name = gen_model.to_var_name
print_float = gen_model.print_float


def print_array(f, indent, array):
    space = " " * indent
    f.write(space + "[| " + "; ".join(str(x) for x in array) + " |]")

def print_array2(f, indent, array2):
    space = " " * indent
    f.write(space + "[|\n")
    for array in array2:
        print_array(f, indent + 1, array)
        f.write(";\n")
    f.write(space + "|]\n")

def print_matrix(f, indent, array2):
    space = " " * indent
    f.write(space + "Tf_lib.Mat (\n")
    print_array2(f, indent + 2, array2)
    f.write(space + ")\n")

def print_vector(f, indent, array):
    space = " " * indent
    f.write(space + "Tf_lib.Vec (\n")
    print_array(f, indent + 2, array)
    f.write(space + "\n)\n")

def run(filename):
    reader = pywrap_tensorflow.NewCheckpointReader(filename)
    var_to_shape_map = reader.get_variable_to_shape_map()
    f = StringIO.StringIO()
    for name in sorted(var_to_shape_map):
        f.write("let %s = " % to_var_name(name))
        arr = reader.get_tensor(name)
        if len(arr.shape) == 0:
            f.write("Tf_lib.Scalar %s\n" % print_float(arr))
        elif len(arr.shape) == 1:
            print_vector(f, 0, arr)
        elif len(arr.shape) == 2:
            print_matrix(f, 0, arr)
        else:
            assert False
        f.write(";;\n\n")
    return f.getvalue()
