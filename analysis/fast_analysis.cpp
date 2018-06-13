#include <assert.h>
#include <stdint.h>
#include <iostream>

#include <vector>
#include <string>
#include <stack>

#include <numpy/arrayobject.h>
#include <tbb/tbb.h>

#ifdef __CPLUSPLUS
extern "C" {
#endif

static PyObject *
pairwise_l2_diff(PyObject *unused_dummy, PyObject * args)
{
  PyArrayObject * matrix;

  int ok = PyArg_ParseTuple(args, "O", &matrix);
  if (!ok) {
    return NULL;
  }

  npy_intp *shape = PyArray_SHAPE(matrix);
  const long num_examples = shape[0];
  const long num_features = shape[1];
  const long row_stride = PyArray_STRIDE(matrix, 0);
  const long col_stride = PyArray_STRIDE(matrix, 1);
  void *const matrix_base_ptr = PyArray_BYTES(matrix);

  npy_intp ret_dim[] = {num_examples, num_examples};
  int typenum = PyArray_TYPE(matrix);
  PyObject *ret = PyArray_SimpleNew(2, ret_dim, NPY_DOUBLE);
  void *const ret_base_ptr  = PyArray_BYTES(ret);
  const long ret_row_stride = PyArray_STRIDE(ret, 0);
  const long ret_col_stride = PyArray_STRIDE(ret, 1);

  auto get = [&](const long i, const long j) -> double {
    void *ptr = matrix_base_ptr + i * row_stride + j * col_stride;
    PyObject * o = PyArray_GETITEM(matrix, ptr);
    double x = PyFloat_AsDouble(o);
    Py_DECREF(o);
    return x;
  };
  auto set_ret = [&](const long i, const long j, const double value) -> void {
    void *ptr = ret_base_ptr + i * ret_row_stride + j * ret_col_stride;
    PyArray_SETITEM(ret, ptr, PyFloat_FromDouble(value));
  };

  for (long l = 0; l < num_examples ; l++) {
    set_ret(l, l, 0.0);
    for (long r = l + 1; r < num_examples ; r++) {

      /* euclidean diff */
      double diff = 0.0;
      for (long i = 0 ; i < num_features ; i++) {
        diff += std::pow(get(l, i) - get(r, i), 2.0);
      }

      /* write back */
      set_ret(l, r, diff);
      set_ret(r, l, diff);
    }
  }

  return ret;
}

static PyMethodDef methods[] = {
  {"pairwise_l2_diff",
    pairwise_l2_diff,
    METH_VARARGS,
    "Constructs the diff betweenparis of feature vectors. Takes a matrix of dimension NUM_EXAMPLES X NUM_FEATURES"},
  {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC
initfast_analysis(void)
{
  (void) Py_InitModule("fast_analysis", methods);
  import_array();
}

#ifdef __CPLUSPLUS
}  // extern "C"
#endif
