#include <assert.h>
#include <stdint.h>

#include <vector>
#include <string>
#include <stack>

#include <numpy/arrayobject.h>

namespace fyp
{

enum node_kind_t {
  KIND_Inlined,
  KIND_Top_level,
  KIND_Decl,
  KIND_Apply,
};

node_kind_t
kind_from_string(const char * c)
{
  if (strcmp(c, "Top_level") == 0) {
    return KIND_Top_level;
  } else if (strcmp(c, "Inlined") == 0) {
    return KIND_Inlined;
  } else if (strcmp(c, "Decl") == 0) {
    return KIND_Decl;
  } else if (strcmp(c, "Apply") == 0) {
    return KIND_Apply;
  } else {
    assert(false);
  }
}

struct stack_item_t
{
  long node;
  double factor;

  stack_item_t(const long node, const double factor)
    : node(node), factor(factor)
  {
  }
};

struct linear_relation_t {
  std::vector<int32_t> participation;
  std::vector<double> benefit_relation;

  linear_relation_t(
      const std::vector<int32_t> & participation,
      const std::vector<double>  & benefit_relation) :
        participation(participation), benefit_relation(benefit_relation)
  {}
};


static linear_relation_t
construct_linear_benefit_relation_impl(
    const long root,
    long num_nodes,
    const std::vector<std::vector<long>> & adjacency_list,
    const std::vector<node_kind_t> & node_kinds,
    double decay_factor,
    bool normalise_with_num_children
)
{
  std::vector<bool> visited(num_nodes, 0);

  linear_relation_t ret(
      std::vector<int32_t>(num_nodes * 2, 0),
      std::vector<double>(num_nodes * 2, 0)
  );

  std::stack<stack_item_t> s;
  s.push(stack_item_t(root, 1.0));

  while (s.size()) {
    auto tos = s.top();
    s.pop();

    const long node = tos.node;
    const double factor = tos.factor;
    const node_kind_t kind = node_kinds[tos.node];

    if (visited[tos.node]) {
      continue;
    }
    visited[node] = 1;

    if (kind == KIND_Inlined
          || kind == KIND_Decl
          || kind == KIND_Top_level) {
      ret.benefit_relation[2 * node] = factor;
      ret.participation[2 * node] = 1;

    } else if(kind == KIND_Apply) {
      ret.benefit_relation[2 * node + 1] = factor;
      ret.participation[2 * node + 1] = 1;

    } else {
      assert(false);
    }

    double num_children = double(adjacency_list[node].size());

    for (auto v : adjacency_list[node]) {
      if (normalise_with_num_children) {
        s.emplace(v, factor * decay_factor / num_children);
      } else {
        s.emplace(v, factor * decay_factor);
      }
    }
  }

  return ret;
}

}  // fyp

#ifdef __CPLUSPLUS
extern "C" {
#endif

static PyObject *
construct_linear_benefit_relation(PyObject *self, PyObject *args)
{
  long root;
  long num_nodes;
  PyObject *edge_list;
  double decay_factor;
  int normalise_with_num_children;
  PyObject *benefit_relation;
  PyObject *participation_mask;

  int ok = PyArg_ParseTuple(args, "llOdiOO", &root, &num_nodes,
      &edge_list, &decay_factor, &normalise_with_num_children,
      &benefit_relation, &participation_mask);
  if (!ok) {
    return NULL;
  }

  std::vector<std::vector<long> > adjacency_list(num_nodes);
  std::vector<fyp::node_kind_t> node_kinds(num_nodes);

  int len = PyList_Size(edge_list);
  for (int i = 0 ; i < len ; i++) {
    PyObject* edge = PyList_GetItem(edge_list, i);
    long u = PyInt_AsLong(PyTuple_GetItem(edge, 0));
    long v = PyInt_AsLong(PyTuple_GetItem(edge, 1));
    adjacency_list[u].push_back(v);

    const char *kind = PyString_AsString(PyTuple_GetItem(edge, 2));
    node_kinds[v] = fyp::kind_from_string(kind);
  }

  auto evaluated = fyp::construct_linear_benefit_relation_impl(
      root, num_nodes, adjacency_list, node_kinds,
      decay_factor, normalise_with_num_children);

  npy_intp stride = PyArray_STRIDE(benefit_relation, 0);
  void *dataptr = PyArray_BYTES(benefit_relation);
  for (int i = 0; i < 2 * num_nodes ; i++) {
    PyArray_SETITEM(
        benefit_relation,
        dataptr,
        PyFloat_FromDouble(evaluated.benefit_relation[i])
    );
    dataptr = dataptr + stride;
  }

  stride = PyArray_STRIDE(participation_mask, 0);
  dataptr = PyArray_BYTES(participation_mask);
  for (int i = 0; i < 2 * num_nodes ; i++) {
    PyArray_SETITEM(
        participation_mask,
        dataptr,
        PyFloat_FromDouble(evaluated.participation[i])
    );
    dataptr = dataptr + stride;
  }

  Py_RETURN_NONE;
}

static PyMethodDef fast_methods[] = {
  {"construct_linear_benefit_relation",
    construct_linear_benefit_relation,
    METH_VARARGS,
    "Constructs linear benefit relation fast"},
  {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC
initfast(void)
{
  (void) Py_InitModule("fast", fast_methods);
}

#ifdef __CPLUSPLUS
}  // extern "C"
#endif
