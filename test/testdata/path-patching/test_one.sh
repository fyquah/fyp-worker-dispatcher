#!/bin/bash

set -euo pipefail

# List of experiments

declare -A experiments_mapping
declare -A bin_name_mapping

experiments_mapping=(["kb-1"]="kb_benchmark")
bin_name_mapping=(["kb-1"]="kb")

experiments_mapping["lens-1"]="lens_benchmark"
bin_name_mapping=["lens-1"]="lens_benchmark"

experiments_mapping=["lexifi-1"]="lexifi-g2pp_benchmark"
bin_name_mapping=["lexifi-1"]="main"


PATH_TO_SEXP_FILE=$(readlink -f "$1.sexp")
CANDIDATE=$1
SCRATCH_DIR=$(mktemp -d)
EXPERIMENT_SUB_DIR="${experiments_mapping["$CANDIDATE"]}"
EXPERIMENT_BIN_NAME="${bin_name_mapping["$CANDIDATE"]}"
EXPERIMENT_DIR=~/fyp/experiments/normal/$EXPERIMENT_SUB_DIR

echo "Running in $SCRATCH_DIR for test for $PATH_TO_SEXP_FILE">/dev/stderr

cd $SCRATCH_DIR
cp $EXPERIMENT_DIR/* $SCRATCH_DIR/

sed -i'' -e 's#ocamlopt#~/fyp/ocaml-unstable-closure-origins/ocamlopt.opt -inlining-overrides overrides.sexp#g' Makefile

make clean
cp $PATH_TO_SEXP_FILE ./overrides.sexp

make >/dev/null

cd $SCRATCH_DIR
fyp-tree-tools v1 decisions-to-tree -output reference-tree.sexp \
  $EXPERIMENT_BIN_NAME.0.data_collector.v1.sexp
fyp-tree-tools v1 decisions-to-tree -output original-tree.sexp \
  $PATH_TO_SEXP_FILE

~/fyp/worker-dispatcher/_build/install/default/bin/data_cleaner \
  path-patching original-tree.sexp \
  -reference reference-tree.sexp \
  -output generated-tree.sexp

fyp-tree-tools v1 print-tree original-tree.sexp  >original-tree-pretty.sexp
fyp-tree-tools v1 print-tree reference-tree.sexp >reference-tree-pretty.sexp
fyp-tree-tools v1 print-tree generated-tree.sexp >generated-tree-pretty.sexp

echo "$(pwd)/generated.sexp"
