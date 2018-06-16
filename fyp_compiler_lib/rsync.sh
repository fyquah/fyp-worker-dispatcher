#!/bin/bash

cd $(dirname "$0")
OCAML_DIR="../../ocaml"

cp $OCAML_DIR/sexp/sexp_file.ml* ./
cp $OCAML_DIR/sexp/sexp.ml* ./
cp $OCAML_DIR/sexp/sexp_lexer.mll ./
cp $OCAML_DIR/sexp/sexp_parser.mly ./
cp $OCAML_DIR/utils/arg_helper.ml* ./
cp $OCAML_DIR/utils/clflags.ml* ./
cp $OCAML_DIR/utils/helper.ml* ./
cp $OCAML_DIR/utils/identifiable.ml* ./
cp $OCAML_DIR/utils/misc.ml* ./
cp $OCAML_DIR/utils/config.ml* ./
cp $OCAML_DIR/utils/numbers.ml* ./
cp $OCAML_DIR/middle_end/allocated_const.ml* ./
cp $OCAML_DIR/middle_end/data_collector.ml* ./
cp $OCAML_DIR/middle_end/data_collector_intf.ml ./
cp $OCAML_DIR/middle_end/backend_intf.ml* ./
cp $OCAML_DIR/middle_end/parameter.ml* ./
cp $OCAML_DIR/middle_end/flambda_iterators.ml* ./
cp $OCAML_DIR/middle_end/flambda.ml* ./
cp $OCAML_DIR/middle_end/inlining_query.ml* ./
cp $OCAML_DIR/middle_end/inlining_cost.ml* ./
cp $OCAML_DIR/middle_end/base_types/apply_id.ml* ./
cp $OCAML_DIR/middle_end/base_types/call_site.ml* ./
cp $OCAML_DIR/middle_end/base_types/closure_id.ml* ./
cp $OCAML_DIR/middle_end/base_types/set_of_closures_origin.ml* ./
cp $OCAML_DIR/middle_end/base_types/closure_origin.mli ./
cp $OCAML_DIR/middle_end/base_types/real_closure_origin.mli ./
cp $OCAML_DIR/middle_end/base_types/export_id.ml* ./
# source file for closure_origin is modified because it doesn't compile on
# 4.05.0 otherwise.
cp $OCAML_DIR/middle_end/base_types/closure_element.ml* ./
cp $OCAML_DIR/middle_end/base_types/id_types.ml* ./
cp $OCAML_DIR/middle_end/base_types/static_exception.ml* ./
cp $OCAML_DIR/middle_end/base_types/variable.ml* ./
cp $OCAML_DIR/middle_end/base_types/var_within_closure.ml* ./
cp $OCAML_DIR/middle_end/base_types/compilation_unit.ml* ./
cp $OCAML_DIR/middle_end/base_types/linkage_name.ml* ./
cp $OCAML_DIR/middle_end/base_types/set_of_closures_id.ml* ./
cp $OCAML_DIR/middle_end/base_types/symbol.ml* ./
cp $OCAML_DIR/middle_end/base_types/tag.ml* ./

cp $OCAML_DIR/middle_end/feature_extractor.ml* ./
cp $OCAML_DIR/middle_end/projection.ml* ./
cp $OCAML_DIR/typing/ident.ml* ./
