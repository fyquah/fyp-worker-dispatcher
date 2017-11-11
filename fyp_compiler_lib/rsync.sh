#!/bin/bash

cd $(dirname "$0")
OCAML_DIR="../../ocaml"

cp $OCAML_DIR/sexp/sexp_file.ml* ./
cp $OCAML_DIR/sexp/sexp.ml* ./
cp $OCAML_DIR/sexp/sexp_lexer.mll ./
cp $OCAML_DIR/sexp/sexp_parser.mly ./
cp $OCAML_DIR/utils/helper.ml* ./
cp $OCAML_DIR/utils/identifiable.ml* ./
cp $OCAML_DIR/utils/misc.ml* ./
cp $OCAML_DIR/middle_end/data_collector.ml* ./
cp $OCAML_DIR/middle_end/base_types/call_site.ml* ./
cp $OCAML_DIR/middle_end/base_types/closure_id.ml* ./
cp $OCAML_DIR/middle_end/base_types/closure_element.ml* ./
cp $OCAML_DIR/middle_end/base_types/variable.ml* ./
cp $OCAML_DIR/middle_end/base_types/compilation_unit.ml* ./
cp $OCAML_DIR/middle_end/base_types/linkage_name.ml* ./
cp $OCAML_DIR/middle_end/feature_extractor.ml* ./
cp $OCAML_DIR/typing/ident.ml* ./
