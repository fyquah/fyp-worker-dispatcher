all: build/plugin_v0.cmxs

build/familiarity_model.ml build/familiarity_weights.ml: build/familiarity_graph.bin
	cd build/ && python2 ../../codegen/codegen.py \
	  --graph familiarity_graph.bin \
	  --checkpoint familiarity_checkpoint.bin \
	  --model-module   Familiarity_model \
	  --weights-module Familiarity_weights

build/plugin_v0.cmxs: \
  tf_lib.mli tf_lib.ml \
  build/familiarity_weights.ml build/familiarity_model.ml \
  manual_features_v1.mli manual_features_v1.ml \
  plugin_v0.ml
	~/fyp/ocaml/ocamlopt.opt -I+compiler-libs -shared -o $@ $^
