all: build/plugin_v0.cmxs

build/familiarity_model.ml build/familiarity_weights.ml: build/familiarity_graph.bin
	cd build/ && python2 ../../../codegen/codegen.py \
	  --graph familiarity_graph.bin \
	  --checkpoint familiarity_checkpoint.bin \
	  --model-module   Familiarity_model \
	  --weights-module Familiarity_weights

build/decision_model.ml build/decision_weights.ml: build/decision_graph.bin
	cd build/ && python2 ../../../codegen/codegen.py \
	  --graph decision_graph.bin \
	  --checkpoint decision_checkpoint.bin \
	  --model-module   Decision_model \
	  --weights-module Decision_weights

build/plugin_v0.cmxs: \
  tf_lib.mli tf_lib.ml \
  build/familiarity_weights.ml build/familiarity_model.ml \
  build/decision_weights.ml build/decision_model.ml \
  import.ml feature_utils.mli feature_utils.ml \
  manual_features_v1.mli manual_features_v1.ml \
  plugin_v0.ml
	echo "" >build/fyp_compiler_lib.ml
	~/fyp/ocaml/ocamlopt.opt -I build/ -I +compiler-libs -shared -o $@ \
	  build/fyp_compiler_lib.ml $^
