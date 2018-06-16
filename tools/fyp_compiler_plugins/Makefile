OCAMLOPT=~/fyp/opam-root/4.06.0+fyp/bin/ocamlopt.opt 
OCAMLC=~/fyp/opam-root/4.06.0+fyp/bin/ocamlc.opt

all: build/plugin_v0.cmxs

build/familiarity_model.ml build/familiarity_weights.ml: build/familiarity_graph.bin
	cd build/ && python2 ../../../codegen/codegen.py \
	  --graph familiarity_graph.bin \
	  --checkpoint familiarity_checkpoint.bin \
	  --model-module   Familiarity_model \
	  --weights-module Familiarity_weights

build/familiarity_normaliser.ml: build/familiarity_normaliser.bin
	../../_build/default/tools/codegen_normaliser.exe $< >$@

build/decision_model.ml build/decision_weights.ml: build/decision_graph.bin
	cd build/ && python2 ../../../codegen/codegen.py \
	  --graph decision_graph.bin \
	  --checkpoint decision_checkpoint.bin \
	  --model-module   Decision_model \
	  --weights-module Decision_weights

build/decision_normaliser.ml: build/decision_normaliser.bin
	../../_build/default/tools/codegen_normaliser.exe $< >$@

build/plugin_v0.cmxs: \
  tf_lib.mli tf_lib.ml \
  import.ml feature_utils.mli feature_utils.ml \
  build/familiarity_weights_v0.ml build/familiarity_model_v0.ml build/familiarity_normaliser_v0.ml \
  build/decision_weights_v0.ml build/decision_model_v0.ml build/decision_normaliser_v0.ml \
  model_loader_v0.ml \
  manual_features_v1.mli manual_features_v1.ml \
  manual_features_v2.mli manual_features_v2.ml \
  manual_features_v3.mli manual_features_v3.ml \
  plugin_v0.ml
	echo "" >build/fyp_compiler_lib.ml
	cp model_loader_v0.ml build/model_loader.ml
	$(OCAMLC) -I build/ -I +compiler-libs -shared -o $(subst cmxs,cmo,$@) \
	  build/fyp_compiler_lib.ml \
	  $(subst model_loader_v0,build/model_loader,$^)
	$(OCAMLOPT) -I build/ -I +compiler-libs -shared -o $@ \
	  build/fyp_compiler_lib.ml \
	  $(subst model_loader_v0,build/model_loader,$^)

build/plugin_v0_logistic_regression.cmxs: \
  import.ml \
  feature_utils.mli feature_utils.ml \
  tf_lib.mli tf_lib.ml \
  build/familiarity_model_v0_logistic_regression.ml \
  build/decision_model_v0_logistic_regression.ml \
  model_loader_v0_logistic_regression.ml \
  manual_features_v1.mli manual_features_v1.ml \
  manual_features_v2.mli manual_features_v2.ml \
  manual_features_v3.mli manual_features_v3.ml \
  plugin_v0.ml
	echo "" >build/fyp_compiler_lib.ml
	cp model_loader_v0_logistic_regression.ml build/model_loader.ml
	$(OCAMLC) -I build/ -I +compiler-libs -shared -o $(subst cmxs,cmo,$@) \
	  build/fyp_compiler_lib.ml \
	  $(subst model_loader_v0_logistic_regression,build/model_loader,$^)
	$(OCAMLOPT) -I build/ -I +compiler-libs -shared -o $@ \
	  build/fyp_compiler_lib.ml \
	  $(subst model_loader_v0_logistic_regression,build/model_loader,$^)

build/plugin_v0_neural_network.cmxs: \
  import.ml \
  feature_utils.mli feature_utils.ml \
  tf_lib.mli tf_lib.ml \
  build/familiarity_model_v0_neural_network.ml \
  build/decision_model_v0_neural_network.ml \
  model_loader_v0_neural_network.ml \
  manual_features_v1.mli manual_features_v1.ml \
  manual_features_v2.mli manual_features_v2.ml \
  manual_features_v3.mli manual_features_v3.ml \
  plugin_v0.ml
	echo "" >build/fyp_compiler_lib.ml
	cp model_loader_v0_neural_network.ml build/model_loader.ml
	$(OCAMLC) -I build/ -I +compiler-libs -shared -o $(subst cmxs,cmo,$@) \
	  build/fyp_compiler_lib.ml \
	  $(subst model_loader_v0_neural_network,build/model_loader,$^)
	$(OCAMLOPT) -I build/ -I +compiler-libs -shared -o $@ \
	  build/fyp_compiler_lib.ml \
	  $(subst model_loader_v0_neural_network,build/model_loader,$^)

build/plugin_v0_clustered_neural_network.cmxs: \
  import.ml \
  feature_utils.mli feature_utils.ml \
  tf_lib.mli tf_lib.ml \
  build/familiarity_model_v0_clustered_neural_network.ml \
  build/decision_model_v0_clustered_neural_network.ml \
  model_loader_v0_clustered_neural_network.ml \
  manual_features_v1.mli manual_features_v1.ml \
  manual_features_v2.mli manual_features_v2.ml \
  manual_features_v3.mli manual_features_v3.ml \
  plugin_v0.ml
	echo "" >build/fyp_compiler_lib.ml
	cp model_loader_v0_clustered_neural_network.ml build/model_loader.ml
	$(OCAMLC) -I build/ -I +compiler-libs -shared -o $(subst cmxs,cmo,$@) \
	  build/fyp_compiler_lib.ml \
	  $(subst model_loader_v0_clustered_neural_network,build/model_loader,$^)
	$(OCAMLOPT) -I build/ -I +compiler-libs -shared -o $@ \
	  build/fyp_compiler_lib.ml \
	  $(subst model_loader_v0_clustered_neural_network,build/model_loader,$^)

build/plugin_v1_neural_network.cmxs: \
  import.ml \
  feature_utils.mli feature_utils.ml \
  tf_lib.mli tf_lib.ml \
  build/decision_model_v1_neural_network.ml \
  build/decision_model_v1_neural_network.ml \
  model_loader_v1_neural_network.ml \
  manual_features_v1.mli manual_features_v1.ml \
  manual_features_v2.mli manual_features_v2.ml \
  manual_features_v3.mli manual_features_v3.ml \
  plugin_v1.ml
	echo "" >build/fyp_compiler_lib.ml
	cp model_loader_v1_neural_network.ml build/model_loader.ml
	$(OCAMLC) -I build/ -I +compiler-libs -c -o $(subst cmxs,cmo,$@) \
	  build/fyp_compiler_lib.ml \
	  $(subst model_loader_v1_neural_network,build/model_loader,$^)
	$(OCAMLOPT) -I build/ -I +compiler-libs -shared -o $@ \
	  build/fyp_compiler_lib.ml \
	  $(subst model_loader_v1_neural_network,build/model_loader,$^)

clean:
	-rm build/*.cmxs build/*.ml build/*.mli rm *.cm*
