# This file trains the best models available
HERE=$(realpath $(dirname $0))

cd ../../
./_build/default/tools/local_reward_model.exe decision-model -feature-version V1 -epochs 200 -spec ./analysis-specs/ALL.sexp -hyperparams tmp/hyperparams-53.sexp -dump-graph $HERE/build/decision_graph.bin -checkpoint $HERE/build/decision_checkpoint.bin -dump-normaliser $HERE/build/decision_normaliser.bin
./_build/default/tools/local_reward_model.exe familiarity-model -feature-version V1 -epochs 200 -spec ./analysis-specs/ALL.sexp -hyperparams tmp/hyperparams-42.sexp -dump-graph $HERE/build/familiarity_graph.bin -checkpoint $HERE/build/familiarity_checkpoint.bin -dump-normaliser $HERE/build/familiarity_normaliser.bin
