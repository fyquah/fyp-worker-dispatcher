# This file trains the best models available

HERE=~/fyp/worker-dispatcher/tools/fyp_compiler_plugins

scp -P 3022 fyquah@82.28.62.7:$HERE/build/\{familiarity_graph.bin,familiarity_checkpoint.bin,familiarity_normaliser.bin,decision_graph.bin,decision_checkpoint.bin,decision_normaliser.bin\} $(dirname $0)/build/
