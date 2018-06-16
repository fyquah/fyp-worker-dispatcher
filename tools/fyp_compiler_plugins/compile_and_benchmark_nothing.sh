#!/bin/bash

make build/plugin_nothing.cmxs
mv build/plugin_v1_neural_network.cmxs build/plugin_nothing.cmxs
cd ../../
./scripts/run-plugin-benchmarks plugin_nothing
