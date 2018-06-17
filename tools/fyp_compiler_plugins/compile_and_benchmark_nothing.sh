#!/bin/bash

make build/plugin_nothing.cmxs
cd ../../
./scripts/run-plugin-benchmarks plugin_nothing
