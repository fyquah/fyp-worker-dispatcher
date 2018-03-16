#!/bin/bash

set -euo pipefail
set -x

BIN="python ./extract_data_from_experiments.py"

$BIN --script-name ./experiment-scripts/simulated-annealing-almabench              --exp-subdir almabench               --bin-name almabench              --output-dir ./out/almabench
$BIN --script-name ./experiment-scripts/simulated-annealing-bdd                    --exp-subdir bdd_benchmark           --bin-name bdd                    --output-dir ./out/bdd
$BIN --script-name ./experiment-scripts/simulated-annealing-fft                    --exp-subdir fft                     --bin-name fft                    --output-dir ./out/fft
$BIN --script-name ./experiment-scripts/simulated-annealing-floats-in-functor      --exp-subdir floats-in-functor       --bin-name b                      --output-dir ./out/floats-in-functor
$BIN --script-name ./experiment-scripts/simulated-annealing-hamming                --exp-subdir hamming                 --bin-name hamming                --output-dir ./out/hamming
$BIN --script-name ./experiment-scripts/simulated-annealing-kahan-sum              --exp-subdir kahan_sum               --bin-name kahan_sum              --output-dir ./out/kahan-sum
$BIN --script-name ./experiment-scripts/simulated-annealing-kb                     --exp-subdir kb_benchmark            --bin-name kb                     --output-dir ./out/kb
$BIN --script-name ./experiment-scripts/simulated-annealing-lens-benchmark         --exp-subdir lens_benchmark          --bin-name lens_benchmark         --output-dir ./out/lens-benchmark
$BIN --script-name ./experiment-scripts/simulated-annealing-lexifi-g2pp_benchmark  --exp-subdir lexifi-g2pp_benchmark   --bin-name main                   --output-dir ./out/lexifi
$BIN --script-name ./experiment-scripts/simulated-annealing-quicksort              --exp-subdir quicksort               --bin-name quicksort              --output-dir ./out/quicksort
$BIN --script-name ./experiment-scripts/simulated-annealing-sequence_benchmark     --exp-subdir sequence_benchmark      --bin-name sequence_benchmark     --output-dir ./out/sequence
$BIN --script-name ./experiment-scripts/simulated-annealing-sequence_cps_benchmark --exp-subdir sequence_cps_benchmark  --bin-name sequence_cps_benchmark --output-dir ./out/sequence-cps