#!/bin/bash

set -euo pipefail
set -x

python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-almabench              --exp-subdir almabench               --bin-name almabench              --output-dir ./out/almabench
python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-bdd                    --exp-subdir bdd_benchmark           --bin-name bdd                    --output-dir ./out/bdd
python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-fft                    --exp-subdir fft                     --bin-name fft                    --output-dir ./out/fft
python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-floats-in-functor      --exp-subdir floats-in-functor       --bin-name b                      --output-dir ./out/floats-in-functor
python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-hamming                --exp-subdir hamming                 --bin-name hamming                --output-dir ./out/hamming
python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-kahan-sum              --exp-subdir kahan_sum               --bin-name kahan_sum              --output-dir ./out/kahan-sum
python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-kb                     --exp-subdir kb_benchmark            --bin-name kb                     --output-dir ./out/kb
python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-lens-benchmark         --exp-subdir lens_benchmark          --bin-name lens_benchmark         --output-dir ./out/lens-benchmark
python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-lexifi-g2pp_benchmark  --exp-subdir lexifi-g2pp_benchmark   --bin-name main                   --output-dir ./out/lexifi
python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-quicksort              --exp-subdir quicksort               --bin-name quicksort              --output-dir ./out/quicksort
python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-sequence_benchmark     --exp-subdir sequence_benchmark      --bin-name sequence_benchmark     --output-dir ./out/sequence
python extract_data_from_experiments.py --script-name ./experiment-scripts/simulated-annealing-sequence_cps_benchmark --exp-subdir sequence_cps_benchmark  --bin-name sequence_cps_benchmark --output-dir ./out/sequence-cps
