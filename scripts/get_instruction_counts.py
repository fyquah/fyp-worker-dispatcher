#!/usr/bin/env python3

import os
import sys

MAPPING = {
        "simulated-annealing-almabench": "almabench",
        "simulated-annealing-bdd": "bdd",
        "simulated-annealing-floats-in-functor": "b",
        "simulated-annealing-kb": "kb",
        "simulated-annealing-lexifi-g2pp_benchmark": "main",
        "simulated-annealing-quicksort": "quicksort",
        "simulated-annealing-sequence_benchmark": "sequence_benchmark",
        "simulated-annealing-sequence_cps_benchmark": "sequence_cps_benchmark",

        "mcts-almabench": "almabench",
        "mcts-float_in_functor": "b",
        "mcts-sequence_benchmark": "sequence_benchmark",
}

bash_commands = []

for line in sys.stdin:
    script_name, exp_dir = line.strip().split(",")
    script_name = os.path.basename(script_name)
    bash_commands.append(
            "./scripts/get_instruction_counts %s %s" % (MAPPING[script_name],
                exp_dir))

for cmd in bash_commands:
    print(cmd)
