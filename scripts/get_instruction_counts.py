#!/usr/bin/env python3

import os
import sys

import py_common

bash_commands = []

for line in sys.stdin:
    script_name, exp_dir = line.strip().split(",")
    script_name = os.path.basename(script_name)
    bin_name = None
    try:
        bin_name = py_common.SCRIPT_TO_BIN_NAME[script_name]
    except KeyError:
        if os.path.basename(script_name.split(" ")[0]) == "simulated-annealing-generic":
            exp_name = script_name.split(" ")[1]
            bin_name = py_common.EXPERIMENT_TO_PARAMETERS[exp_name].bin_name
        else:
            assert False

    bash_commands.append(
            "./scripts/get_instruction_counts %s %s" %
            (bin_name, exp_dir))

for cmd in bash_commands:
    print(cmd)
