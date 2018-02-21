#!/usr/bin/env python3

import os
import sys

import py_common

bash_commands = []

for line in sys.stdin:
    script_name, exp_dir = line.strip().split(",")
    script_name = os.path.basename(script_name)
    bash_commands.append(
            "./scripts/get_instruction_counts %s %s" %
            (py_common.SCRIPT_TO_BIN_NAME[script_name], exp_dir))

for cmd in bash_commands:
    print(cmd)
