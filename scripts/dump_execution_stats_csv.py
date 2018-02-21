import os
import sys

import py_common

bash_commands = []

CMD = "jbuilder exec -- controller simulated-annealing plot v1-execution-stats %s >> %s"


def extract_exp_name(script):
    prefixes = ["simulated-annealing-", "mcts-"]
    for prefix in prefixes:
        if script.startswith(prefix):
            return script[len(prefix):].replace("_", "-")
    raise RuntimeError("Do not recognise experiment %s" % script)

files_to_clean = []

for line in sys.stdin:
    script_name, exp_dir = line.strip().split(",")
    script_name = os.path.basename(script_name)
    output = "pca-data/" + extract_exp_name(script_name) + ".csv"
    files_to_clean.append(output)
    bash_commands.append((CMD % (exp_dir, output)))

for filename in set(files_to_clean):
    bash_commands.append(
            "python3 ./scripts/remove_duplicates_from_data.py %s" % filename
    )

for cmd in bash_commands:
    print(cmd)
