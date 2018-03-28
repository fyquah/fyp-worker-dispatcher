import argparse
import concurrent.futures
import sys
from learn_linear_general_reward import HyperParameters

parser = argparse.ArgumentParser(description=
        "batch executor. Executes the [run] method of the module given in "
        "the args' flag.")
parser.add_argument("module", type=str, help="module")
parser.add_argument("--dry-run", action="store_true", help="dry run (for testing)")


def main():
    args = parser.parse_args()
    tasks = []
    for line in sys.stdin:
        tasks.append([x for x in line.strip().split(" ") if x])
    module = __import__(args.module)

    if args.dry_run:
        print "[DRY_RUN] Executing [run] function from %s with the following args" % module
        for task in tasks:
            print "   ", module.parser.parse_args(task)
    else:
        for task in tasks:
            module.run(task)


if __name__ == "__main__":
    main()
