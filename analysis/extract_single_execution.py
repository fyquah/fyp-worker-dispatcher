import argparse
import sys

import inlining_tree


parser = argparse.ArgumentParser(description="formulate the problem")
parser.add_argument("--rundir", type=str, required=True)
parser.add_argument("--bin-name", type=str, help="output dir", required=True)
parser.add_argument("--exp-subdir", type=str,
        help="experiment subdirectory name", required=True)
parser.add_argument("--path-patching", action="store_true")


def main():
    args = parser.parse_args()
    if args.path_patching:
        preprocessing = ("path_patching", args.exp_subdir)
    else:
        preprocessing = None

    tree = inlining_tree.load_tree_from_rundir(
            args.rundir, args.bin_name, preprocessing)
    print("Done!")

if __name__ == "__main__":
    main()
