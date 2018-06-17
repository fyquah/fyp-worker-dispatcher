import py_common
import argparse

parser = argparse.ArgumentParser(description="query")
parser.add_argument("--name", type=str, help="name")
group1 = parser.add_mutually_exclusive_group(required=True)
group1.add_argument("--bin-name", action="store_true")
group1.add_argument("--bin-files", action="store_true")
group1.add_argument("--subdir", action="store_true")
group1.add_argument("--bin-args", action="store_true")
group1.add_argument("--all", action="store_true")
group1.add_argument("--all-old", action="store_true")
group1.add_argument("--methods", action="store_true")
group1.add_argument("--module-paths", action="store_true")

def main():
    args = parser.parse_args()
    name = args.name

    if args.all:
        print " ".join(py_common.EXPERIMENT_TO_PARAMETERS.keys())
        return

    if args.all_old:
        print " ".join(py_common.INITIAL_EXPERIMENTS)
        return

    d = py_common.EXPERIMENT_TO_PARAMETERS[name]

    if args.bin_name:
        subkey = "bin_name"
    elif args.bin_files:
        print ",".join(d.bin_files)
        return
    elif args.subdir:
        subkey = "subdir"
    elif args.bin_args:
        subkey = "bin_args"
    elif args.module_paths:
        print ",".join(d.module_paths)
        return
    else:
        assert False

    print getattr(d, subkey),


if __name__ == "__main__":
    main()
