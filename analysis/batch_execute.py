import argparse
import concurrent.futures
import sys

parser = argparse.ArgumentParser(description=
        "batch executor. Executes the [run] method of the module given in "
        "the args' flag.")
parser.add_argument("module", type=str, help="module")
parser.add_argument("--num-threads", type=int, help="num threads", default=2)
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
        pool = concurrent.futures.ThreadPoolExecutor(args.num_threads)
        futures = []
        for task in tasks:
            futures.append(pool.submit(module.run, task))
        for r in concurrent.futures.as_completed(futures):
            r.result()


if __name__ == "__main__":
    main()
