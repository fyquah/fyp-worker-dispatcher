import argparse
import logging
import os
import shutil
import subprocess
import tempfile

import concurrent.futures
import sexpdata

import inlining_tree
import extract_data_from_experiments
import py_common

parser = argparse.ArgumentParser(description="formulate the problem")
parser.add_argument("--output-dir", type=str, help="output dir", required=True)
parser.add_argument("--experiment-name", type=str, required=True, help="")
parser.add_argument("--debug", action="store_true", help="debugging mode")


def load_tree_from_from_decision_file(data_collector_file):
    tree_file = tempfile.NamedTemporaryFile(delete=True)
    try:
        proc = subprocess.Popen([
            "../_build/default/tools/tree_tools.exe", "v1",
            "decisions-to-tree", data_collector_file,
            "-expand", "-output", tree_file.name])

        proc.wait()
        if proc.returncode != 0:
            raise RuntimeError(
                "decisions-to-tree failed for arguments %s -expand"
                % data_collector_file)

        tree = inlining_tree.build_tree_from_str(tree_file.read())
        shutil.copyfile(
                tree_file.name,
                os.path.join(
                    os.path.dirname(data_collector_file), "expanded-tree.sexp"))
    finally:
        tree_file.close()

    if tree is None:
        logging.info("Dropping %s because cannot parse sexp correctly" % data_collector_file)
        return None

    logging.info("Done with %s" % data_collector_file)
    return tree


def load_execution_time(execution_stats_file):
    with open(execution_stats_file) as f:
        execution_stats_sexp = sexpdata.load(f)
        m = inlining_tree.sexp_to_map(execution_stats_sexp)
        execution_time = inlining_tree.geometric_mean([
                inlining_tree.parse_time(inlining_tree.unpack_atom(x))
                for x in m["raw_execution_time"]
        ])
        return execution_time


def last_few_components(fname, n):
    if n == 0:
        return ""
    p = last_few_components(os.path.dirname(fname), n - 1)
    if p != "":
        return p + "/" + os.path.basename(fname)
    else:
        return os.path.basename(fname)


def infer_execution_directory(dirname):
    # [Dirname] has the form of
    #    bla/bla/almabench/2017-01-01-19-24-33/opt_data/step/substep/
    relpath = last_few_components(dirname, 4)

    candidates = [
            os.path.join("/media/usb/home/fyquah/fyp/prod/rundir/", relpath),
            os.path.join("/media/usb2/home/fyquah/fyp/prod/rundir/", relpath),
    ]
    for candidate in candidates:
        if os.path.exists(candidate):
            return candidate

    raise RuntimeError(
            "Cannot infer execution_directory for %s. Tried looking at %s"
            % (dirname, " , ".join(candidates)))

def choose_first_that_exists(candidates):
    for candidate in candidates:
        if os.path.exists(candidate):
            return candidate
    raise RuntimeError("None of %s exists!" % (",".join(candidates)))

def main():
    logging.getLogger().setLevel(logging.INFO)
    args = parser.parse_args()
    bin_name = py_common.EXPERIMENT_TO_PARAMETERS[args.experiment_name].bin_name
    exp_subdir = py_common.EXPERIMENT_TO_PARAMETERS[args.experiment_name].subdir
    tasks = []

    processed_data_dir = os.path.join(
            "/media/usb/home/fyquah/fyp/prod/processed-data",
            args.experiment_name)

    logging.info("bin_name = %s" % bin_name)
    logging.info("exp_subdir = %s" % exp_subdir)
    logging.info("Looking for processed data in %s" % processed_data_dir)

    proc = subprocess.Popen([
            "find", processed_data_dir, "-name", "decisions.sexp"],
            stdout=subprocess.PIPE)
    decision_files = []
    for line in proc.stdout:
        decision_files.append(line.strip())
        if args.debug and len(decision_files) > 100:
            proc.terminate()
            break

    logging.info("Found %d decision files" % len(decision_files))
    results = []

    def process_single_decision_file(decision_file):
        tree = load_tree_from_from_decision_file(decision_file)
        if tree is None:
            return None
        decision_file_dirname = os.path.dirname(decision_file)
        execution_directory = infer_execution_directory(
                os.path.dirname(decision_file))
        execution_stats_file = choose_first_that_exists([
            os.path.join(decision_file_dirname, "execution_stats.sexp"),
            os.path.join(execution_directory, "execution_stats.sexp"),
        ])
        execution_time = load_execution_time(execution_stats_file)
        return (execution_directory, tree, execution_time)

    # Multiple threads have pretty much zero improvement here.
    for decision_file in decision_files:
        output = process_single_decision_file(decision_file)
        if output is not None:
            results.append(output)

    execution_directories, trees, execution_times = zip(*results)

    problem = extract_data_from_experiments.formulate_problem(
            trees, execution_times, execution_directories)
    if not os.path.exists(args.output_dir):
        os.mkdir(args.output_dir)
    problem.dump(args.output_dir)


if __name__  == "__main__":
    main()
