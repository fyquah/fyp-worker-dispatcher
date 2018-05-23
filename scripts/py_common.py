import collections

ExperimentParameters = collections.namedtuple(
        "ExperimentParameters",
        ["bin_name", "subdir", "bin_args", "module_paths"])

SCRIPT_TO_BIN_NAME = {
        "simulated-annealing-almabench":              "almabench",
        "simulated-annealing-bdd":                    "bdd",
        "simulated-annealing-fft":                    "fft",
        "simulated-annealing-floats-in-functor":      "b",
        "simulated-annealing-hamming":                "hamming",
        "simulated-annealing-kahan-sum":              "kahan_sum",
        "simulated-annealing-kb":                     "kb",
        "simulated-annealing-lens-benchmark":         "lens_benchmark",
        "simulated-annealing-lexifi-g2pp_benchmark":  "main",
        "simulated-annealing-quicksort":              "quicksort",
        "simulated-annealing-sequence_benchmark":     "sequence_benchmark",
        "simulated-annealing-sequence_cps_benchmark": "sequence_cps_benchmark",

        "mcts-almabench": "almabench",
        "mcts-float_in_functor": "b",
        "mcts-float-in-functor": "b",
        "mcts-floats-in-functor": "b",
        "mcts-sequence_benchmark": "sequence_benchmark",
}


SCRIPT_SUFFIX_TO_EXP_NAME = {
        "almabench": "almabench",
        "bdd":                    "bdd",
        "fft":                    "fft",
        "float_in_functor":       "floats-in-functor",
        "float-in-functor":       "floats-in-functor",
        "floats-in-functor":      "floats-in-functor",
        "hamming":                "hamming",
        "kahan-sum":              "kahan-sum",
        "kb":                     "kb",
        "lens-benchmark":         "lens",
        "lexifi-g2pp_benchmark":  "lexifi",
        "quicksort":              "quicksort",
        "sequence_benchmark":     "sequence",
        "sequence_cps_benchmark": "sequence-cps",
}

EXPERIMENT_TO_PARAMETERS = {

        "almabench": ExperimentParameters(
            bin_name="almabench",
            subdir="normal/almabench",
            bin_args="23",
            module_paths=["almabench"]
        ),

        "bdd": ExperimentParameters(
            bin_name="bdd",
            subdir="normal/bdd_benchmark",
            bin_args="28",
            module_paths=["bdd"]
        ),

        "fft": ExperimentParameters(
            bin_name="fft",
            subdir="normal/fft",
            bin_args="24",
            module_paths=["fft"]
        ),

        "floats-in-functor": ExperimentParameters(
            bin_name="b",
            subdir="normal/floats-in-functor",
            bin_args="500000 3.0",
            module_paths=["a", "b"]
        ),

        "hamming": ExperimentParameters(
            bin_name="hamming",
            subdir="normal/hamming",
            bin_args="20000000",
            module_paths=["hamming"]
        ),

        "kahan-sum": ExperimentParameters(
            bin_name="kahan_sum",
            subdir="normal/kahan_sum",
            bin_args="300000000",
            module_paths=["kahan_sum", "main"]
        ),

        "kb": ExperimentParameters(
            bin_name="kb",
            subdir="normal/kb_benchmark",
            bin_args="400",
            module_paths=["kb"]
        ),

        "lens": ExperimentParameters(
            bin_name="lens_benchmark",
            subdir="normal/lens_benchmark",
            bin_args="1500000000",
            module_paths=["lens", "lens_benchmark"]
        ),

        "lexifi": ExperimentParameters(
            bin_name="main",
            subdir="normal/lexifi-g2pp_benchmark",
            bin_args="",
            module_paths=["date", "math", "optimization", "g2pp_calibration", "main"]
        ),

        "quicksort": ExperimentParameters(
            bin_name="quicksort",
            subdir="normal/quicksort",
            bin_args="1500",
            module_paths=["quicksort"]
        ),

        "sequence": ExperimentParameters(
            bin_name="sequence_benchmark",
            subdir="normal/sequence_benchmark",
            bin_args="45000",
            module_paths=["sequence", "sequence_benchmark"]
        ),

        "sequence-cps": ExperimentParameters(
            bin_name="sequence_cps_benchmark",
            subdir="normal/sequence_cps_benchmark",
            bin_args="120000",
            module_paths=["sequence_cps", "sequence_cps_benchmark"]
        ),

        "fyq-stdlib-int-sets": ExperimentParameters(
            bin_name="main",
            subdir="fyq/stdlib-int-sets",
            bin_args="",
            module_paths=["main"]
        ),

        "fyq-stdlib-functor-record-sets": ExperimentParameters(
            bin_name="main",
            subdir="fyq/stdlib-functor-record-sets",
            bin_args="",
            module_paths=["main"]
        ),

        "fyq-rev-list": ExperimentParameters(
            bin_name="main",
            subdir="fyq/rev-list",
            bin_args="400000000",
            module_paths=["main"]
        ),

        "fyq-symbolic-maths": ExperimentParameters(
            bin_name="main",
            subdir="fyq/symbolic-maths",
            bin_args="44",
            module_paths=["main"]
        ),
}
