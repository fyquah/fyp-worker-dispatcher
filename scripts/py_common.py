import collections

ExperimentParameters = collections.namedtuple(
        "ExperimentParameters", ["bin_name", "subdir", "bin_args"])

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

EXPERIMENT_TO_PARAMETERS = {

        "almabench": ExperimentParameters(
            bin_name="almabench",
            subdir="normal/almabench",
            bin_args="23"),

        "bdd": ExperimentParameters(
            bin_name="bdd",
            subdir="normal/bdd_benchmark",
            bin_args="28"),

        "fft": ExperimentParameters(
            bin_name="fft",
            subdir="normal/fft",
            bin_args="24"),

        "floats-in-functor": ExperimentParameters(
            bin_name="b",
            subdir="normal/floats-in-functor",
            bin_args="500000 3.0"),

        "hamming": ExperimentParameters(
            bin_name="hamming",
            subdir="normal/hamming",
            bin_args="20000000"),

        "kahan-sum": ExperimentParameters(
            bin_name="kahan_sum",
            subdir="normal/kahan_sum",
            bin_args="300000000"),

        "kb": ExperimentParameters(
            bin_name="kb",
            subdir="normal/kb_benchmark",
            bin_args="400"),

        "lens": ExperimentParameters(
            bin_name="lens_benchmark",
            subdir="normal/lens_benchmark",
            bin_args="1500000000"),

        "lexifi": ExperimentParameters(
            bin_name="main",
            subdir="normal/lexifi-g2pp_benchmark",
            bin_args=""),

        "quicksort": ExperimentParameters(
            bin_name="quicksort",
            subdir="normal/quicksort",
            bin_args="1500"),

        "sequence": ExperimentParameters(
            bin_name="sequence_benchmark",
            subdir="normal/sequence_benchmark",
            bin_args="45000"),

        "sequence-cps": ExperimentParameters(
            bin_name="sequence_cps_benchmark",
            subdir="normal/sequence_cps_benchmark",
            bin_args="120000"),

        "fyq-stdlib-int-sets": ExperimentParameters(
            bin_name="main",
            subdir="fyq/stdlib-int-sets",
            bin_args=""),

        "fyq-stdlib-functor-record-sets": ExperimentParameters(
            bin_name="main",
            subdir="fyq/stdlib-functor-record-sets",
            bin_args=""),

        "fyq-rev-list": ExperimentParameters(
            bin_name="main",
            subdir="fyq/rev-list",
            bin_args="400000000"),

        "fyq-symbolic-maths": ExperimentParameters(
            bin_name="main",
            subdir="fyq/symbolic-maths",
            bin_args="44"),

}
