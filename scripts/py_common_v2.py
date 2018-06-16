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

        "levinson-durbin": ExperimentParameters(
                bin_name="bench",
                subdir="micro-evaluation/numerical/levinson-durbin",
                bin_args="20000",
                module_paths=["bench"],
        ),

        "lu-decomposition": ExperimentParameters(
                bin_name="bench",
                subdir="micro-evaluation/numerical/lu-decomposition",
                bin_args="700",
                module_paths=["bench"],
        ),

        "naive-multilayer": ExperimentParameters(
                bin_name="bench",
                subdir="micro-evaluation/numerical/naive-multilayer",
                bin_args="700",
                module_paths=["bench"],
        ),

        "qr-decomposition": ExperimentParameters(
                bin_name="bench",
                subdir="micro-evaluation/numerical/qr-decomposition",
                bin_args="",
                module_paths=["bench"],
        ),

        "chameneos-redux-async": ExperimentParameters(
                bin_name="bench",
                subdir="micro-evaluation/chameneos-redux-async",
                bin_args="3000000",
                module_paths=["bench"],
        ),

        "chameneos-redux-evtchn": ExperimentParameters(
                bin_name="bench",
                subdir="micro-evaluation/chameneos-redux-evtchn",
                bin_args="150000",
                module_paths=["bench"],
        ),

        "chameneos-redux-lwt": ExperimentParameters(
                bin_name="bench",
                subdir="micro-evaluation/chameneos-redux-lwt",
                bin_args="3000000",
                module_paths=["bench"],
        ),

        "chameneos-redux-th": ExperimentParameters(
                bin_name="bench",
                subdir="micro-evaluation/chameneos-redux-th",
                bin_args="450000",
                module_paths=["bench"],
        ),

        "valet-async": ExperimentParameters(
                bin_name="bench",
                subdir="micro-evaluation/valet-async",
                bin_args="147",
                module_paths=["valet_core", "valet_react", "test_lib", "bench"],
        ),

        "valet-lwt": ExperimentParameters(
                bin_name="bench",
                subdir="micro-evaluation/valet-lwt",
                bin_args="135",
                module_paths=["valet_core", "valet_react", "test_lib", "bench"],
        ),

        "sauvola": ExperimentParameters(
                bin_name="bench",
                subdir="micro-evaluation/sauvola",
                bin_args="./example2_small.jpg /tmp/bla",
                module_paths=["bench"],
        ),
}

INITIAL_EXPERIMENTS = [
        "almabench",
        "bdd",
        "fft",

        "floats-in-functor",
        "hamming",
        "kahan-sum",

        "kb",
        "lens",
        "lexifi",

        "quicksort",
        "sequence",
        "sequence-cps",
        ]
