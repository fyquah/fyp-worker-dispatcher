import argparse

import gen_model
import gen_weights


def module_name_to_filename(a):
    return a[0].lower() + a[1:] + ".ml"


parser = argparse.ArgumentParser(description="codegen")
parser.add_argument("--graph", type=str, help="pb file for TF graph", required=True)
parser.add_argument("--model-module", type=str, help="module name (starts with caps) of the generated model ML file", required=True)
parser.add_argument("--weights-module", type=str, help="module name (starts with caps) of weights module to use", required=True)
parser.add_argument("--checkpoint", type=str, help="checkpoint file for TF weights", required=True)

args = parser.parse_args()

with open(module_name_to_filename(args.model_module), "w") as f:
    f.write(gen_model.run(graph=args.graph, weights_module=args.weights_module))

with open(module_name_to_filename(args.weights_module), "w") as f:
    f.write(gen_weights.run(args.checkpoint))
