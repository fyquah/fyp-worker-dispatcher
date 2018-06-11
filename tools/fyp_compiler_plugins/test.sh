#!/bin/bash

set -euo pipefail

~/fyp/opam-root/4.05.0+fyp/bin/ocamlopt.opt -plugin $1 main.ml
