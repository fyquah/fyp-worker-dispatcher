#!/bin/bash

# sudo yum install gmp gmp-devel

set -euo pipefail

export OPAMROOT="$HOME/fyp/opam-root"
eval `opam config env`
echo `which ocamlopt`

echo "Installing benchmark dependencies"
export OCAMLPARAM="_,O3=1"
opam install -y depext jbuilder conf-gmp zarith easy-format biniou async lwt cohttp-lwt cohttp-lwt-unix psmt2-frontend camlzip ocplib-simplex react async uuidm ocplib-endian camlimages

# These are the main packages we are trying to benchmark...
# export OCAMLPARAM="_,O3=1,inline-max-unroll=10,plugin=$PREFIX.cmxs,plugin=$PREFIX.cmo"
# echo "Installing main thing"
# export OCAMLPARAM="_,O3=1,inline-max-unroll=10,plugin=$PREFIX.cmxs,plugin=$PREFIX.cmo"
# opam install -y alt-ergo -j 1
