#!/bin/bash

if [ "$1" = "" ]; then
  echo "Must provide algorithm"
  exit 1
fi

RUNDIR=../dev/controller-rundir

jbuilder exec -- controller "$1" \
  -config dev_config.sexp \
  -rundir "$RUNDIR" \
  -exp-dir ../experiments/async/cohttp \
  -bin-name cohttp_async_bench \
  -args "10000"
