#!/bin/bash

RUNDIR=../dev/controller-rundir

jbuilder exec -- controller \
  -config dev_config.sexp \
  -rundir "$RUNDIR" \
  -exp-dir ../experiments/async/cohttp \
  -bin-name cohttp_async_bench \
  -args "10000" \
  1>"$RUNDIR/stdout.log"
