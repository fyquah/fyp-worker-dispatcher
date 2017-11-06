#!/bin/bash

RUNDIR=../dev/controller-rundir

jbuilder exec -- controller \
  -config deploy_config.sexp \
  -rundir "$RUNDIR" \
  -exp-dir ../experiments/async/cohttp \
  -bin-name cohttp_async_bench \
  -bin-args "10000" \
  1>"$RUNDIR/stdout.log"
