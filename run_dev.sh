#!/bin/bash

RUNDIR=../dev/controller-rundir

jbuilder exec -- controller \
  -config dev_config.sexp \
  -rundir "$RUNDIR" \
  1>"$RUNDIR/stdout.log"
