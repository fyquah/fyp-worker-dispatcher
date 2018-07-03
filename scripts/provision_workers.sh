#!/bin/bash

# Setup the rundirs for workers

WORKER_HOSTNAMES="192.168.0.10 192.168.0.11 192.168.0.12"

for hostname in $WORKER_HOSTNAMES; do
  ssh "$hostname" mkdir -p worker-rundir/0
done
